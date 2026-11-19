.pre_merge_equal <- function(x, y) {
  if (is.factor(x)) x <- as.character(x)
  if (is.factor(y)) y <- as.character(y)
  isTRUE(all.equal(x, y, check.attributes = FALSE))
}

.pre_merge_bind_rows <- function(tables) {
  columns <- unique(unlist(lapply(tables, names), use.names = FALSE))
  prototypes <- lapply(columns, function(nm) {
    tables[[which(vapply(tables, function(z) nm %in% names(z), logical(1L)))[[1L]]]][[nm]]
  })
  names(prototypes) <- columns
  tables <- lapply(tables, function(z) {
    for (nm in setdiff(columns, names(z))) {
      z[[nm]] <- prototypes[[nm]][rep(NA_integer_, nrow(z))]
    }
    for (nm in names(z)) if (is.factor(z[[nm]])) z[[nm]] <- as.character(z[[nm]])
    z[, columns, drop = FALSE]
  })
  out <- do.call(rbind, tables)
  rownames(out) <- NULL
  out
}

# Preserve pre-existing provenance columns, including when merging a merged object.
.pre_merge_annotate <- function(df, annotations) {
  for (nm in names(annotations)) {
    if (nm %in% names(df)) {
      replacement <- tail(make.unique(c(names(df), paste0(nm, ".input"))), 1L)
      names(df)[names(df) == nm] <- replacement
    }
    df[[nm]] <- annotations[[nm]]
  }
  df
}

.pre_merge_check_descriptor <- function(reference, current, id, label, experiment,
                                        ignore = character()) {
  columns <- setdiff(names(reference), ignore)
  if (!setequal(columns, setdiff(names(current), ignore))) {
    .pre_abort("Experiment '%s': %s metadata columns differ from the first experiment.", experiment, label)
  }
  for (nm in setdiff(columns, id)) {
    if (!.pre_merge_equal(reference[[nm]], current[[nm]])) {
      .pre_abort("Experiment '%s': incompatible %s metadata in column '%s'.", experiment, label, nm)
    }
  }
}

.pre_merge_acquisition <- function(x, time_columns) {
  acq <- x@acquisitionData
  if (!length(time_columns)) return(acq)

  # A time descriptor can vary between runs (e.g. observed timing or filenames).
  # Store it per sample/channel/time instead of borrowing the first run's values.
  grid <- expand.grid(
    sample_id = x@sampleData$sample_id,
    channel_id = x@channelData$channel_id,
    time_id = x@timeData$time_id,
    KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE
  )
  key <- function(z) paste(z$sample_id, z$channel_id, z$time_id, sep = "\r")
  idx <- match(key(grid), key(acq))
  for (nm in setdiff(names(acq), names(grid))) grid[[nm]] <- acq[[nm]][idx]
  ti <- match(grid$time_id, x@timeData$time_id)
  for (nm in time_columns) {
    # Existing acquisition fields are more specific than time descriptors.
    if (nm %in% names(x@timeData)) {
      if (!(nm %in% names(grid))) {
        grid[[nm]] <- x@timeData[[nm]][ti]
      } else {
        absent_acquisition <- is.na(idx)
        grid[[nm]][absent_acquisition] <- x@timeData[[nm]][ti[absent_acquisition]]
      }
    }
  }
  grid
}

#' Merge structurally compatible experiments
#'
#' Appends samples/wells from independent experiments without averaging values.
#' Channels, logical time points and within-well positions are matched by ID.
#' @param ... Two or more PlateReaderExperiment objects, or a single list of them.
#' @param experiment_ids Optional unique, non-empty character labels, one per
#'   experiment. Defaults to input names, with experiment1, experiment2, etc.
#'   for unnamed inputs.
#' @param axis Merge axis: `"auto"` selects time when layout and design match
#'   and recorded times are distinct, otherwise samples. Use `"sample"` to
#'   force independent plates or `"time"` to require consecutive reads.
#' @return A PlateReaderExperiment with combined samples and design annotations.
#' @export
merge_experiments <- function(..., experiment_ids = NULL, axis = c("auto", "sample", "time")) {
  experiments <- list(...)
  if (length(experiments) == 1L && is.list(experiments[[1L]]) &&
      !is(experiments[[1L]], "PlateReaderExperiment")) {
    experiments <- experiments[[1L]]
  }
  if (length(experiments) < 2L) .pre_abort("Supply at least two experiments to merge.")
  for (i in seq_along(experiments)) {
    if (!is(experiments[[i]], "PlateReaderExperiment")) {
      .pre_abort("Input %d must be a PlateReaderExperiment.", i)
    }
    validObject(experiments[[i]])
  }
  if (is.null(experiment_ids)) {
    experiment_ids <- names(experiments)
    if (is.null(experiment_ids)) experiment_ids <- rep("", length(experiments))
    unnamed <- is.na(experiment_ids) | !nzchar(trimws(experiment_ids))
    experiment_ids[unnamed] <- paste0("experiment", which(unnamed))
  }
  if (!is.character(experiment_ids) || length(experiment_ids) != length(experiments) ||
      anyNA(experiment_ids) || any(!nzchar(trimws(experiment_ids))) || anyDuplicated(experiment_ids)) {
    .pre_abort("experiment_ids must contain one unique, non-empty character label per experiment.")
  }
  names(experiment_ids) <- NULL

  axis <- match.arg(axis)
  can_merge_time <- .pre_can_merge_times(experiments)
  if (axis == "time" && !can_merge_time)
    .pre_abort("Time-axis merge requires an identical plate layout/design and distinct recorded time values.")
  if (axis == "time" || (axis == "auto" && can_merge_time))
    return(.pre_merge_on_time(experiments, experiment_ids))

  reference <- experiments[[1L]]
  dimensions <- c("sample", "channel", "time", "position")
  for (i in seq_along(experiments)) {
    x <- experiments[[i]]
    indices <- vector("list", 3L)
    for (axis in 2:4) {
      target <- dimnames(reference)[[axis]]
      current <- dimnames(x)[[axis]]
      if (length(target) != length(current) ||
          (axis != 4L && !setequal(target, current))) {
        .pre_abort("Experiment '%s': %s identifiers/count differ from the first experiment.",
                   experiment_ids[[i]], dimensions[[axis]])
      }
      # Read positions with different labels can still correspond by ordinal
      # position, provided their physical descriptors match below.
      indices[[axis - 1L]] <- if (axis == 4L && !setequal(target, current)) {
        seq_along(current)
      } else match(target, current)
    }
    # Direct indexing also supports empty sample dimensions.
    x@data <- x@data[, indices[[1L]], indices[[2L]], indices[[3L]], drop = FALSE]
    x@channelData <- x@channelData[indices[[1L]], , drop = FALSE]
    x@timeData <- x@timeData[indices[[2L]], , drop = FALSE]
    x@positionData <- x@positionData[indices[[3L]], , drop = FALSE]
    .pre_merge_check_descriptor(reference@channelData, x@channelData,
                                "channel_id", "channel", experiment_ids[[i]])
    .pre_merge_check_descriptor(reference@positionData, x@positionData,
                                "position_id", "position", experiment_ids[[i]], "position_index")
    for (nm in c("cycle", "nominal_time_s")) {
      if ((nm %in% names(reference@timeData)) != (nm %in% names(x@timeData)) ||
          !.pre_merge_equal(reference@timeData[[nm]], x@timeData[[nm]])) {
        .pre_abort("Experiment '%s': incompatible logical time metadata in column '%s'.",
                   experiment_ids[[i]], nm)
      }
    }
    experiments[[i]] <- x
  }

  time_columns <- unique(unlist(lapply(experiments, function(x) names(x@timeData)), use.names = FALSE))
  varying_time <- Filter(function(nm) {
    !all(vapply(experiments, function(x) {
      nm %in% names(x@timeData) && .pre_merge_equal(reference@timeData[[nm]], x@timeData[[nm]])
    }, logical(1L)))
  }, setdiff(time_columns, c("time_id", "time_index")))
  shared_time <- reference@timeData[, setdiff(names(reference@timeData), varying_time), drop = FALSE]

  prefix <- function(label, ids) if (length(ids)) paste0(label, "::", ids) else character()
  sample_ids <- make.unique(unlist(lapply(seq_along(experiments), function(i) {
    prefix(experiment_ids[[i]], experiments[[i]]@sampleData$sample_id)
  }), use.names = FALSE))
  plate_maps <- lapply(seq_along(experiments), function(i) {
    ids <- unique(as.character(experiments[[i]]@sampleData$plate_id))
    ids <- ids[!is.na(ids)]
    data.frame(source_plate_id = ids, plate_id = prefix(experiment_ids[[i]], ids), stringsAsFactors = FALSE)
  })
  plate_ids <- make.unique(unlist(lapply(plate_maps, function(z) z$plate_id), use.names = FALSE))

  d <- dim(reference)
  out <- array(NA_real_, dim = c(length(sample_ids), d[2:4]),
               dimnames = c(list(sample = sample_ids), dimnames(reference)[2:4]))
  samples <- designs <- acquisitions <- sources <- vector("list", length(experiments))
  offset <- plate_offset <- 0L
  for (i in seq_along(experiments)) {
    x <- experiments[[i]]
    rows <- offset + seq_len(dim(x)[[1L]])
    ids <- sample_ids[rows]
    out[rows, , , ] <- x@data
    annotation <- list(experiment_id = rep(experiment_ids[[i]], length(ids)),
                       source_sample_id = as.character(x@sampleData$sample_id))
    sd <- .pre_merge_annotate(x@sampleData, annotation)
    dd <- .pre_merge_annotate(x@design, annotation)
    sd$sample_id <- dd$sample_id <- ids
    pm <- plate_maps[[i]]
    pm$plate_id <- plate_ids[plate_offset + seq_len(nrow(pm))]
    if ("plate_id" %in% names(sd)) {
      sd <- .pre_merge_annotate(sd, list(source_plate_id = as.character(sd$plate_id)))
      sd$plate_id <- pm$plate_id[match(sd$source_plate_id, pm$source_plate_id)]
    }
    acq <- .pre_merge_acquisition(x, varying_time)
    acq$sample_id <- ids[match(acq$sample_id, x@sampleData$sample_id)]
    acq <- .pre_merge_annotate(acq, list(experiment_id = rep(experiment_ids[[i]], nrow(acq))))
    samples[[i]] <- sd
    designs[[i]] <- dd
    acquisitions[[i]] <- acq
    sources[[i]] <- list(
      metadata = x@metadata, channelData = x@channelData,
      timeData = x@timeData, positionData = x@positionData,
      position_id_map = data.frame(source_position_id = x@positionData$position_id,
                                   position_id = reference@positionData$position_id),
      sample_id_map = data.frame(source_sample_id = x@sampleData$sample_id, sample_id = ids),
      plate_id_map = pm
    )
    offset <- offset + length(ids)
    plate_offset <- plate_offset + nrow(pm)
  }
  names(sources) <- experiment_ids
  merged <- PlateReaderExperiment(
    data = out, sampleData = .pre_merge_bind_rows(samples),
    channelData = reference@channelData, timeData = shared_time,
    positionData = reference@positionData, design = .pre_merge_bind_rows(designs),
    acquisitionData = .pre_merge_bind_rows(acquisitions),
    metadata = list(
      source = "Merged PlateReaderExperiment objects", source_format = "merged_experiments",
      source_files = unique(unlist(lapply(experiments, function(x) x@metadata$source_files), use.names = FALSE)),
      experiments = sources, processing_history = list()
    )
  )
  .pre_history(merged, "merge_experiments", list(experiment_ids = experiment_ids))
}
