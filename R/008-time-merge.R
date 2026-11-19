.pre_time_stamps <- function(x) {
  a <- x@acquisitionData
  if (!"timestamp" %in% names(a) || !inherits(a$timestamp, "POSIXt")) return(NULL)
  v <- vapply(x@timeData$time_id, function(id) {
    t <- as.numeric(a$timestamp[a$time_id == id])
    if (!length(t) || all(is.na(t))) NA_real_ else min(t, na.rm = TRUE)
  }, numeric(1L))
  if (anyNA(v)) NULL else v
}

#' Align logical observed times to acquisition timestamps
#' @param x A PlateReaderExperiment with acquisitionData$timestamp.
#' @param origin Optional POSIXct origin; defaults to the earliest timestamp.
#' @return The experiment with timeData$observed_time_s measured from origin,
#'   and timeData$absolute_time recording the corresponding clock time.
#' @export
offset_time_to_real <- function(x, origin = NULL) {
  if (!is(x, "PlateReaderExperiment")) .pre_abort("x must be a PlateReaderExperiment.")
  stamps <- .pre_time_stamps(x)
  if (is.null(stamps)) .pre_abort("Each logical time point needs an acquisitionData$timestamp to align real time.")
  tz <- attr(x@acquisitionData$timestamp, "tzone") %||% "UTC"
  if (is.null(origin)) origin <- as.POSIXct(min(stamps), origin = "1970-01-01", tz = tz)
  if (!inherits(origin, "POSIXt") || length(origin) != 1L || is.na(origin))
    .pre_abort("origin must be one non-missing POSIXct value.")
  x@timeData$observed_time_s <- stamps - as.numeric(origin)
  x@timeData$absolute_time <- as.POSIXct(stamps, origin = "1970-01-01", tz = tz)
  if ("actual_time_s" %in% names(x@acquisitionData)) {
    x@acquisitionData$actual_time_s <- as.numeric(x@acquisitionData$timestamp) - as.numeric(origin)
  }
  x@metadata$time_origin <- origin
  x <- .pre_history(x, "offset_time_to_real", list(origin = origin))
  validObject(x)
  x
}

.pre_can_merge_times <- function(experiments) {
  ref <- experiments[[1L]]
  for (x in experiments[-1L]) {
    if (!setequal(ref@sampleData$sample_id, x@sampleData$sample_id)) return(FALSE)
    si <- match(ref@sampleData$sample_id, x@sampleData$sample_id)
    for (slot in c("sampleData", "design")) {
      a <- slot(ref, slot); b <- slot(x, slot)[si, , drop = FALSE]
      columns <- setdiff(names(a), "plate_id")
      if (!setequal(columns, setdiff(names(b), "plate_id"))) return(FALSE)
      for (nm in columns) if (!.pre_merge_equal(a[[nm]], b[[nm]])) return(FALSE)
    }
  }
  stamps <- lapply(experiments, .pre_time_stamps)
  if (all(vapply(stamps, Negate(is.null), logical(1L)))) {
    values <- unlist(stamps, use.names = FALSE)
  } else {
    values <- unlist(lapply(experiments, function(x) x@timeData$observed_time_s), use.names = FALSE)
  }
  length(values) == sum(vapply(experiments, function(x) nrow(x@timeData), integer(1L))) &&
    all(is.finite(values)) && !anyDuplicated(values)
}

.pre_merge_on_time <- function(experiments, ids) {
  ref <- experiments[[1L]]
  stamps <- lapply(experiments, .pre_time_stamps)
  if (all(vapply(stamps, Negate(is.null), logical(1L)))) {
    origin <- as.POSIXct(min(unlist(stamps)), origin = "1970-01-01", tz = "UTC")
    experiments <- lapply(experiments, offset_time_to_real, origin = origin)
  }
  ntime <- sum(vapply(experiments, function(x) dim(x)[[3L]], integer(1L)))
  newids <- sprintf("T%03d", seq_len(ntime))
  d <- dim(ref)
  out <- array(NA_real_, dim = c(d[1:2], ntime, d[4L]),
               dimnames = list(sample = dimnames(ref)[[1L]], channel = dimnames(ref)[[2L]],
                               time = newids, position = dimnames(ref)[[4L]]))
  times <- acquisitions <- sources <- vector("list", length(experiments))
  pos <- 0L
  for (i in seq_along(experiments)) {
    x <- experiments[[i]]
    si <- match(dimnames(ref)[[1L]], dimnames(x)[[1L]])
    ci <- match(dimnames(ref)[[2L]], dimnames(x)[[2L]])
    pi <- if (setequal(dimnames(ref)[[4L]], dimnames(x)[[4L]]))
      match(dimnames(ref)[[4L]], dimnames(x)[[4L]]) else seq_len(d[[4L]])
    if (anyNA(ci) || length(ci) != d[[2L]] || length(pi) != d[[4L]])
      .pre_abort("Experiment '%s': incompatible channel or read positions.", ids[[i]])
    .pre_merge_check_descriptor(ref@channelData, x@channelData[ci, , drop = FALSE], "channel_id", "channel", ids[[i]])
    .pre_merge_check_descriptor(ref@positionData, x@positionData[pi, , drop = FALSE], "position_id", "position", ids[[i]], "position_index")
    n <- dim(x)[[3L]]
    ix <- pos + seq_len(n)
    out[, , ix, ] <- x@data[si, ci, , pi, drop = FALSE]
    td <- x@timeData
    original_ids <- td$time_id
    td$time_id <- newids[ix]
    td$time_index <- ix
    td$experiment_id <- ids[[i]]
    td$source_time_id <- original_ids
    times[[i]] <- td
    acq <- x@acquisitionData
    if (nrow(acq)) {
      acq$time_id <- newids[ix][match(acq$time_id, original_ids)]
      acq$experiment_id <- ids[[i]]
    }
    acquisitions[[i]] <- acq
    sources[[i]] <- list(metadata = x@metadata, timeData = x@timeData,
                          sampleData = x@sampleData, design = x@design)
    pos <- pos + n
  }
  names(sources) <- ids
  merged <- PlateReaderExperiment(out, sampleData = ref@sampleData,
    channelData = ref@channelData, timeData = .pre_merge_bind_rows(times),
    positionData = ref@positionData, design = ref@design,
    acquisitionData = .pre_merge_bind_rows(acquisitions),
    metadata = list(source = "Merged PlateReaderExperiment objects",
                    source_format = "merged_experiments", merge_axis = "time",
                    experiments = sources, processing_history = list()))
  .pre_history(merged, "merge_experiments", list(axis = "time", experiment_ids = ids))
}
