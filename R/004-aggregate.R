.pre_summarise_acquisition <- function(acq, member_ids, new_id) {
  if (nrow(acq) == 0L) return(acq)
  z <- acq[acq$sample_id %in% member_ids, , drop = FALSE]
  if (nrow(z) == 0L) return(z)
  keys <- unique(z[, c("channel_id", "time_id"), drop = FALSE])
  out <- vector("list", nrow(keys))
  for (k in seq_len(nrow(keys))) {
    q <- z[z$channel_id == keys$channel_id[[k]] & z$time_id == keys$time_id[[k]], , drop = FALSE]
    row <- list(sample_id = new_id, channel_id = keys$channel_id[[k]], time_id = keys$time_id[[k]])
    for (nm in setdiff(names(q), c("sample_id", "channel_id", "time_id"))) {
      v <- q[[nm]]
      if (inherits(v, "POSIXct")) {
        nums <- as.numeric(v)
        val <- if (all(is.na(nums))) NA_real_ else mean(nums, na.rm = TRUE)
        row[[nm]] <- as.POSIXct(val, origin = "1970-01-01", tz = attr(v, "tzone") %||% "UTC")
      } else if (is.numeric(v)) {
        row[[nm]] <- .pre_mean_or_na(v, TRUE)
      } else if (is.logical(v)) {
        row[[nm]] <- any(v %in% TRUE, na.rm = TRUE)
      } else {
        nonmissing <- unique(as.character(v[!is.na(v)]))
        row[[nm]] <- if (length(nonmissing) == 1L) nonmissing else NA_character_
      }
    }
    out[[k]] <- as.data.frame(row, stringsAsFactors = FALSE)
  }
  do.call(rbind, out)
}

#' Average within-well positions
#'
#' Returns another PlateReaderExperiment and keeps the position dimension with
#' length one.
#' @export
average_positions <- function(x, na.rm = TRUE) {
  if (!is(x, "PlateReaderExperiment")) .pre_abort("x must be a PlateReaderExperiment.")
  d <- dim(x@data)
  out <- array(
    NA_real_,
    dim = c(d[[1L]], d[[2L]], d[[3L]], 1L),
    dimnames = list(
      sample = dimnames(x@data)[[1L]],
      channel = dimnames(x@data)[[2L]],
      time = dimnames(x@data)[[3L]],
      position = "mean"
    )
  )
  for (s in seq_len(d[[1L]])) {
    for (ch in seq_len(d[[2L]])) {
      for (tt in seq_len(d[[3L]])) {
        out[s, ch, tt, 1L] <- .pre_mean_or_na(x@data[s, ch, tt, ], na.rm = na.rm)
      }
    }
  }
  x@data <- out
  x@positionData <- data.frame(
    position_id = "mean",
    summary = "mean across within-well positions",
    n_source_positions = d[[4L]],
    stringsAsFactors = FALSE
  )
  x <- .pre_history(x, "average_positions", list(na.rm = na.rm))
  validObject(x)
  x
}

#' Average samples by one or more design columns
#'
#' @param x PlateReaderExperiment.
#' @param by Column name(s) in design defining groups.
#' @param na.rm Remove missing values before averaging.
#' @export
average_by_group <- function(x, by = "group", na.rm = TRUE) {
  if (!all(by %in% names(x@design))) {
    .pre_abort("Unknown design column(s): %s", paste(setdiff(by, names(x@design)), collapse = ", "))
  }
  if (length(by) == 0L) .pre_abort("'by' must contain at least one design column.")
  key_df <- x@design[, by, drop = FALSE]
  if (anyNA(key_df)) .pre_abort("Grouping columns contain missing values.")
  key <- do.call(paste, c(key_df, sep = " | "))
  groups <- unique(key)
  d <- dim(x@data)
  out <- array(
    NA_real_, dim = c(length(groups), d[[2L]], d[[3L]], d[[4L]]),
    dimnames = list(sample = make.unique(groups), channel = dimnames(x@data)[[2L]],
                    time = dimnames(x@data)[[3L]], position = dimnames(x@data)[[4L]])
  )

  new_sample <- vector("list", length(groups))
  new_design <- vector("list", length(groups))
  new_acq <- vector("list", length(groups))
  for (g in seq_along(groups)) {
    members <- which(key == groups[[g]])
    for (ch in seq_len(d[[2L]])) for (tt in seq_len(d[[3L]])) for (pp in seq_len(d[[4L]])) {
      out[g, ch, tt, pp] <- .pre_mean_or_na(x@data[members, ch, tt, pp], na.rm = na.rm)
    }
    sid <- dimnames(out)[[1L]][[g]]
    new_sample[[g]] <- data.frame(
      sample_id = sid, plate_id = NA_character_, well = NA_character_,
      plate_row = NA_character_, plate_row_index = NA_integer_, plate_column = NA_integer_,
      n_source_samples = length(members), stringsAsFactors = FALSE
    )
    dd <- x@design[members[[1L]], by, drop = FALSE]
    dd$sample_id <- sid
    dd$n_source_samples <- length(members)
    dd <- dd[, c("sample_id", by, "n_source_samples"), drop = FALSE]
    new_design[[g]] <- dd
    new_acq[[g]] <- .pre_summarise_acquisition(x@acquisitionData, x@sampleData$sample_id[members], sid)
  }

  x@data <- out
  x@sampleData <- do.call(rbind, new_sample)
  x@design <- do.call(rbind, new_design)
  acq_nonempty <- Filter(function(z) nrow(z) > 0L, new_acq)
  x@acquisitionData <- if (length(acq_nonempty)) do.call(rbind, acq_nonempty) else
    data.frame(sample_id = character(), channel_id = character(), time_id = character(), stringsAsFactors = FALSE)
  rownames(x@sampleData) <- rownames(x@design) <- rownames(x@acquisitionData) <- NULL
  x <- .pre_history(x, "average_by_group", list(by = by, na.rm = na.rm))
  validObject(x)
  x
}

#' Subtract the mean of a control condition
#'
#' The control mean is calculated independently for every channel, time point
#' and position, then subtracted from every sample.
#' @param x PlateReaderExperiment.
#' @param control Control value(s) in design column `by`.
#' @param by Design column identifying the control group.
#' @param na.rm Ignore missing control measurements.
#' @export
subtract_control <- function(x, control, by = "group", na.rm = TRUE) {
  if (!(by %in% names(x@design))) .pre_abort("Unknown design column '%s'.", by)
  controls <- which(x@design[[by]] %in% control)
  if (length(controls) == 0L) .pre_abort("No samples match the requested control in design$%s.", by)
  d <- dim(x@data)
  ctrl <- array(NA_real_, dim = d[2:4], dimnames = dimnames(x@data)[2:4])
  for (ch in seq_len(d[[2L]])) for (tt in seq_len(d[[3L]])) for (pp in seq_len(d[[4L]])) {
    ctrl[ch, tt, pp] <- .pre_mean_or_na(x@data[controls, ch, tt, pp], na.rm = na.rm)
  }
  for (s in seq_len(d[[1L]])) {
    x@data[s, , , ] <- x@data[s, , , ] - ctrl
  }
  x <- .pre_history(x, "subtract_control", list(by = by, control = control, na.rm = na.rm))
  md <- x@metadata
  md$last_control_values <- ctrl
  x@metadata <- md
  validObject(x)
  x
}
