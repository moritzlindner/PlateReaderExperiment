#' Subset a PlateReaderExperiment by assay dimensions
#'
#' Dimension order is sample, channel, time, position and dimensions are never
#' dropped.
#' @export
setMethod(
  "[",
  signature(x = "PlateReaderExperiment"),
  function(x, i, j, ..., drop = FALSE) {
    dots <- list(...)
    if (length(dots) > 2L) .pre_abort("PlateReaderExperiment has exactly four dimensions.")

    si <- if (missing(i)) seq_len(dim(x)[[1L]]) else .pre_resolve(i, dimnames(x)[[1L]], "sample")
    ci <- if (missing(j)) seq_len(dim(x)[[2L]]) else .pre_resolve(j, dimnames(x)[[2L]], "channel")
    ti <- if (length(dots) >= 1L) .pre_resolve(dots[[1L]], dimnames(x)[[3L]], "time") else seq_len(dim(x)[[3L]])
    pi <- if (length(dots) >= 2L) .pre_resolve(dots[[2L]], dimnames(x)[[4L]], "position") else seq_len(dim(x)[[4L]])

    x@data <- x@data[si, ci, ti, pi, drop = FALSE]
    x@sampleData <- x@sampleData[si, , drop = FALSE]
    x@channelData <- x@channelData[ci, , drop = FALSE]
    x@timeData <- x@timeData[ti, , drop = FALSE]
    x@positionData <- x@positionData[pi, , drop = FALSE]
    x@design <- x@design[si, , drop = FALSE]
    rownames(x@sampleData) <- rownames(x@channelData) <- rownames(x@timeData) <- NULL
    rownames(x@positionData) <- rownames(x@design) <- NULL

    if (nrow(x@acquisitionData) > 0L) {
      keep <- x@acquisitionData$sample_id %in% x@sampleData$sample_id &
        x@acquisitionData$channel_id %in% x@channelData$channel_id &
        x@acquisitionData$time_id %in% x@timeData$time_id
      x@acquisitionData <- x@acquisitionData[keep, , drop = FALSE]
      rownames(x@acquisitionData) <- NULL
    }
    validObject(x)
    x
  }
)

#' Subset by dimension identifiers and experimental-design values
#'
#' @param x PlateReaderExperiment.
#' @param samples,channels,times,positions Optional identifiers or indices.
#' @param group Optional value(s) from design$group.
#' @param ... Additional named design filters, e.g. treatment="drug".
#' @export
subset.PlateReaderExperiment <- function(
    x, samples = NULL, channels = NULL, times = NULL, positions = NULL,
    group = NULL, ...) {

  sample_ids <- x@sampleData$sample_id
  keep <- rep(TRUE, length(sample_ids))
  if (!is.null(samples)) {
    idx <- .pre_resolve(samples, sample_ids, "sample")
    keep <- seq_along(sample_ids) %in% idx
  }

  filters <- list(...)
  if (!is.null(group)) filters$group <- group
  if (length(filters) > 0L) {
    for (nm in names(filters)) {
      if (!(nm %in% names(x@design))) .pre_abort("Unknown design column '%s'.", nm)
      keep <- keep & x@design[[nm]] %in% filters[[nm]]
    }
  }

  si <- which(keep)
  ci <- .pre_resolve(channels, x@channelData$channel_id, "channel")
  ti <- .pre_resolve(times, x@timeData$time_id, "time")
  pi <- .pre_resolve(positions, x@positionData$position_id, "position")
  x[si, ci, ti, pi]
}
