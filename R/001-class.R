#' PlateReaderExperiment class
#'
#' A plate-reader experiment with one canonical four-dimensional assay array.
#' Array dimensions are always sample x channel x time x position.
#'
#' @slot data Numeric four-dimensional array.
#' @slot sampleData Data frame describing samples/wells.
#' @slot channelData Data frame describing channels/filters.
#' @slot timeData Data frame describing logical time points.
#' @slot positionData Data frame describing within-well read positions.
#' @slot design Data frame describing experimental conditions; keyed by sample_id.
#' @slot acquisitionData Data frame with acquisition-specific metadata keyed by
#'   sample_id, channel_id and time_id.
#' @slot metadata Free-form experiment and provenance metadata.
#' @exportClass PlateReaderExperiment
setClass(
  "PlateReaderExperiment",
  slots = c(
    data = "array",
    sampleData = "data.frame",
    channelData = "data.frame",
    timeData = "data.frame",
    positionData = "data.frame",
    design = "data.frame",
    acquisitionData = "data.frame",
    metadata = "list"
  ),
  validity = function(object) {
    d <- dim(object@data)
    if (!is.numeric(object@data) || length(d) != 4L) {
      return("'data' must be a numeric 4-D array: sample x channel x time x position.")
    }
    if (is.null(dimnames(object@data)) || length(dimnames(object@data)) != 4L ||
        any(vapply(dimnames(object@data), is.null, logical(1L)))) {
      return("All four data dimensions must have dimnames.")
    }

    specs <- list(
      sample = list(df = object@sampleData, id = "sample_id", n = d[[1L]], dn = dimnames(object@data)[[1L]]),
      channel = list(df = object@channelData, id = "channel_id", n = d[[2L]], dn = dimnames(object@data)[[2L]]),
      time = list(df = object@timeData, id = "time_id", n = d[[3L]], dn = dimnames(object@data)[[3L]]),
      position = list(df = object@positionData, id = "position_id", n = d[[4L]], dn = dimnames(object@data)[[4L]])
    )
    for (nm in names(specs)) {
      s <- specs[[nm]]
      if (!(s$id %in% names(s$df))) {
        return(sprintf("%sData must contain column '%s'.", nm, s$id))
      }
      if (nrow(s$df) != s$n) {
        return(sprintf("%sData must have exactly %d rows.", nm, s$n))
      }
      ids <- as.character(s$df[[s$id]])
      if (anyNA(ids) || any(!nzchar(ids)) || anyDuplicated(ids)) {
        return(sprintf("%sData$%s must contain unique, non-missing identifiers.", nm, s$id))
      }
      if (!identical(ids, as.character(s$dn))) {
        return(sprintf("%sData$%s must exactly match the corresponding array dimnames.", nm, s$id))
      }
    }

    if (!("sample_id" %in% names(object@design))) {
      return("design must contain column 'sample_id'.")
    }
    design_ids <- as.character(object@design$sample_id)
    if (nrow(object@design) != d[[1L]] || anyDuplicated(design_ids) ||
        !identical(design_ids, as.character(object@sampleData$sample_id))) {
      return("design must contain exactly one row per sample in sampleData order.")
    }

    acq <- object@acquisitionData
    if (nrow(acq) > 0L) {
      req <- c("sample_id", "channel_id", "time_id")
      if (!all(req %in% names(acq))) {
        return("acquisitionData must contain sample_id, channel_id and time_id.")
      }
      if (any(!acq$sample_id %in% object@sampleData$sample_id) ||
          any(!acq$channel_id %in% object@channelData$channel_id) ||
          any(!acq$time_id %in% object@timeData$time_id)) {
        return("acquisitionData contains identifiers absent from the assay dimensions.")
      }
      key <- paste(acq$sample_id, acq$channel_id, acq$time_id, sep = "\r")
      if (anyDuplicated(key)) {
        return("acquisitionData keys sample_id x channel_id x time_id must be unique.")
      }
    }
    TRUE
  }
)

#' Construct a PlateReaderExperiment
#' @export
PlateReaderExperiment <- function(
    data,
    sampleData = NULL,
    channelData = NULL,
    timeData = NULL,
    positionData = NULL,
    design = NULL,
    acquisitionData = NULL,
    metadata = list()) {

  if (!is.numeric(data) || length(dim(data)) != 4L) {
    .pre_abort("'data' must be a numeric 4-D array: sample x channel x time x position.")
  }
  d <- dim(data)
  dn <- dimnames(data)
  defaults <- list(
    sample = if (!is.null(dn[[1L]])) dn[[1L]] else sprintf("S%03d", seq_len(d[[1L]])),
    channel = if (!is.null(dn[[2L]])) dn[[2L]] else sprintf("C%03d", seq_len(d[[2L]])),
    time = if (!is.null(dn[[3L]])) dn[[3L]] else sprintf("T%03d", seq_len(d[[3L]])),
    position = if (!is.null(dn[[4L]])) dn[[4L]] else sprintf("P%03d", seq_len(d[[4L]]))
  )
  dimnames(data) <- unname(defaults)
  names(dimnames(data)) <- c("sample", "channel", "time", "position")

  if (is.null(sampleData)) sampleData <- data.frame(sample_id = defaults$sample, stringsAsFactors = FALSE)
  if (is.null(channelData)) channelData <- data.frame(channel_id = defaults$channel, stringsAsFactors = FALSE)
  if (is.null(timeData)) timeData <- data.frame(time_id = defaults$time, stringsAsFactors = FALSE)
  if (is.null(positionData)) positionData <- data.frame(position_id = defaults$position, stringsAsFactors = FALSE)
  if (is.null(design)) {
    design <- data.frame(
      sample_id = as.character(sampleData$sample_id),
      biological_sample = as.character(sampleData$sample_id),
      group = NA_character_,
      stringsAsFactors = FALSE
    )
  }
  if (is.null(acquisitionData)) {
    acquisitionData <- data.frame(
      sample_id = character(), channel_id = character(), time_id = character(),
      stringsAsFactors = FALSE
    )
  }

  out <- new(
    "PlateReaderExperiment",
    data = data,
    sampleData = sampleData,
    channelData = channelData,
    timeData = timeData,
    positionData = positionData,
    design = design,
    acquisitionData = acquisitionData,
    metadata = metadata
  )
  validObject(out)
  out
}

#' @export
setMethod("show", "PlateReaderExperiment", function(object) {
  d <- dim(object@data)
  cat("PlateReaderExperiment\n")
  cat(sprintf("  data: %d samples x %d channels x %d times x %d positions\n",
              d[[1L]], d[[2L]], d[[3L]], d[[4L]]))
  if (!is.null(object@metadata$source_format)) {
    cat(sprintf("  source format: %s\n", object@metadata$source_format))
  }
  if ("group" %in% names(object@design) && any(!is.na(object@design$group))) {
    cat(sprintf("  groups: %s\n", paste(unique(stats::na.omit(object@design$group)), collapse = ", ")))
  }
  invisible(object)
})

#' @export
setMethod("dim", "PlateReaderExperiment", function(x) dim(x@data))

#' @export
setMethod("dimnames", "PlateReaderExperiment", function(x) dimnames(x@data))
