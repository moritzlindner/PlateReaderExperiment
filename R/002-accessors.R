#' Access the assay array
#' @export
assay <- function(x) {
  if (!is(x, "PlateReaderExperiment")) .pre_abort("x must be a PlateReaderExperiment.")
  x@data
}

#' @export
sampleData <- function(x) x@sampleData
#' @export
channelData <- function(x) x@channelData
#' @export
timeData <- function(x) x@timeData
#' @export
positionData <- function(x) x@positionData
#' @export
design <- function(x) x@design
#' @export
acquisitionData <- function(x) x@acquisitionData
#' @export
metadata <- function(x) x@metadata

#' Replace experimental design
#' @export
`design<-` <- function(x, value) {
  if (!is(x, "PlateReaderExperiment")) .pre_abort("x must be a PlateReaderExperiment.")
  if (!is.data.frame(value)) .pre_abort("design must be a data.frame.")
  if (!("sample_id" %in% names(value))) .pre_abort("design must contain 'sample_id'.")
  idx <- match(x@sampleData$sample_id, as.character(value$sample_id))
  if (anyNA(idx) || nrow(value) != nrow(x@sampleData) || anyDuplicated(value$sample_id)) {
    .pre_abort("design must contain exactly one row for every sample_id.")
  }
  value <- value[idx, , drop = FALSE]
  rownames(value) <- NULL
  x@design <- value
  validObject(x)
  x
}

#' Add or replace experimental-design annotation
#'
#' @param x PlateReaderExperiment.
#' @param value Data frame containing design information.
#' @param by Key in both sampleData and value, commonly "sample_id" or "well".
#' @export
set_design <- function(x, value, by = "sample_id") {
  if (!is.data.frame(value)) .pre_abort("value must be a data.frame.")
  if (!(by %in% names(x@sampleData)) || !(by %in% names(value))) {
    .pre_abort("'%s' must be present in both sampleData and value.", by)
  }
  if (anyDuplicated(value[[by]])) .pre_abort("Design key '%s' must be unique.", by)
  idx <- match(x@sampleData[[by]], value[[by]])
  if (anyNA(idx)) {
    .pre_abort("No design row found for: %s",
               paste(x@sampleData[[by]][is.na(idx)], collapse = ", "))
  }
  added <- value[idx, setdiff(names(value), by), drop = FALSE]
  old <- x@design
  for (nm in names(added)) old[[nm]] <- added[[nm]]
  old$sample_id <- x@sampleData$sample_id
  old <- old[, c("sample_id", setdiff(names(old), "sample_id")), drop = FALSE]
  design(x) <- old
  x
}
