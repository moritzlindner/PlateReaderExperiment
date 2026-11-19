.pre_attach_by_match <- function(df, descriptor, key) {
  if (nrow(descriptor) == 0L) return(df)
  idx <- match(df[[key]], descriptor[[key]])
  extra <- setdiff(names(descriptor), names(df))
  for (nm in extra) df[[nm]] <- descriptor[[nm]][idx]
  df
}

#' Convert a PlateReaderExperiment to long format
#'
#' One row is returned per sample x channel x time x position combination.
#' Dimension descriptors and design columns are attached automatically.
#' @param x PlateReaderExperiment.
#' @param row.names,optional Standard as.data.frame arguments.
#' @param drop_na Remove rows with missing assay values.
#' @param include_channel_metadata Include all channelData columns.
#' @param include_acquisition_metadata Join acquisitionData.
#' @param ... Ignored.
#' @export
setMethod(
  "as.data.frame",
  "PlateReaderExperiment",
  function(x, row.names = NULL, optional = FALSE, ...,
           drop_na = FALSE,
           include_channel_metadata = TRUE,
           include_acquisition_metadata = TRUE) {
    d <- dim(x@data)
    grid <- expand.grid(
      sample_id = dimnames(x@data)[[1L]],
      channel_id = dimnames(x@data)[[2L]],
      time_id = dimnames(x@data)[[3L]],
      position_id = dimnames(x@data)[[4L]],
      KEEP.OUT.ATTRS = FALSE,
      stringsAsFactors = FALSE
    )
    grid$value <- as.vector(x@data)

    grid <- .pre_attach_by_match(grid, x@sampleData, "sample_id")
    grid <- .pre_attach_by_match(grid, x@design, "sample_id")
    if (include_channel_metadata) {
      grid <- .pre_attach_by_match(grid, x@channelData, "channel_id")
    }
    grid <- .pre_attach_by_match(grid, x@timeData, "time_id")
    grid <- .pre_attach_by_match(grid, x@positionData, "position_id")

    if (include_acquisition_metadata && nrow(x@acquisitionData) > 0L) {
      key_grid <- paste(grid$sample_id, grid$channel_id, grid$time_id, sep = "\r")
      key_acq <- paste(x@acquisitionData$sample_id, x@acquisitionData$channel_id,
                       x@acquisitionData$time_id, sep = "\r")
      idx <- match(key_grid, key_acq)
      extra <- setdiff(names(x@acquisitionData), names(grid))
      for (nm in extra) grid[[nm]] <- x@acquisitionData[[nm]][idx]
    }

    if (drop_na) grid <- grid[!is.na(grid$value), , drop = FALSE]
    rownames(grid) <- NULL
    grid
  }
)
