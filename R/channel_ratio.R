#' Calculate the ratio of two channels
#'
#' Computes an element-wise ratio between two assay channels for every sample,
#' time point and within-well position. The derived values are appended to the
#' input object as an additional channel; the original channels are retained.
#'
#' Missing values propagate according to ordinary R arithmetic. Division by
#' zero follows ordinary R semantics and therefore produces \code{Inf} or
#' \code{-Inf} where applicable.
#'
#' @param x A \code{PlateReaderExperiment}.
#' @param numerator Single channel identifier or channel index used as the
#'   numerator.
#' @param denominator Single channel identifier or channel index used as the
#'   denominator.
#' @param name Name for the newly created ratio channel. If \code{NULL}, a name
#'   of the form \code{"<numerator>_over_<denominator>"} is used.
#'
#' @return A \code{PlateReaderExperiment} containing all original channels and
#'   one additional derived ratio channel.
#'
#' @examples
#' \dontrun{
#' x_ratio <- channel_ratio(
#'   x,
#'   numerator = "eGFP-Peak-Peak",
#'   denominator = "mCherry",
#'   name = "GFP_mCherry_ratio"
#' )
#'
#' assay(x_ratio)[, "GFP_mCherry_ratio", , , drop = FALSE]
#' }
#'
#' @seealso \code{\link{subtract_control}}
#' @export
channel_ratio <- function(x, numerator, denominator, name = NULL) {
  if (!is(x, "PlateReaderExperiment")) {
    .pre_abort("x must be a PlateReaderExperiment.")
  }

  channels <- as.character(x@channelData$channel_id)
  numerator_idx <- .pre_resolve(numerator, channels, "numerator channel")
  denominator_idx <- .pre_resolve(denominator, channels, "denominator channel")

  if (length(numerator_idx) != 1L) {
    .pre_abort("'numerator' must identify exactly one channel.")
  }
  if (length(denominator_idx) != 1L) {
    .pre_abort("'denominator' must identify exactly one channel.")
  }

  numerator_id <- channels[[numerator_idx]]
  denominator_id <- channels[[denominator_idx]]

  if (is.null(name)) {
    name <- paste0(numerator_id, "_over_", denominator_id)
  }
  if (!is.character(name) || length(name) != 1L || is.na(name) ||
      !nzchar(trimws(name))) {
    .pre_abort("'name' must be one non-empty character string.")
  }
  name <- trimws(name)
  if (name %in% channels) {
    .pre_abort("Channel '%s' already exists; choose a different 'name'.", name)
  }

  d <- dim(x@data)
  new_channels <- c(channels, name)
  out <- array(
    NA_real_,
    dim = c(d[[1L]], d[[2L]] + 1L, d[[3L]], d[[4L]]),
    dimnames = list(
      sample = dimnames(x@data)[[1L]],
      channel = new_channels,
      time = dimnames(x@data)[[3L]],
      position = dimnames(x@data)[[4L]]
    )
  )

  out[, seq_len(d[[2L]]), , ] <- x@data
  out[, d[[2L]] + 1L, , ] <-
    x@data[, numerator_idx, , ] / x@data[, denominator_idx, , ]

  cd <- x@channelData
  if (!("channel_name" %in% names(cd))) {
    cd$channel_name <- as.character(cd$channel_id)
  }
  if (!("derived_operation" %in% names(cd))) {
    cd$derived_operation <- NA_character_
  }
  if (!("numerator_channel" %in% names(cd))) {
    cd$numerator_channel <- NA_character_
  }
  if (!("denominator_channel" %in% names(cd))) {
    cd$denominator_channel <- NA_character_
  }

  cd[nrow(cd) + 1L, ] <- NA
  new_row <- nrow(cd)
  cd$channel_id[[new_row]] <- name
  cd$channel_name[[new_row]] <- name
  cd$derived_operation[[new_row]] <- "ratio"
  cd$numerator_channel[[new_row]] <- numerator_id
  cd$denominator_channel[[new_row]] <- denominator_id
  rownames(cd) <- NULL

  x@data <- out
  x@channelData <- cd
  x <- .pre_history(
    x,
    "channel_ratio",
    list(
      numerator = numerator_id,
      denominator = denominator_id,
      channel_name = name
    )
  )

  validObject(x)
  x
}
