#' Plot time series by group and channel
#'
#' @param x PlateReaderExperiment.
#' @param by Design column defining lines/groups.
#' @param channels Optional channel IDs.
#' @param position_average Average within-well positions before plotting.
#' @param group_average Average samples within `by` before plotting.
#' @param time_unit seconds, minutes, or hours.
#' @export
plot_timeseries <- function(
    x, by = "group", channels = NULL,
    position_average = TRUE, group_average = TRUE,
    time_unit = c("seconds", "minutes", "hours")) {

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    .pre_abort("Package 'ggplot2' is required for plotting. Install it with install.packages('ggplot2').")
  }
  time_unit <- match.arg(time_unit)
  if (!(by %in% names(x@design))) .pre_abort("Unknown design column '%s'.", by)
  if (!is.null(channels)) x <- subset(x, channels = channels)
  if (position_average && dim(x)[[4L]] > 1L) x <- average_positions(x)
  if (group_average) x <- average_by_group(x, by = by)
  df <- as.data.frame(x, drop_na = TRUE, include_acquisition_metadata = FALSE)

  if ("observed_time_s" %in% names(df) && any(!is.na(df$observed_time_s))) {
    df$.plot_time <- df$observed_time_s
  } else if ("nominal_time_s" %in% names(df) && any(!is.na(df$nominal_time_s))) {
    df$.plot_time <- df$nominal_time_s
  } else {
    df$.plot_time <- match(df$time_id, x@timeData$time_id) - 1L
  }
  divisor <- switch(time_unit, seconds = 1, minutes = 60, hours = 3600)
  df$.plot_time <- df$.plot_time / divisor

  group_var <- if (group_average) by else "sample_id"
  p <- ggplot2::ggplot(
    df,
    ggplot2::aes_string(x = ".plot_time", y = "value", colour = by, group = group_var)
  ) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::labs(x = paste("Time [", time_unit, "]", sep = ""), y = "Signal", colour = by) +
    ggplot2::theme_bw()

  if (length(unique(df$channel_id)) > 1L) {
    p <- p + ggplot2::facet_wrap(stats::as.formula("~ channel_id"), scales = "free_y")
  }
  p
}

#' Violin plot with sample and optional within-well position dots
#'
#' The violin distribution is based on one position-averaged value per sample,
#' avoiding treatment of within-well positions as independent experimental
#' units. With `show_positions = TRUE`, raw position values are superimposed as
#' dots.
#'
#' @param x PlateReaderExperiment.
#' @param group_by Design column displayed on the x axis.
#' @param channels,times Optional channel/time IDs.
#' @param colour_by Column in the long data frame used for dot colour. Defaults
#'   to sample_id.
#' @param shape_by Optional column used for dot shape. With show_positions=TRUE,
#'   defaults to position_id.
#' @param show_positions Show individual within-well positions as dots.
#' @param violin_alpha Alpha for violin layer.
#' @param point_size Dot size.
#' @export
plot_violin <- function(
    x,
    group_by = "group",
    channels = NULL,
    times = NULL,
    colour_by = "sample_id",
    shape_by = NULL,
    show_positions = FALSE,
    violin_alpha = 0.35,
    point_size = 2.2) {

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    .pre_abort("Package 'ggplot2' is required for plotting. Install it with install.packages('ggplot2').")
  }
  if (!(group_by %in% names(x@design))) .pre_abort("Unknown design column '%s'.", group_by)
  x <- subset(x, channels = channels, times = times)

  means <- if (dim(x)[[4L]] > 1L) average_positions(x) else x
  violin_df <- as.data.frame(means, drop_na = TRUE, include_acquisition_metadata = FALSE)
  point_df <- if (show_positions) {
    as.data.frame(x, drop_na = TRUE, include_acquisition_metadata = FALSE)
  } else {
    violin_df
  }

  if (is.null(shape_by) && show_positions) shape_by <- "position_id"
  required <- c(group_by, colour_by, shape_by)
  required <- required[!is.na(required) & nzchar(required)]
  missing_cols <- setdiff(required, names(point_df))
  if (length(missing_cols)) .pre_abort("Plot column(s) not found: %s", paste(missing_cols, collapse = ", "))

  p <- ggplot2::ggplot() +
    ggplot2::geom_violin(
      data = violin_df,
      mapping = ggplot2::aes_string(x = group_by, y = "value"),
      trim = FALSE, alpha = violin_alpha
    )

  point_mapping <- if (is.null(shape_by)) {
    ggplot2::aes_string(x = group_by, y = "value", colour = colour_by)
  } else {
    ggplot2::aes_string(x = group_by, y = "value", colour = colour_by, shape = shape_by)
  }
  p <- p + ggplot2::geom_jitter(
    data = point_df,
    mapping = point_mapping,
    width = 0.12,
    height = 0,
    size = point_size,
    alpha = 0.85
  ) +
    ggplot2::labs(x = group_by, y = "Signal", colour = colour_by,
                  shape = if (is.null(shape_by)) NULL else shape_by) +
    ggplot2::theme_bw()

  if (!is.null(shape_by)) {
    lev <- unique(as.character(point_df[[shape_by]]))
    lev <- lev[!is.na(lev)]
    shape_values <- rep(0:25, length.out = length(lev))
    names(shape_values) <- lev
    p <- p + ggplot2::scale_shape_manual(values = shape_values)
  }

  nc <- length(unique(point_df$channel_id))
  nt <- length(unique(point_df$time_id))
  if (nc > 1L && nt > 1L) {
    p <- p + ggplot2::facet_grid(stats::as.formula("time_id ~ channel_id"), scales = "free_y")
  } else if (nc > 1L) {
    p <- p + ggplot2::facet_wrap(stats::as.formula("~ channel_id"), scales = "free_y")
  } else if (nt > 1L) {
    p <- p + ggplot2::facet_wrap(stats::as.formula("~ time_id"), scales = "free_y")
  }
  p
}

#' Boxplot with sample and optional within-well position dots
#'
#' The boxplot distribution is based on one position-averaged value per sample,
#' so within-well spatial reads are not treated as independent experimental
#' units. With `show_positions = TRUE`, the individual position measurements
#' are superimposed as dots. Colour and shape can encode additional sample or
#' position-level dimensions.
#'
#' @param x PlateReaderExperiment.
#' @param group_by Design column displayed on the x axis.
#' @param channels,times Optional channel/time IDs.
#' @param colour_by Column in the long data frame used for dot colour. Defaults
#'   to `sample_id`.
#' @param shape_by Optional column used for dot shape. With
#'   `show_positions = TRUE`, defaults to `position_id`.
#' @param show_positions Show individual within-well positions as dots. The
#'   boxplot itself always uses position-averaged sample values.
#' @param box_alpha Alpha for the boxplot fill.
#' @param box_width Width of the boxes.
#' @param point_size Dot size.
#'
#' @return A `ggplot` object. If multiple channels and/or time points are
#'   selected, the plot is faceted in the same way as [plot_violin()].
#'
#' @examples
#' \dontrun{
#' plot_boxplot(
#'   x,
#'   group_by = "group",
#'   channels = "eGFP-Peak-Peak",
#'   times = "T002",
#'   colour_by = "biological_sample"
#' )
#'
#' plot_boxplot(
#'   x,
#'   group_by = "group",
#'   channels = "eGFP-Peak-Peak",
#'   times = "T002",
#'   colour_by = "biological_sample",
#'   shape_by = "position_id",
#'   show_positions = TRUE
#' )
#' }
#' @export
plot_boxplot <- function(
    x,
    group_by = "group",
    channels = NULL,
    times = NULL,
    colour_by = "sample_id",
    shape_by = NULL,
    show_positions = FALSE,
    box_alpha = 0.35,
    box_width = 0.65,
    point_size = 2.2) {

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    .pre_abort("Package 'ggplot2' is required for plotting. Install it with install.packages('ggplot2').")
  }
  if (!(group_by %in% names(x@design))) .pre_abort("Unknown design column '%s'.", group_by)
  x <- subset(x, channels = channels, times = times)

  means <- if (dim(x)[[4L]] > 1L) average_positions(x) else x
  box_df <- as.data.frame(means, drop_na = TRUE, include_acquisition_metadata = FALSE)
  point_df <- if (show_positions) {
    as.data.frame(x, drop_na = TRUE, include_acquisition_metadata = FALSE)
  } else {
    box_df
  }

  if (is.null(shape_by) && show_positions) shape_by <- "position_id"
  required <- c(group_by, colour_by, shape_by)
  required <- required[!is.na(required) & nzchar(required)]
  missing_cols <- setdiff(required, names(point_df))
  if (length(missing_cols)) .pre_abort("Plot column(s) not found: %s", paste(missing_cols, collapse = ", "))

  p <- ggplot2::ggplot() +
    ggplot2::geom_boxplot(
      data = box_df,
      mapping = ggplot2::aes_string(x = group_by, y = "value"),
      width = box_width,
      alpha = box_alpha,
      outlier.shape = NA
    )

  point_mapping <- if (is.null(shape_by)) {
    ggplot2::aes_string(x = group_by, y = "value", colour = colour_by)
  } else {
    ggplot2::aes_string(x = group_by, y = "value", colour = colour_by, shape = shape_by)
  }
  p <- p + ggplot2::geom_jitter(
    data = point_df,
    mapping = point_mapping,
    width = 0.12,
    height = 0,
    size = point_size,
    alpha = 0.85
  ) +
    ggplot2::labs(x = group_by, y = "Signal", colour = colour_by,
                  shape = if (is.null(shape_by)) NULL else shape_by) +
    ggplot2::theme_bw()

  if (!is.null(shape_by)) {
    lev <- unique(as.character(point_df[[shape_by]]))
    lev <- lev[!is.na(lev)]
    shape_values <- rep(0:25, length.out = length(lev))
    names(shape_values) <- lev
    p <- p + ggplot2::scale_shape_manual(values = shape_values)
  }

  nc <- length(unique(point_df$channel_id))
  nt <- length(unique(point_df$time_id))
  if (nc > 1L && nt > 1L) {
    p <- p + ggplot2::facet_grid(stats::as.formula("time_id ~ channel_id"), scales = "free_y")
  } else if (nc > 1L) {
    p <- p + ggplot2::facet_wrap(stats::as.formula("~ channel_id"), scales = "free_y")
  } else if (nt > 1L) {
    p <- p + ggplot2::facet_wrap(stats::as.formula("~ time_id"), scales = "free_y")
  }
  p
}
