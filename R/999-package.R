#' PlateReaderExperiment: structured plate-reader experiments
#'
#' `PlateReaderExperiment` provides an S4 representation for plate-reader data
#' with a canonical four-dimensional assay in the fixed order sample x channel x
#' time x position. The package imports supported Tecan SparkControl Excel
#' exports and provides tools for experimental-design annotation, subsetting,
#' position and group averaging, control subtraction, long-format conversion,
#' and plotting.
#'
#' The principal workflow is:
#'
#' 1. Import with [read.tecan()].
#' 2. Assign experimental conditions with [set_design()].
#' 3. Inspect or transform using [average_positions()], [average_by_group()],
#'    [subtract_control()], or [subset()].
#' 4. Convert with `as.data.frame()` or plot with [plot_timeseries()],
#'    [plot_violin()], and [plot_boxplot()].
#'
#' See the package README and `inst/doc/PlateReaderExperiment-guide.md` for the
#' complete user guide.
#'
#' @docType package
#' @name PlateReaderExperiment-package
#' @keywords internal
"_PACKAGE"
