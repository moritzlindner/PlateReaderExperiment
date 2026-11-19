\name{PlateReaderExperiment-package}
\alias{PlateReaderExperiment-package}
\docType{package}
\title{Structured Plate Reader Experiments}
\description{
The \pkg{PlateReaderExperiment} package provides an S4 representation for plate-reader experiments with a canonical four-dimensional assay, import of supported Tecan SparkControl Excel exports, experimental-design annotation, aggregation, control subtraction, long-format conversion, and plotting helpers.
}
\details{
Every experiment uses the fixed assay dimension order \code{sample x channel x time x position}. Single-read exports use a position dimension of length one; single-time multi-read and kinetic multi-read exports retain each within-well spatial position. Missing or interrupted measurements are represented by \code{NA}.

The principal entry point for Tecan data is \code{read.tecan()}. See \code{?PlateReaderExperiment-class} for the object model and \code{?read.tecan} for supported import layouts.
}
\seealso{
\code{\link{PlateReaderExperiment}}, \code{\link{read.tecan}}, \code{\link{set_design}}, \code{\link{average_positions}}, \code{\link{average_by_group}}, \code{\link{subtract_control}}, \code{\link{plot_timeseries}}, \code{\link{plot_violin}}, \code{\link{plot_boxplot}}
}
