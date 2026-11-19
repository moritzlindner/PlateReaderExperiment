\name{PlateReaderExperiment-class}
\alias{PlateReaderExperiment-class}
\alias{class:PlateReaderExperiment}
\alias{show,PlateReaderExperiment-method}
\alias{dim,PlateReaderExperiment-method}
\alias{dimnames,PlateReaderExperiment-method}
\title{PlateReaderExperiment S4 class}
\description{
An S4 class representing plate-reader measurements with one canonical numeric four-dimensional assay array in the fixed dimension order sample, channel, time, position.
}
\section{Slots}{
\describe{
  \item{\code{data}}{Numeric four-dimensional array. All dimensions have dimnames and are never dropped by package methods.}
  \item{\code{sampleData}}{One row per sample/well. Must contain a unique \code{sample_id} exactly matching the first assay dimension. Tecan imports also provide plate and well coordinates.}
  \item{\code{channelData}}{One row per measurement channel. Must contain unique \code{channel_id}. Additional columns hold channel/filter metadata parsed from the source file.}
  \item{\code{timeData}}{One row per logical time point. Must contain unique \code{time_id}. May include cycle, nominal time, observed time, and source-file information.}
  \item{\code{positionData}}{One row per within-well read position. Must contain unique \code{position_id}. A single-read experiment has one position.}
  \item{\code{design}}{Exactly one row per sample, keyed by \code{sample_id}. Holds arbitrary experimental annotations such as group, biological sample, genotype, treatment, dose, or batch.}
  \item{\code{acquisitionData}}{Acquisition metadata keyed by unique sample x channel x time combinations. May contain timestamps, temperature, Tecan-reported mean/SD, number of observed positions, validity, and source file.}
  \item{\code{metadata}}{Free-form experiment, provenance, and processing metadata.}
}
}
\section{Validity}{
The class requires a numeric 4-D assay, matching descriptor rows and dimnames for all dimensions, exactly one design row per sample, and valid unique acquisition keys. Constructors and transformation methods call \code{validObject()} so inconsistent objects fail early.
}
\section{Dimension order}{
The assay dimension order is invariant:
\preformatted{sample x channel x time x position}
A simple plate read therefore still has four dimensions, with the position dimension having length one.
}
\section{Display methods}{
\code{show(x)} prints assay dimensions and selected metadata. \code{dim(x)} and \code{dimnames(x)} operate on the canonical assay.
}
\seealso{\code{\link{PlateReaderExperiment}}, \code{\link{PlateReaderExperiment-accessors}}, \code{\link{as.data.frame-PlateReaderExperiment}}}
