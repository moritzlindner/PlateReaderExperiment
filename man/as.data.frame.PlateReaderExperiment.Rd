\name{as.data.frame-PlateReaderExperiment}
\alias{as.data.frame,PlateReaderExperiment-method}
\title{Convert a PlateReaderExperiment to long format}
\usage{
\S4method{as.data.frame}{PlateReaderExperiment}(
  x,
  row.names = NULL,
  optional = FALSE,
  ...,
  drop_na = FALSE,
  include_channel_metadata = TRUE,
  include_acquisition_metadata = TRUE
)
}
\arguments{
  \item{x}{A \code{PlateReaderExperiment}.}
  \item{row.names, optional}{Standard \code{as.data.frame} arguments; not used to alter the experiment mapping.}
  \item{...}{Ignored.}
  \item{drop_na}{Logical; remove rows whose assay \code{value} is missing.}
  \item{include_channel_metadata}{Logical; append all additional columns from \code{channelData(x)}.}
  \item{include_acquisition_metadata}{Logical; append matching sample x channel x time acquisition metadata.}
}
\description{
Creates one row per sample x channel x time x position assay cell and attaches dimension descriptors and experimental-design annotations.
}
\details{
The output always contains \code{sample_id}, \code{channel_id}, \code{time_id}, \code{position_id}, and \code{value}. Sample, design, time, and position descriptors are attached automatically. Channel and acquisition metadata can be omitted to reduce repeated columns in very large long-format data frames.
}
\value{A long-format \code{data.frame}.}
\examples{
\dontrun{
df <- as.data.frame(x)
df_observed <- as.data.frame(x, drop_na = TRUE)
df_small <- as.data.frame(
  x,
  drop_na = TRUE,
  include_channel_metadata = FALSE,
  include_acquisition_metadata = FALSE
)
}
}
\seealso{\code{\link{subset.PlateReaderExperiment}}, \code{\link{PlateReaderExperiment-accessors}}}
