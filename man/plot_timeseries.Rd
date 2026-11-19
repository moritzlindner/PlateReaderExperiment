\name{plot_timeseries}
\alias{plot_timeseries}
\title{Plot plate-reader time series by group and channel}
\usage{
plot_timeseries(
  x,
  by = "group",
  channels = NULL,
  position_average = TRUE,
  group_average = TRUE,
  time_unit = c("seconds", "minutes", "hours")
)
}
\arguments{
  \item{x}{A \code{PlateReaderExperiment}.}
  \item{by}{Design column defining groups and colour.}
  \item{channels}{Optional channel ID or vector of channel IDs.}
  \item{position_average}{Logical; average within-well positions before plotting.}
  \item{group_average}{Logical; average samples within \code{by} before plotting.}
  \item{time_unit}{Display time unit: seconds, minutes, or hours.}
}
\description{
Creates line-and-point time-series plots using experimental-design groups and one or more measurement channels.
}
\details{
When available, \code{observed_time_s} is used for the x axis; otherwise \code{nominal_time_s} is used, followed by the sequential time index as a final fallback.

With \code{position_average = TRUE}, multiple within-well reads are first averaged. With \code{group_average = TRUE}, samples are then averaged within the selected design factor. Multiple channels are faceted with independent y scales.
}
\value{A \code{ggplot} object. Requires the suggested package \pkg{ggplot2}.}
\examples{
\dontrun{
plot_timeseries(
  x,
  by = "group",
  channels = c("eGFP-Peak-Peak", "eGFP-Peak-Trough"),
  position_average = TRUE,
  group_average = TRUE,
  time_unit = "minutes"
)
}
}
\seealso{\code{\link{plot_violin}}, \code{\link{plot_boxplot}}}
