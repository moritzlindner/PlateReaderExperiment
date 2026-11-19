\name{offset_time_to_real}
\alias{offset_time_to_real}
\title{Align observed time to recorded acquisition timestamps}
\usage{
offset_time_to_real(x, origin = NULL)
}
\arguments{
  \item{x}{A \code{PlateReaderExperiment} with a recorded \code{timestamp} for every logical time point in \code{acquisitionData()}.}
  \item{origin}{Optional one-element POSIXct origin. Defaults to the earliest acquisition timestamp.}
}
\description{
Express observed time as seconds since a shared clock origin, using the earliest recorded acquisition timestamp across channels and wells at each logical time point.
}
\details{
Updates \code{timeData(x)$observed_time_s} and adds \code{absolute_time} as POSIXct. If present, \code{acquisitionData(x)$actual_time_s} is adjusted to the same origin. The requested \code{nominal_time_s} schedule and raw timestamps remain intact. Returns an error if any time point lacks a recorded timestamp. A processing-history entry and the origin are stored in \code{metadata()}.
}
\value{A valid \code{PlateReaderExperiment}.}
\examples{
\dontrun{
x <- offset_time_to_real(read.tecan("kinetic.xlsx"))
timeData(x)[, c("observed_time_s", "absolute_time")]
}
}
\seealso{\code{\link{merge_experiments}}}
