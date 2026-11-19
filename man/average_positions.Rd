\name{average_positions}
\alias{average_positions}
\title{Average within-well read positions}
\usage{
average_positions(x, na.rm = TRUE)
}
\arguments{
  \item{x}{A \code{PlateReaderExperiment}.}
  \item{na.rm}{Logical; ignore missing position measurements when calculating the mean.}
}
\description{
Averages the position dimension independently for every sample x channel x time combination while keeping the assay four-dimensional.
}
\details{
The returned position dimension has length one and identifier \code{"mean"}. The remaining dimensions are unchanged. If every source position is missing for a sample/channel/time combination, the averaged value remains \code{NA}. The operation is recorded in the processing history.
}
\value{A \code{PlateReaderExperiment} with position dimension length one.}
\examples{
\dontrun{
x_mean <- average_positions(x)
dim(x_mean)
positionData(x_mean)
}
}
\seealso{\code{\link{average_by_group}}, \code{\link{subtract_control}}}
