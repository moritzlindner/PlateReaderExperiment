\name{PlateReaderExperiment}
\alias{PlateReaderExperiment}
\title{Construct a PlateReaderExperiment}
\usage{
PlateReaderExperiment(
  data,
  sampleData = NULL,
  channelData = NULL,
  timeData = NULL,
  positionData = NULL,
  design = NULL,
  acquisitionData = NULL,
  metadata = list()
)
}
\arguments{
  \item{data}{Numeric four-dimensional array in sample x channel x time x position order.}
  \item{sampleData}{Optional data frame describing the first dimension. Must contain \code{sample_id}.}
  \item{channelData}{Optional data frame describing channels. Must contain \code{channel_id}.}
  \item{timeData}{Optional data frame describing time points. Must contain \code{time_id}.}
  \item{positionData}{Optional data frame describing positions. Must contain \code{position_id}.}
  \item{design}{Optional experimental-design data frame with one row per sample and column \code{sample_id}.}
  \item{acquisitionData}{Optional acquisition metadata containing \code{sample_id}, \code{channel_id}, and \code{time_id}.}
  \item{metadata}{Free-form list of experiment metadata.}
}
\description{
Constructs and validates a \code{PlateReaderExperiment}. Missing descriptor tables are generated from assay dimnames, or from automatically generated identifiers when dimnames are absent.
}
\details{
When \code{design} is omitted, a default design is created with \code{sample_id}, \code{biological_sample}, and an unassigned \code{group}. Descriptor identifiers must exactly match the corresponding assay dimnames and order.
}
\value{A valid \code{PlateReaderExperiment} object.}
\examples{
a <- array(
  rnorm(4 * 2 * 3),
  dim = c(4, 2, 3, 1),
  dimnames = list(
    sample = c("A1", "A2", "B1", "B2"),
    channel = c("GFP", "RFP"),
    time = c("T001", "T002", "T003"),
    position = "P001"
  )
)
x <- PlateReaderExperiment(a)
dim(x)
}
\seealso{\code{\link{PlateReaderExperiment-class}}, \code{\link{read.tecan}}}
