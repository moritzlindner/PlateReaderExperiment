\name{channel_ratio}
\alias{channel_ratio}
\title{Calculate the ratio of two channels}
\usage{
channel_ratio(x, numerator, denominator, name = NULL)
}
\arguments{
\item{x}{A \code{PlateReaderExperiment}.}

\item{numerator}{Single channel identifier or channel index used as the numerator.}

\item{denominator}{Single channel identifier or channel index used as the denominator.}

\item{name}{Name for the newly created ratio channel. If \code{NULL}, a name of the form \code{"<numerator>_over_<denominator>"} is used.}
}
\value{
A \code{PlateReaderExperiment} containing all original channels and one additional derived ratio channel.
}
\description{
Computes an element-wise ratio between two assay channels for every sample,
time point and within-well position. The derived values are appended to the
input object as an additional channel; the original channels are retained.
}
\details{
Missing values propagate according to ordinary R arithmetic. Division by zero
follows ordinary R semantics and therefore produces \code{Inf} or \code{-Inf}
where applicable.
}
\examples{
\dontrun{
x_ratio <- channel_ratio(
  x,
  numerator = "eGFP-Peak-Peak",
  denominator = "mCherry",
  name = "GFP_mCherry_ratio"
)

assay(x_ratio)[, "GFP_mCherry_ratio", , , drop = FALSE]
}
}
\seealso{
\code{\link{subtract_control}}
}
