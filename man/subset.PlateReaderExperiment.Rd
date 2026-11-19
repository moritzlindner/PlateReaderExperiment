\name{subset.PlateReaderExperiment}
\alias{subset.PlateReaderExperiment}
\alias{[,PlateReaderExperiment-method}
\title{Subset a PlateReaderExperiment}
\usage{
\method{[}{PlateReaderExperiment}(x, i, j, ..., drop = FALSE)

\method{subset}{PlateReaderExperiment}(
  x,
  samples = NULL,
  channels = NULL,
  times = NULL,
  positions = NULL,
  group = NULL,
  ...
)
}
\arguments{
  \item{x}{A \code{PlateReaderExperiment}.}
  \item{i}{Sample selector for array-style subsetting.}
  \item{j}{Channel selector for array-style subsetting.}
  \item{drop}{Ignored conceptually; package subsetting never drops assay dimensions.}
  \item{samples, channels, times, positions}{Optional identifiers, numeric indices, or logical selectors for the corresponding dimensions.}
  \item{group}{Optional value or values from \code{design(x)$group}.}
  \item{...}{For \code{[}, the third and fourth dimension selectors (time and position). For \code{subset()}, additional named filters for arbitrary design columns, e.g. \code{treatment = "Drug"}.}
}
\description{
Subsets the assay while preserving the canonical four-dimensional structure and synchronously subsetting all associated descriptor, design, and acquisition metadata.
}
\details{
Array-style dimension order is always sample, channel, time, position:
\preformatted{x[samples, channels, times, positions]}
Dimensions are never dropped when a single element is selected.

\code{subset()} provides a more readable interface and can also filter by experimental-design values.
}
\value{A subsetted \code{PlateReaderExperiment}.}
\examples{
\dontrun{
x2 <- x[, "eGFP-Peak-Peak", c("T001", "T002"), ]
x3 <- subset(x, group = "Drug", channels = "eGFP-Peak-Peak")
x4 <- subset(x, genotype = "WT", treatment = "Drug")
}
}
\seealso{\code{\link{set_design}}, \code{\link{as.data.frame-PlateReaderExperiment}}}
