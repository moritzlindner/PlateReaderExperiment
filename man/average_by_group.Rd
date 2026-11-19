\name{average_by_group}
\alias{average_by_group}
\title{Average samples by experimental-design groups}
\usage{
average_by_group(x, by = "group", na.rm = TRUE)
}
\arguments{
  \item{x}{A \code{PlateReaderExperiment}.}
  \item{by}{One or more column names in \code{design(x)} defining the groups.}
  \item{na.rm}{Logical; ignore missing sample measurements when calculating means.}
}
\description{
Averages the sample dimension for each unique combination of one or more experimental-design factors, independently at every channel x time x position combination.
}
\details{
Grouping columns may not contain missing values. The returned object remains a \code{PlateReaderExperiment}; its first dimension represents aggregated groups and \code{sampleData()} records \code{n_source_samples}. Acquisition metadata are summarized across source samples where possible.
}
\value{A grouped, sample-averaged \code{PlateReaderExperiment}.}
\examples{
\dontrun{
x_group <- average_by_group(x, by = "group")
x_gt <- average_by_group(x, by = c("genotype", "treatment"))
}
}
\seealso{\code{\link{average_positions}}, \code{\link{subtract_control}}}
