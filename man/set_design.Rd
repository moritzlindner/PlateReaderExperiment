\name{set_design}
\alias{set_design}
\title{Add or replace experimental-design annotations}
\usage{
set_design(x, value, by = "sample_id")
}
\arguments{
  \item{x}{A \code{PlateReaderExperiment}.}
  \item{value}{Data frame containing the key column and one or more design columns to add or replace.}
  \item{by}{Key column present in both \code{sampleData(x)} and \code{value}. Common choices are \code{"sample_id"} and \code{"well"}.}
}
\description{
Matches a design table to samples using an explicit key and adds or replaces the supplied design columns.
}
\details{
Matching is \strong{key-based, not row-position-based}. With \code{by = "well"}, for example, \code{value$well == "A2"} is matched to the row whose \code{sampleData(x)$well} is \code{"A2"}, regardless of the row order in \code{value}.

The key must be unique in \code{value}, and every sample in \code{x} must have exactly one matching row. Existing design columns with the same names are replaced; other existing design columns are retained.
}
\value{The modified \code{PlateReaderExperiment}.}
\examples{
\dontrun{
layout <- data.frame(
  well = c("A1", "A2", "B1", "B2"),
  group = c("Control", "Control", "Drug", "Drug"),
  biological_sample = c("C1", "C2", "D1", "D2")
)
x <- set_design(x, layout, by = "well")
design(x)
}
}
\seealso{\code{\link{PlateReaderExperiment-accessors}}, \code{\link{subset.PlateReaderExperiment}}}
