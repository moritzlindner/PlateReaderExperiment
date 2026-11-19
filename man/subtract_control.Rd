\name{subtract_control}
\alias{subtract_control}
\title{Subtract the mean of a control condition}
\usage{
subtract_control(x, control, by = "group", na.rm = TRUE)
}
\arguments{
  \item{x}{A \code{PlateReaderExperiment}.}
  \item{control}{Value or values in the design column specified by \code{by} identifying control samples.}
  \item{by}{Design column identifying the control condition.}
  \item{na.rm}{Logical; ignore missing control measurements when calculating the control mean.}
}
\description{
Calculates the mean of all matching control samples separately for every channel x time x position combination and subtracts that mean from every sample.
}
\details{
If raw multi-position data are supplied, control subtraction is position-specific. To subtract a baseline based on one mean signal per well, call \code{average_positions()} first.

The most recently used control array is stored in \code{metadata(x)$last_control_values}, and the transformation is appended to the processing history. If all control measurements at a channel/time/position combination are missing, corrected values at that combination are \code{NA}.
}
\value{A control-subtracted \code{PlateReaderExperiment}.}
\examples{
\dontrun{
x_corrected <- subtract_control(x, control = "Control", by = "group")
x_corrected <- x |>
  average_positions() |>
  subtract_control(control = "Control", by = "group")
}
}
\seealso{\code{\link{average_positions}}, \code{\link{average_by_group}}}
