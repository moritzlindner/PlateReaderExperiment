\name{plot_violin}
\alias{plot_violin}
\title{Violin plot with sample and optional within-well position dots}
\usage{
plot_violin(
  x,
  group_by = "group",
  channels = NULL,
  times = NULL,
  colour_by = "sample_id",
  shape_by = NULL,
  show_positions = FALSE,
  violin_alpha = 0.35,
  point_size = 2.2
)
}
\arguments{
  \item{x}{A \code{PlateReaderExperiment}.}
  \item{group_by}{Design column shown on the x axis.}
  \item{channels, times}{Optional channel/time identifiers to plot.}
  \item{colour_by}{Column in the long-format data used to colour dots. Commonly \code{"sample_id"} or \code{"biological_sample"}.}
  \item{shape_by}{Optional column used for dot shape. With \code{show_positions = TRUE}, defaults to \code{"position_id"}.}
  \item{show_positions}{Logical; overlay raw within-well position measurements instead of only sample/well means.}
  \item{violin_alpha}{Transparency of the violin layer.}
  \item{point_size}{Size of the superimposed dots.}
}
\description{
Plots group distributions as violins with individual measurements superimposed while preserving the well/sample as the experimental unit for the distribution itself.
}
\details{
The violin distribution is always calculated from one position-averaged value per sample/well. This avoids treating multiple positions within one well as independent experimental replicates.

When \code{show_positions = FALSE}, the dots are those position-averaged sample/well values. When \code{show_positions = TRUE}, the violin is unchanged but raw within-well position values are shown as dots. If \code{shape_by} is omitted in that mode, position ID is mapped to shape automatically.

Multiple channels and/or times are faceted automatically.
}
\value{A \code{ggplot} object. Requires the suggested package \pkg{ggplot2}.}
\examples{
\dontrun{
plot_violin(
  x,
  group_by = "group",
  channels = "eGFP-Peak-Peak",
  times = "T002",
  colour_by = "biological_sample"
)
plot_violin(
  x,
  group_by = "group",
  channels = "eGFP-Peak-Peak",
  times = "T002",
  colour_by = "biological_sample",
  show_positions = TRUE
)
}
}
\seealso{\code{\link{plot_boxplot}}, \code{\link{plot_timeseries}}}
