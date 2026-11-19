\name{plot_boxplot}
\alias{plot_boxplot}
\title{Boxplot with sample and optional within-well position dots}
\usage{
plot_boxplot(
  x,
  group_by = "group",
  channels = NULL,
  times = NULL,
  colour_by = "sample_id",
  shape_by = NULL,
  show_positions = FALSE,
  box_alpha = 0.35,
  box_width = 0.65,
  point_size = 2.2
)
}
\arguments{
  \item{x}{A \code{PlateReaderExperiment}.}
  \item{group_by}{Design column displayed on the x axis.}
  \item{channels, times}{Optional channel/time identifiers to plot.}
  \item{colour_by}{Column in the long-format data used to colour dots. Commonly \code{"sample_id"} or \code{"biological_sample"}.}
  \item{shape_by}{Optional column used for dot shape. With \code{show_positions = TRUE}, defaults to \code{"position_id"}.}
  \item{show_positions}{Logical; show raw within-well positions as dots. The boxplot itself always uses position-averaged sample/well values.}
  \item{box_alpha}{Transparency of the boxplot layer.}
  \item{box_width}{Width of boxplot boxes.}
  \item{point_size}{Size of the superimposed dots.}
}
\description{
Creates grouped boxplots with individual sample/well measurements superimposed. Optional raw position dots expose within-well spatial variation without using those positions to calculate the boxplot distribution.
}
\details{
Quartiles and whiskers are always calculated from one position-averaged value per sample/well, avoiding pseudoreplication from several spatial reads within one well.

With \code{show_positions = FALSE}, the jittered dots are the same well/sample means used for the boxplot. With \code{show_positions = TRUE}, the boxplot is unchanged but the raw position-level measurements are shown as dots. If \code{shape_by} is omitted in that mode, \code{position_id} is used automatically.

Ordinary boxplot outlier symbols are suppressed because all individual observations are already displayed. Multiple channels and/or times are faceted automatically.
}
\value{A \code{ggplot} object. Requires the suggested package \pkg{ggplot2}.}
\examples{
\dontrun{
plot_boxplot(
  x,
  group_by = "group",
  channels = "eGFP-Peak-Peak",
  times = "T002",
  colour_by = "biological_sample"
)
plot_boxplot(
  x,
  group_by = "group",
  channels = "eGFP-Peak-Peak",
  times = "T002",
  colour_by = "biological_sample",
  shape_by = "position_id",
  show_positions = TRUE
)
}
}
\seealso{\code{\link{plot_violin}}, \code{\link{plot_timeseries}}}
