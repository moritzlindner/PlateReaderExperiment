\name{plot_layout}
\alias{plot_layout}
\title{Plot the physical plate layout}
\usage{
plot_layout(
  x,
  fill_by = "group",
  label_by = c("well", "group"),
  show_empty = TRUE,
  plate_format = NULL,
  label_sep = "\\n",
  text_size = 3.5,
  title = NULL
)
}
\arguments{
\item{x}{A \code{PlateReaderExperiment}.}

\item{fill_by}{Optional single column name from \code{sampleData(x)} or
\code{design(x)} used to fill measured wells. The default is \code{"group"}.
Use \code{NULL} for an uncoloured layout.}

\item{label_by}{Character vector of column names from \code{sampleData(x)} or
\code{design(x)} printed inside measured wells. The default is
\code{c("well", "group")}. Missing columns and missing values are omitted from
the label.}

\item{show_empty}{Logical. If \code{TRUE}, show the complete inferred plate
grid and leave unmeasured wells empty. If \code{FALSE}, only measured wells
are drawn.}

\item{plate_format}{Optional plate format used when \code{show_empty = TRUE}.
Supply one of \code{6}, \code{12}, \code{24}, \code{48}, \code{96},
\code{384}, or \code{1536}. If \code{NULL}, the smallest standard format
containing all observed well coordinates is inferred separately for each
plate.}

\item{label_sep}{Separator placed between multiple label fields.}

\item{text_size}{Text size for labels inside wells.}

\item{title}{Optional plot title.}
}
\value{
A \code{ggplot2} object.
}
\description{
Display the wells of a \code{PlateReaderExperiment} in their physical plate
arrangement. Well fill can represent a design or sample variable, and one or
more variables can be printed inside each measured well.
}
\details{
The function uses \code{plate_row_index} and \code{plate_column} from
\code{sampleData(x)} when available. If those fields are absent, coordinates
are derived from standard well identifiers such as \code{"A1"} or
\code{"H12"}.
}
\examples{
\dontrun{
x <- read.tecan("results.xlsx")

plot_layout(x)

plot_layout(
  x,
  fill_by = "treatment",
  label_by = c("well", "biological_sample", "treatment")
)

plot_layout(x, fill_by = NULL, label_by = "well", plate_format = 96)
}
}
