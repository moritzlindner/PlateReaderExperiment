\name{read.tecan}
\alias{read.tecan}
\title{Read Tecan SparkControl Excel results}
\usage{
read.tecan(
  path,
  sheet = "Result sheet",
  pattern = "\\\\.(xlsx|xlsm)$",
  recursive = FALSE,
  order_by = c("start_time", "file"),
  tz = Sys.timezone()
)
}
\arguments{
  \item{path}{Path to one Excel workbook or to a folder containing compatible sequential single-time workbooks.}
  \item{sheet}{Expected result worksheet name. If a workbook has exactly one worksheet, that sheet is accepted automatically.}
  \item{pattern}{Regular expression selecting workbooks when \code{path} is a folder.}
  \item{recursive}{Logical; search subdirectories when importing a folder.}
  \item{order_by}{For folders of single-time exports, order time points by acquisition \code{"start_time"} or by \code{"file"} name.}
  \item{tz}{Time zone used for timestamps that do not contain an explicit zone.}
}
\description{
Automatically detects a supported Tecan SparkControl export layout and normalizes it to a four-dimensional \code{PlateReaderExperiment}.
}
\details{
Three export families are currently supported.

\strong{Plate-matrix exports:} each channel contains a plate-shaped well matrix. If a folder is supplied, each compatible workbook becomes one logical time point. The first workbook defines static plate/channel structure, and later files must match. The normalized assay is well/sample x channel x workbook/time x one position.

\strong{Single-time multi-read exports:} each channel contains one wide matrix with wells in columns and within-well read positions in rows. The normalized assay is well/sample x channel x workbook/time x position. A compatible folder of such workbooks is interpreted as sequential time points.

\strong{Kinetic multi-read exports:} kinetic cycles and multiple positions per well occur in one workbook. The normalized assay is well/sample x channel x kinetic time x position. Incomplete or interrupted acquisitions are retained and missing measurements are represented as \code{NA}.

A folder containing multiple kinetic workbooks is rejected because such files could represent replicate plates rather than consecutive time points.

The importer is strict about structural mismatches. It also includes a best-effort temporary repair path for some malformed SparkControl OOXML ZIP archives whose worksheet contents remain readable.
}
\value{A \code{PlateReaderExperiment}.}
\examples{
\dontrun{
x <- read.tecan("kinetic.xlsx", tz = "Europe/Berlin")
x <- read.tecan("folder/of/sequential_exports", tz = "Europe/Berlin")
}
}
\seealso{\code{\link{PlateReaderExperiment-class}}, \code{\link{set_design}}}
