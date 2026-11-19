\name{merge_experiments}
\alias{merge_experiments}
\title{Merge structurally compatible plate-reader experiments}
\usage{
merge_experiments(..., experiment_ids = NULL, axis = c("auto", "sample", "time"))
}
\arguments{
  \item{...}{Two or more \code{PlateReaderExperiment} objects, or one list of them.}
  \item{experiment_ids}{Optional unique, non-empty character labels, one per experiment. If omitted, named inputs supply their labels and unnamed inputs use \code{experiment1}, \code{experiment2}, etc.}
  \item{axis}{Choose \code{"auto"} (default), \code{"time"}, or \code{"sample"}. Automatic mode joins on time when layout and design match and recorded times are distinct; otherwise it joins samples.}
}
\description{
Combine compatible experiments along the time or sample dimension without averaging their readings.
}
\details{
For a sample-axis merge, channel IDs and logical time IDs must match (their order may differ). Within-well read positions must have the same count and compatible physical descriptors; if their IDs differ they are aligned by position order. Channel settings, cycles and nominal time points must be compatible. Different observed time points and time-specific source filenames are retained per acquisition. Other columns of \code{sampleData()}, \code{design()} and \code{acquisitionData()} are retained, with missing columns filled with \code{NA}.

For a time-axis merge, matching wells and design are retained, and logical time IDs are regenerated; original time IDs and input labels are stored in \code{timeData()}. When all recorded acquisition timestamps are present, observed times are aligned to a shared origin.

For a sample-axis merge, sample IDs and plate IDs are qualified by experiment label to avoid collisions; the original IDs remain in \code{source_sample_id} and \code{source_plate_id}. The \code{experiment_id} column identifies the input in both \code{sampleData()} and \code{design()}. The original well names remain unchanged. Run metadata, full time tables and the sample, plate and read-position ID maps are available under \code{metadata(result)$experiments}. If an input already contains a provenance column, it is retained under a \code{.input} suffixed name.
}
\value{A valid \code{PlateReaderExperiment} with all input wells and readings.}
\examples{
\dontrun{
plate1 <- read.tecan("plate1.xlsx")
plate2 <- read.tecan("plate2.xlsx")
combined <- merge_experiments(first = plate1, second = plate2)
table(design(combined)$experiment_id)
}
}
\seealso{\code{\link{read.tecan}}, \code{\link{set_design}}, \code{\link{average_by_group}}}
