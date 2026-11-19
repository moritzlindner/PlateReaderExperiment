\name{PlateReaderExperiment-accessors}
\alias{assay}
\alias{sampleData}
\alias{channelData}
\alias{timeData}
\alias{positionData}
\alias{design}
\alias{design<-}
\alias{acquisitionData}
\alias{metadata}
\title{Access PlateReaderExperiment assay and metadata}
\usage{
assay(x)
sampleData(x)
channelData(x)
timeData(x)
positionData(x)
design(x)
design(x) <- value
acquisitionData(x)
metadata(x)
}
\arguments{
  \item{x}{A \code{PlateReaderExperiment}.}
  \item{value}{For \code{design<-}, a complete design data frame containing exactly one unique row for every \code{sample_id}.}
}
\description{
Accessor functions return the canonical assay and its associated descriptor, design, acquisition, or experiment metadata.
}
\details{
\code{assay(x)} returns the numeric sample x channel x time x position array.

\code{sampleData(x)}, \code{channelData(x)}, \code{timeData(x)}, and \code{positionData(x)} describe the corresponding dimensions.

\code{design(x)} contains experimental annotations for each sample. Complete design replacement with \code{design(x) <- value} is strict: every sample must occur exactly once. For adding or replacing selected design columns by a key such as physical well, prefer \code{set_design()}.

\code{acquisitionData(x)} contains metadata specific to sample x channel x time acquisitions. \code{metadata(x)} returns free-form experiment and processing metadata.
}
\value{The requested array, data frame, or metadata list. The replacement form returns the modified object.}
\seealso{\code{\link{set_design}}, \code{\link{PlateReaderExperiment-class}}}
