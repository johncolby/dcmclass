% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/train_model.R
\name{train_model}
\alias{train_model}
\title{Train model}
\usage{
train_model(
  dcm_dir,
  gt_labels_path,
  save_path = "model.Rdata",
  n_cv = 5,
  repeats = 5,
  num_fields = NULL,
  fct_fields = NULL,
  char_fields = NULL,
  char_splitters = NULL
)
}
\arguments{
\item{dcm_dir}{String. Directory path to training set, organized like:
accessions/studies/series/dicoms.}

\item{gt_labels_path}{String. File path to ground truth labels file \code{gt_labels.csv}.}

\item{save_path}{String or FALSE. (Optional) File path to save trained model as a \code{.Rdata} file.}

\item{n_cv}{Number of cross validation folds for model training.}

\item{repeats}{Number of repeats for repeated cross validation.}

\item{num_fields}{Char vector. Names of numeric fields. Should at least contain \code{SeriesNumber}.}

\item{fct_fields}{Char vector or \code{FALSE}. (Optional) Names of factor/categorical fields. These will be preprocessed into multiple one-hot i.e. dummy encoded variables.}

\item{char_fields}{Char vector or \code{FALSE}. (Optional) Names of character/string fields. These will be preprocessed with basic text mining methods into a set of variables representing the document term matrix (i.e. word frequency histograms).}

\item{char_splitters}{Char vector. Regular expression splitting patterns to use for \code{char_fields}.}
}
\description{
Train a model to identify modality (e.g axial FLAIR, T1, T1CE) based on
DICOM header data.
}
\details{
Default values:
\itemize{
\item \code{num_fields = c('SeriesNumber', 'SliceThickness', 'RepetitionTime', 'EchoTime',
 'MagneticFieldStrength', 'SpacingBetweenSlices', 'FlipAngle',
 'ImagesInAcquisition', 'Rows', 'Columns')}
\item \code{fct_fields = c('CodeValue', 'MRAcquisitionType', 'ScanningSequence',
 'StationName', 'VariableFlipAngleFlag')}
\item \code{char_fields = c('SeriesDescription', 'ScanOptions')}
\item \code{char_splitters = c('[[:punct:][:space:]]+', '[:space:]+')}
}
}
\seealso{
Other main functions: 
\code{\link{import_studies}()},
\code{\link{predict_headers}()}
}
\concept{main functions}
