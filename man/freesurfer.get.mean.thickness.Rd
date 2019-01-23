\name{freesurfer.get.mean.thickness}
\alias{freesurfer.get.mean.thickness}
\title{Get mean hemisphere thickness}


\description{
This function gets the mean thickness for one hemisphere from FreeSurfer surface data for a list of subjects
}


\usage{
freesurfer.get.mean.thickness(subjects, hemi, freesurfer_subjects_dir = "~/subjects")
}


\arguments{
  \item{subjects}{
a list of FreeSurfer subject IDs
}
  \item{hemi}{
hemisphere, "lh" or "rh"
}
  \item{freesurfer_subjects_dir}{
directory with freesurfer subjects
}
}


\value{
the object returned is a numeric vector with the mean thickness for one hemisphere for all subjects
}


\examples{
# See help(freesurfR) for examples
}
