\name{freesurfer.get.total.area}
\alias{freesurfer.get.total.area}
\title{Get total hemisphere area}


\description{
This function gets the total area for one hemisphere from FreeSurfer surface data for a list of subjects
}


\usage{
freesurfer.get.total.area(subjects, hemi, freesurfer_subjects_dir = "~/subjects")
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
the object returned is a numeric vector with the total area for one hemisphere for all subjects
}


\examples{
# See help(freesurfR) for examples
}
