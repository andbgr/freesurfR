\name{freesurfer.get.parcstats}
\alias{freesurfer.get.parcstats}
\title{Get surface parcellation stats}


\description{
This runs FreeSurfer's aparcstats2table and gives you a dataframe with area, thickness, or volume for a list of subjects
}


\usage{
freesurfer.get.parcstats(subjects, meas, hemi = NULL, freesurfer_subjects_dir = "~/subjects")
}


\arguments{
  \item{subjects}{
a list of FreeSurfer subject IDs
}
  \item{meas}{
measure, "area", "thickness", or "volume"
}
  \item{hemi}{
hemisphere, "lh" or "rh". if not specified, stats for both hemispheres will be returned
}
  \item{freesurfer_subjects_dir}{
directory with freesurfer subjects
}
}


\value{
the object returned is a dataframe with parcellation stats for all subjects
}


\examples{
# See help(freesurfR) for examples
}
