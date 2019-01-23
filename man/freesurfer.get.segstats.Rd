\name{freesurfer.get.segstats}
\alias{freesurfer.get.segstats}
\title{Get subcortical segmentation stats}


\description{
This runs FreeSurfer's asegstats2table and gives you a dataframe with subcortical segmentation volumes for a list of subjects
}


\usage{
freesurfer.get.segstats(subjects, freesurfer_subjects_dir = "~/subjects")
}


\arguments{
  \item{subjects}{
a list of FreeSurfer subject IDs
}
  \item{freesurfer_subjects_dir}{
directory with freesurfer subjects
}
}


\value{
the object returned is a dataframe with segmentation stats for all subjects
}


\examples{
# See help(freesurfR) for examples
}
