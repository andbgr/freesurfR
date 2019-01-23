\name{freesurfer.group.analysis.lm.stats.to.suma}
\alias{freesurfer.group.analysis.lm.stats.to.suma}
\title{freesurfer.group.analysis.lm.stats.to.suma}


\description{
Write t or p values from freesurfer glm to dset files and optionally launch SUMA to view them
}


\usage{
freesurfer.group.analysis.lm.stats.to.suma(surface.stats, hemi, template = "fsaverage", freesurfer_subjects_dir = "~/subjects", launch.suma = TRUE)
}


\arguments{
  \item{surface.stats}{
surface stats as returned by \code{\link{freesurfer.group.analysis.lm.stats}}
}
  \item{hemi}{
hemisphere, "lh" or "rh"
}
  \item{template}{
FreeSurfer template to be used for rendering
}
  \item{freesurfer_subjects_dir}{
directory with freesurfer subjects
}
  \item{launch.suma}{
whether to directly launch SUMA
}
}


\examples{
# See help(freesurfR) for examples
}


\seealso{
\code{\link{freesurfer.group.analysis.lm.stats.to.mgh}}
}
