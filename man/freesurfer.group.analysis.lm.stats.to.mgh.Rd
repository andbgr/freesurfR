\name{freesurfer.group.analysis.lm.stats.to.mgh}
\alias{freesurfer.group.analysis.lm.stats.to.mgh}
\title{freesurfer.group.analysis.lm.stats.to.mgh}



\description{
Write t or p values from freesurfer glm to mgh files and optionally launch freeview to view them
NOTE: There seems to be a bug that causes freeview to render datasets that contain only zeroes entirely in yellow (instead of transparent). This may be a problem with FDR-corrected datasets where nothing remains.
}


\usage{
freesurfer.group.analysis.lm.stats.to.mgh(surface.stats, hemi, template = "fsaverage", freesurfer_subjects_dir = "~/subjects", launch.freeview = TRUE)
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
  \item{launch.freeview}{
whether to directly launch freeview
}
}


\examples{
# See help(freesurfR) for examples
}


\seealso{
\code{\link{freesurfer.group.analysis.lm.stats.to.mgh}}
}
