\name{freesurfer.group.analysis.lm}
\alias{freesurfer.group.analysis.lm}
\title{freesurfer.group.analysis.lm}


\description{
General Linear Model group analysis of FreeSurfer surface data
}


\usage{
freesurfer.group.analysis.lm(data, subjects = NULL, formula, demean = FALSE, hemi, fwhm, template = "fsaverage", freesurfer_subjects_dir = "~/subjects", debug.mode = FALSE)
}


\arguments{
  \item{data}{
dataframe containing description of FreeSurfer subjects. Must have an 'ID' column.
}
  \item{subjects}{
a list of FreeSurfer subject IDs. If not specified, all subjects in data will be used.
}
  \item{formula}{
formula as accepted by lm(), with either "thickness", "area", or "volume" as dependent variable
}
  \item{demean}{
whether to center variables around a mean of zero. See also http://mumford.fmripower.org/mean_centering/
}
  \item{hemi}{
hemisphere, "lh" or "rh"
}
  \item{fwhm}{
Smoothing kernel size, must be 0, 5, 10, 15, 20, or 25
}
  \item{template}{
FreeSurfer template to be used for mapping
}
  \item{freesurfer_subjects_dir}{
directory with freesurfer subjects
}
  \item{debug.mode}{
limit analysis to the first 1000 vertices, which is useful for testing the workflow
}
}


\value{
the object returned is a list() of lm() objects, the lenght of which is the number of vertices
}


\examples{
# See help(freesurfR) for examples
}


\seealso{
\code{\link{freesurfer.group.analysis.QDEC}} - an alternative to this
}
