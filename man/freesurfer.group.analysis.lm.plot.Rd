\name{freesurfer.group.analysis.lm.plot}
\alias{freesurfer.group.analysis.lm.plot}
\title{freesurfer.group.analysis.lm.plot}


\description{
Make nice plots of the surface model for a selected vertex
}


\usage{
freesurfer.group.analysis.lm.plot(surface.lm, vertex, title = NULL, conf.int = TRUE, add.mean.to.residuals = FALSE, interactions = TRUE, interactive = TRUE)
}


\arguments{
  \item{surface.lm}{
list() of lm() objects as returned by \code{\link{freesurfer.group.analysis.lm}}
}
  \item{vertex}{
number of the vertex for which to make a plot. use freeview or SUMA to find it.
}
  \item{title}{
optional title for the plot.
}
  \item{conf.int}{
conf.int: logical, whether to plot confidence intervals as shaded areas
}
  \item{add.mean.to.residuals}{
add.mean.to.residuals: logical, whether to add the y mean to the residuals
}
  \item{interactions}{
interactions: logical, whether to plot interactions of continuous with categorial terms using colored groups
}
  \item{interactive}{
interactive: logical, whether to prompt for input before advancing to next plot. set this to false when plotting to a file
}
}


\examples{
# See help(freesurfR) for examples
}


\seealso{
\code{\link{plot.lm.results}} - This is the 'normal' (non-surface) version of this function
}
