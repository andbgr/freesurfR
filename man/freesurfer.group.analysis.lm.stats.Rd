\name{freesurfer.group.analysis.lm.stats}
\alias{freesurfer.group.analysis.lm.stats}
\title{freesurfer.group.analysis.lm.stats}


\description{
Extract t values or p values from a surface linear model. Optionally do FDR correction.
}


\usage{
freesurfer.group.analysis.lm.stats(surface.lm, type.of.value, correction = "none", correction.threshold = 0.05)
}


\arguments{
  \item{surface.lm}{
list() of lm() objects as returned by \code{\link{freesurfer.group.analysis.lm}}
}
  \item{type.of.value}{
"t" for t-values, "p" for p-values, or "p_log10" for log10-transformed p-values as used in QDEC
}
  \item{correction}{
Type of multiple comparison correction to apply. "none" or "fdr"
}
  \item{correction.threshold}{
Desired theshold for correction
}
}


\value{
the object returned is a list() of vectors with stats for each term of the model. the length of the list is the number of terms, the lengh of one vector is the number of vertices.
}


\examples{
# See help(freesurfR) for examples
}
