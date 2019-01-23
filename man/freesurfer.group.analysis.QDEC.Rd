\name{freesurfer.group.analysis.QDEC}
\alias{freesurfer.group.analysis.QDEC}
\title{freesurfer.group.analysis.QDEC}


\description{
General Linear Model group analysis using FreeSurfer's QDEC
}


\usage{
freesurfer.group.analysis.QDEC(data, subjects = NULL, variables = NULL, demean = TRUE, freesurfer_subjects_dir = "~/subjects")
}


\arguments{
  \item{data}{
dataframe containing description of FreeSurfer subjects. Must have an 'ID' column.
}
  \item{subjects}{
a list of FreeSurfer subject IDs. If not specified, all subjects in data will be used.
}
  \item{variables}{
a list of variables, i.e. column names in data to be included. If not specified, all variables in data will be used.
}
  \item{demean}{
whether to center variables around a mean of zero. See also http://mumford.fmripower.org/mean_centering/
}
  \item{freesurfer_subjects_dir}{
directory with freesurfer subjects
}
}


\examples{
# See help(freesurfR) for examples
}


\seealso{
\code{\link{freesurfer.group.analysis.lm}} - an alternative to this
}
