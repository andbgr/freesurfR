\name{freesurfR-package}
\alias{freesurfR-package}
\alias{freesurfR}
\docType{package}
\title{FreeSurfer Group Analysis Tools}


\description{
This package contains tools for doing group analysis of FreeSurfer (https://surfer.nmr.mgh.harvard.edu/) surface data using the general linear model in R (lm).
Results can be rendered in FreeSurfer's freeview or AFNI's SUMA. Plots for selected vertices can be rendered in R with ggplot2.
}


\examples{
\dontrun{
# These examples were taken from https://surfer.nmr.mgh.harvard.edu/fswiki/FsTutorial/QdecGroupAnalysis_freeview
# The following presupposes that the subjects have been processed with 'recon-all -all -qcache'
ID <- c("011121_vc8048", "021121_62313-2", "010607_vc7017", "021121_vc10557", "020718_62545", "020322_vc8817")
Gender <- factor(c("Female", "Female", "Female", "Male", "Male", "Male"))
Age <- c(70, 71, 73, 75, 76, 77)
Diagnosis <- factor(c("Demented", "Demented", "Nondemented", "Demented", "Demented", "Nondemented"))
data <- data.frame(ID, Age, Gender, Diagnosis)


### Group analysis with lm ###
# Run the model. This takes some time and memory.
lm <- freesurfer.group.analysis.lm(data = data, formula = thickness ~ Age + Gender + Diagnosis, hemi = "lh", fwhm = 10, template = "fsaverage", freesurfer_subjects_dir = "~/subjects")

# Extract t values from the model, do FDR correction
t <- freesurfer.group.analysis.lm.stats(lm, type.of.value = "t", correction="fdr")

# Write the t values to mgh files and open them in FreeSurfer's freeview
freesurfer.group.analysis.lm.stats.to.mgh(t, hemi = "lh", template = "fsaverage", freesurfer_subjects_dir = "~/subjects", launch.freeview = TRUE)

# Write the t values to .dset ASCII files and open them in AFNI's SUMA
freesurfer.group.analysis.lm.stats.to.suma(t, hemi = "lh", template = "fsaverage", freesurfer_subjects_dir = "~/subjects", launch.suma = TRUE)

# Make nice plots for vertex #1000
freesurfer.group.analysis.lm.plot(lm, vertex = 1000, interactive = TRUE)


### Group analysis with QDEC ###
# Call QDEC
freesurfer.group.analysis.QDEC(data = data, freesurfer_subjects_dir = "~/subjects")


### Extract stats ###
# Get segmentation stats
segstats <- freesurfer.get.segstats(subjects = data$ID, freesurfer_subjects_dir = "~/subjects")
}
}


\seealso{
\code{\link{cortex}} - Power Analysis for FreeSurfer
}
