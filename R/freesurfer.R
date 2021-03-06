# Tools for group analysis of FreeSurfer data in R
# To be used after processing with recon-all -all -qcache
# TODO: remove 'subjects' from lm calls, simply use data$ID




# https://sites.google.com/a/brain.org.au/ctp/
# Not in CRAN
# require(cortex) # Functions are in mgh_tools.R for now




# Check availability of FreeSurfer 'recon-all -all' preprocessed files
freesurfer.files.available <- function(subjects, freesurfer_subjects_dir = "~/subjects")
{
	subjects.available <- NULL
	message("Checking availability of FreeSurfer 'recon-all -all' preprocessed files...", appendLF = FALSE)
	for (i in seq_along(subjects))
	{
		if (file.exists(sprintf("%s/%s/scripts/recon-all.log", freesurfer_subjects_dir, subjects[i])))
		{
			# TODO: find some R function that can do this instead of a system call
			logfile.tail <- system(sprintf("tail -1 %s/%s/scripts/recon-all.log", freesurfer_subjects_dir, subjects[i]), intern = TRUE)
			logfile.ok <- grepl("finished without error", logfile.tail)
		}
		else
		{
			logfile.ok <- FALSE
		}
		subjects.available[i] <- logfile.ok
		message(".", appendLF = FALSE)
	}
	message("DONE")
	return(subjects.available)
}




# Check availability of FreeSurfer 'recon-all -qcache' preprocessed files (required for QDEC)
# TODO: use full range of meas and smoothing again
freesurfer.qcache.files.available <- function(subjects, freesurfer_subjects_dir = "~/subjects")
{
	hemi <- c("lh", "rh")
	meas <- "thickness"
	smoothing <- "fwhm10"
	template <- "fsaverage"
	
	# Create all possible combinations. This can probably be done nicer
	hemi.expanded <- expand.grid(hemi, meas, smoothing)[[1]]
	meas.expanded <- expand.grid(hemi, meas, smoothing)[[2]]
	smoothing.expanded <- expand.grid(hemi, meas, smoothing)[[3]]
	
	message("Checking availability of FreeSurfer 'recon-all -qcache' preprocessed files...", appendLF = FALSE)
	subjects.available <- NULL
	for (i in seq_along(subjects))
	{
		subjects.available[i] <- all(file.exists(sprintf("%s/%s/surf/%s.%s.%s.%s.mgh", freesurfer_subjects_dir, subjects[i], hemi.expanded, meas.expanded, smoothing.expanded, template)))
		message(".", appendLF = FALSE)
	}
	message("DONE")
	return(subjects.available)
}




# Check availability of FreeSurfer to SUMA exported dataset files
freesurfer.suma.dsets.available <- function(subjects, freesurfer_subjects_dir = "~/subjects")
{
	file <- c("lh.thickness.gii.dset", "rh.thickness.gii.dset", "std.141.lh.thickness.niml.dset", "std.141.rh.thickness.niml.dset", "std.60.lh.thickness.niml.dset", "std.60.rh.thickness.niml.dset")
		
	message("Checking availability of FreeSurfer to SUMA exported dataset files...", appendLF = FALSE)
	subjects.available <- NULL
	for (i in seq_along(subjects))
	{
		subjects.available[i] <- all(file.exists(sprintf("%s/%s/SUMA/%s", freesurfer_subjects_dir, subjects[i], file)))
		message(".", appendLF = FALSE)
	}
	message("DONE")
	return(subjects.available)
}




# Retrieve FreeSurfer segmentation stats created with asegstats2table
freesurfer.get.segstats <- function(subjects, freesurfer_subjects_dir = "~/subjects")
{
	message("Retrieving FreeSurfer segmentation stats...")
	# Create a temporary directory for the csv files
	temp_dir <- tempdir()
	# Call FreeSurfer's asegstats2table that will collect the stats from all subjects
	system2("asegstats2table", args = sprintf("--subjects %s -t %s/segstats.csv --skip -d comma", paste(subjects, collapse = " "), temp_dir), env = sprintf("SUBJECTS_DIR=%s", freesurfer_subjects_dir))
	segstats <- read.csv(sprintf("%s/segstats.csv", temp_dir))
	# Cosmetics for consistency
	colnames(segstats)[1] <- "ID"
	segstats <- segstats[match(subjects, segstats$ID),]
	message("DONE")
	return(segstats)
}




# Retrieve FreeSurfer parcellation stats created with aparcstats2table
# meas: "area", "thickness", or "volume"
# hemi: "lh" or "rh". This is only here for backwards compatibility reasons, you will most likely want to leave it unspecified to get both hemispheres.
# cache: Use existing stats file instead of calling aparcstats2table to generate it
freesurfer.get.parcstats <- function(subjects, meas, hemi = NULL, freesurfer_subjects_dir = "~/subjects")
{
	message("Retrieving FreeSurfer parcellation stats...")
	if(!is.null(hemi))
	{
		file <- sprintf("parcstats_%s_%s.csv", meas, hemi)
		# Create a temporary directory for the csv files
		temp_dir <- tempdir()
		# Call FreeSurfer's aparcstats2table that will collect the stats from all subjects
		system2("aparcstats2table", args = sprintf("--subjects %s --hemi=%s -m %s -t %s/%s --skip -d comma", paste(subjects, collapse = " "), hemi, meas, temp_dir, file), env = sprintf("SUBJECTS_DIR=%s", freesurfer_subjects_dir))
		parcstats <- read.csv(sprintf("%s/%s", temp_dir, file))
		# Cosmetics for consistency. Also, merge() later on will fail if there is no common identifier in lh and rh
		colnames(parcstats)[1] <- "ID"
	}
	else
	{
		parcstats.lh <- freesurfer.get.parcstats(subjects, meas = meas, hemi = "lh", freesurfer_subjects_dir = freesurfer_subjects_dir)
		parcstats.rh <- freesurfer.get.parcstats(subjects, meas = meas, hemi = "rh", freesurfer_subjects_dir = freesurfer_subjects_dir)
		parcstats <- merge(parcstats.lh, parcstats.rh)
	}
	parcstats <- parcstats[match(subjects, parcstats$ID),]
	message("DONE")
	return(parcstats)
}




# Check availability of FreeSurfer segmentation stats created with asegstats2table
# TODO: Remove this function?
freesurfer.segstats.available <- function(subjects, freesurfer_subjects_dir = "~/subjects")
{
	message("Checking availability of FreeSurfer segmentation stats...", appendLF = FALSE)
	subjects.available <- !is.na(suppressMessages(freesurfer.get.segstats(subjects, freesurfer_subjects_dir = freesurfer_subjects_dir))$ID)
	message("DONE")
	return(subjects.available)
}




# Check availability of FreeSurfer parcellation stats created with aparcstats2table
# TODO: Remove this function?
freesurfer.parcstats.available <- function(subjects, freesurfer_subjects_dir = "~/subjects")
{
	message("Checking availability of FreeSurfer parcellation stats...", appendLF = FALSE)
	subjects.available <- rep(TRUE, length(subjects))
	for (meas in c("area", "thickness", "volume"))
	{
		subjects.available <- subjects.available & !is.na(suppressMessages(freesurfer.get.parcstats(subjects, meas = meas, freesurfer_subjects_dir = freesurfer_subjects_dir))$ID)
	}
	message("DONE")
	return(subjects.available)
}




# TODO: Description
# Prerequisite: Subjects have to have been preprocessed with the -qcache option
# meas: "area", "thickness", or "volume"
# hemi: "lh" or "rh"
# template: common template for mapping the surface
# fwhm: smoothing; 0, 5, 10, 15, 20, 25
freesurfer.get.surface.datasets <- function(subjects, meas, hemi, template = "fsaverage", fwhm, freesurfer_subjects_dir = "~/subjects")
{
	# Construct filenames
	surface.dataset.filenames <- paste0(freesurfer_subjects_dir, "/", subjects, "/surf/", hemi, ".", meas, ".fwhm", fwhm, ".", template, ".mgh")
	
	# Get vertex number from first subject
	vertex_number <- length(load.mgh(surface.dataset.filenames[1])$x)
	
	# Preallocate a matrix for the surface datasets
	# Read in the data
	message("Reading surface datasets...", appendLF = FALSE)
	surface.datasets <- matrix(nrow = length(subjects), ncol = vertex_number)
	for(i in seq_along(subjects))
	{
		surface.dataset <- load.mgh(surface.dataset.filenames[i])$x
		surface.datasets[i,] <- surface.dataset
		message(".", appendLF = FALSE)
	}
	message("DONE")
	
	return(surface.datasets)
}




# TODO: Description
freesurfer.dataset.to.suma.dset <- function(dataset, file)
{
	message("Writing file '", file, "'")
	dataset[is.na(dataset)] <- 0
	write.table(dataset, file, col.names = FALSE, row.names = FALSE, quote = FALSE)
}




# TODO: Description
freesurfer.get.mean.thickness <- function(subjects, hemi, freesurfer_subjects_dir = "~/subjects")
{
	message("Retrieving mean thickness...", appendLF = FALSE)
	mean_thickness <- NULL
	for (i in seq_along(subjects))
	{
		subject <- subjects[i]
		file <- paste(freesurfer_subjects_dir, "/", subject, "/stats/", hemi, ".aparc.stats", sep = "")
		if(file.exists(file))
		{
			mean_thickness[i] <- as.numeric(system(paste("grep MeanThickness", file, "| awk -F ', ' '{print $4}'"), intern = TRUE))
		}
		else
		{
			mean_thickness[i] <- NA
		}
		message(".", appendLF = FALSE)
	}
	message("DONE")
	return(mean_thickness)
}




# TODO: Description
freesurfer.get.total.area <- function(subjects, hemi, freesurfer_subjects_dir = "~/subjects")
{
	message("Retrieving total area...", appendLF = FALSE)
	total_area <- NULL
	for (i in seq_along(subjects))
	{
		subject <- subjects[i]
		file <- paste(freesurfer_subjects_dir, "/", subject, "/stats/", hemi, ".aparc.stats", sep = "")
		if(file.exists(file))
		{
			total_area[i] <- as.numeric(system(paste("grep WhiteSurfArea", file, "| awk -F ', ' '{print $4}'"), intern = TRUE))
		}
		else
		{
			total_area[i] <- NA
		}
		message(".", appendLF = FALSE)
	}
	message("DONE")
	return(total_area)
}




# Do the SUMA make spec so we can use SUMA to render datasets on it
# NOTE: for a general freesurfer to suma function, see suma.make.spec.fs() by Franz
# TODO: Be verbose, do testing, etc
freesurfer.prepare.template.for.suma <- function(template = "fsaverage", freesurfer_subjects_dir = "~/subjects")
{
	message("Preparing FreeSurfer template for SUMA...")
	
	if(template == "fsaverage")
	{
		# The default template, "fsaverage", is by default a symlink to /usr/local/freesurfer/subjects/fsaverage
		# This replaces the symlink fsaverage with an actual copy of fsaverage, so it becomes writeable for SUMA
		fsaverage.path <- paste(freesurfer_subjects_dir, "fsaverage", sep = "/")
		fsaverage.actual_path <- system(paste("readlink -f", fsaverage.path), intern = TRUE)
		
		if (fsaverage.path != fsaverage.actual_path)
		{
			system(paste("rm -v", fsaverage.path, "&& cp -av", fsaverage.actual_path, fsaverage.path))
		}
	}
	
	# Run @SUMA_Make_Spec_FS unless SUMA subfolder exists
	template.path <- paste(freesurfer_subjects_dir, template, sep = "/")
	if(!file.exists(paste(template.path, "SUMA", sep = "/")))
	{
		wd <- getwd()
		setwd(template.path)
		system(paste("@SUMA_Make_Spec_FS -no_ld -sid", template))
		setwd(wd)
	}
	
	message("Preparing FreeSurfer template for SUMA...DONE")
}





# TODO: Description
# This is incomplete, see also TODOs for freesurfer.prepare.fsaverage()
freesurfer.launch.suma <- function(template = "fsaverage", hemi = NULL, freesurfer_subjects_dir = "~/subjects")
{
	if (is.null(hemi)) hemi <- "both"
	system(paste0("suma -spec ", freesurfer_subjects_dir, "/", template, "/SUMA/", template, "_", hemi, ".spec -niml &"))
}




# Prepare for general linear model analysis in QDEC
# subjects: List of subject IDs, e.g. 'c("H458_102F", "H458_107M")'
# variables: List of data column names, e.g. 'c("Age", "Gender")'
# data: dataframe describing all subjects, must have and "ID" column
# demean: Whether to center continuous variables around a mean of 0
# freesurfer_subjects_dir: location of FreeSurfer subjects
# TODO: clean up .levels files before writing new ones
# TODO: remove duplicate messages that have moved upstream in the functions
freesurfer.group.analysis.QDEC <- function(data, subjects = NULL, variables = NULL, demean = TRUE, freesurfer_subjects_dir = "~/subjects")
{
	if(is.null(subjects))
		subjects <- data$ID
	
	if(is.null(variables))
		variables <- colnames(data)[colnames(data) != "ID"]
	
	# Check availability of things
	subjects.available.freesurfer <- freesurfer.qcache.files.available(subjects, freesurfer_subjects_dir = freesurfer_subjects_dir)
	# Print message about excluded subjects, and document them in a file
	excluded_subjects <- subjects[!subjects.available.freesurfer]
	if (length(excluded_subjects) != 0)
	{
		message(sprintf("\033[1mWARNING: %i subjects have been excluded for incomplete FreeSurfer data:\033[0m", length(excluded_subjects)))
		print(excluded_subjects)
		write.table(excluded_subjects, file = "subjects.excluded.for.incomplete.freesurfer.data", quote = FALSE, row.names = FALSE, col.names = FALSE)
	}
	
	subjects.available.data <- data.subjects.available(subjects, variables, data)
	# Print message about excluded subjects, and document them in a file
	excluded_subjects <- subjects[!subjects.available.data]
	if (length(excluded_subjects) != 0)
	{
		message(sprintf("\033[1mWARNING: %i subjects have been excluded for incomplete data:\033[0m", length(excluded_subjects)))
		print(excluded_subjects)
		write.table(excluded_subjects, file = "subjects.excluded.for.incomplete.data", quote = FALSE, row.names = FALSE, col.names = FALSE)
	}
	
	subjects <- subjects[subjects.available.freesurfer & subjects.available.data]
	
	data <- data[match(subjects, data$ID),c("ID", variables)]
	# Cosmetics, "fsid" instead of "ID"
	colnames(data)[1] <- "fsid"

	# Determine which variables are factors and write .levels files for QDEC
	message("\033[1mWriting .levels files for QDEC...\033[0m", appendLF = FALSE)
	for (variable in variables)
	{
		if (is.factor(data[,variable]))
		{
			if (length(levels(droplevels(data[,variable]))) == 2)
			{
				write(levels(droplevels(data[,variable])), file=sprintf("%s.levels", variable))
			}
			else
			{
				warning("Factorial variable ", variable, " has more or less than two levels, converting to numeric because QDEC can only handle factors with two levels.", immediate. = TRUE)
				data[,variable] <- as.numeric(data[,variable])
			}
		}
	}
	message("\033[1mDONE\033[0m")
	
	# Demean continuous variables
	if (demean)
	{
		for (variable in variables)
		{
			if (is.numeric(data[,variable]))
			{
				data[,variable] <- data[,variable] - mean(data[,variable])
			}
		}
	}
	
	# Write table to file
	message("\033[1mWriting qdec.table.dat...\033[0m", appendLF = FALSE)
	write.table(data, file="qdec.table.dat", row.names = FALSE, quote = FALSE)
	message("\033[1mDONE\033[0m")
	
	# QDEC fails unless qdec dir in subjects dir already exists
	qdec_dir <- paste0(freesurfer_subjects_dir, "/qdec")
	system(paste("test -d", qdec_dir, "|| mkdir", qdec_dir))
	
	# TODO: Description
	message("\033[1mLaunching QDEC...\033[0m")
	system2("qdec", args = "--table qdec.table.dat", env = sprintf("SUBJECTS_DIR=%s", freesurfer_subjects_dir))
}




# Write a FreeSurfer group descriptor (FSGD) file
# For details see http://surfer.nmr.mgh.harvard.edu/fswiki/FsgdFormat
# subjects: List of subject IDs, e.g. 'c("H458_102F", "H458_107M")'
# variables: List of data column names, e.g. 'c("Age", "Gender")'
# default_variable: must be continuous, and must be one of variables
# title: a descriptive title for display and file naming
# data: dataframe describing all subjects, must have and "ID" column
freesurfer.create.FSGD.file <- function(data, subjects = NULL, variables = NULL, default_variable, title = "Untitled")
{
	if(is.null(subjects))
		subjects <- data$ID
	
	if(is.null(variables))
		variables <- colnames(data)[colnames(data) != "ID"]
	
	out_file <- paste0(title, ".fsgd")
	
	message("Writing '", out_file, "'...", appendLF = FALSE)
	
	# The first line must be "GroupDescriptorFile 1"
	write("GroupDescriptorFile 1", file = out_file)
	
	# This will be used for display.
	write(paste("Title", title), file = out_file, append = TRUE)
		
	# All categorial variables will be combined into one 'Class' variable, containing all possible combinations of categorial variables
	data$Class <- vector("character", length = length(data$ID))
	for (variable in variables)
	{
		if (is.factor(data[,variable]))
		{
			data[,variable] <- droplevels(data[,variable])
			data$Class <- paste0(data$Class, variable, data[,variable])
		}
	}
	data$Class <- as.factor(data$Class)
	
	# Classes are listed in the header
	classes <- levels(data$Class)
	for(class in classes)
	{
		write(paste("Class", class, "plus blue"), file = out_file, append = TRUE)
	}
	
	# Header for continuous variables
	variables.continuous <- !as.logical(lapply(data[,variables], is.factor))
	write(paste("Variables", paste(variables[variables.continuous], collapse = ' ')), file = out_file, append = TRUE)
	
	# Actual entries
	data <- data[match(subjects, data$ID), c("ID", "Class", variables[variables.continuous])]
	write.table(cbind(rep("Input"), data), quote = FALSE, col.names = FALSE, row.names = FALSE, file = out_file, append = TRUE)
	
	# Declaration of default variable
	write(paste("DefaultVariable", default_variable), file = out_file, append = TRUE)
	
	message("DONE")
	return(out_file)
}




# Do vertex-wise GLM (general linear model) group analysis with FreeSurfer surface datasets
# subjects: List of subject IDs, e.g. 'c("H458_102F", "H458_107M")'
# formula: outcome ~ factor1 + factor2
#      OR: outcome ~ factor1 * factor2 (NOTE: This actually implies main terms plus interaction terms, don't do this: factor1 + factor2 + factor1 * factor2)
#          outcome: "area", "thickness", "volume", or "curv"
#          factors: data column names, e.g. "Age", "Gender"
# demean: Whether to center continuous variables around a mean of 0
# data: dataframe describing all subjects, must have and "ID" column
# hemi: "lh" or "rh"
# fwhm: smoothing; 0, 5, 10, 15, 20, 25
# template: common template for mapping the surface. Subjects must have been preprocessed with the -qcache option for this template
# freesurfer_subjects_dir: location of FreeSurfer subjects
# debug.mode: Logical, if TRUE, vertex number is reduced to 1000 for testing purposes
# TODO: both hemispheres at once
freesurfer.group.analysis.lm <- function(data, subjects = NULL, formula, demean = FALSE, hemi, fwhm, template = "fsaverage", freesurfer_subjects_dir = "~/subjects", debug.mode = FALSE)
{
	if(is.null(subjects))
		subjects <- data$ID
	
	# Extract variable names from formula
	# this can probably be done nicer
	variables <- rownames(attributes(terms(formula))$factors)
	outcome <- variables[1]
	factors <- variables[-1]
	
	# Check variables
	if(! outcome %in% c("area", "thickness", "volume", "curv")) stop("'", outcome, "' is not a valid outcome measure. 'area', 'thickness', 'volume', and 'curv' are allowed")
	for(factor in factors)
	{
		if(! factor %in% colnames(data)) stop("'", factor, "' is not a column in data")
	}
	
	# Check availability of things
	subjects.available.freesurfer <- freesurfer.qcache.files.available(subjects, freesurfer_subjects_dir = freesurfer_subjects_dir)
	# Print message about excluded subjects, and document them in a file
	excluded_subjects <- subjects[!subjects.available.freesurfer]
	if (length(excluded_subjects) != 0)
	{
		warning(length(excluded_subjects), " subjects have been excluded for incomplete FreeSurfer data:", immediate. = TRUE)
		print(excluded_subjects)
		write.table(excluded_subjects, file = "subjects.excluded.for.incomplete.freesurfer.data", quote = FALSE, row.names = FALSE, col.names = FALSE)
	}
	
	subjects.available.data <- data.subjects.available(subjects, variables = factors, data = data)
	# Print message about excluded subjects, and document them in a file
	excluded_subjects <- subjects[!subjects.available.data]
	if (length(excluded_subjects) != 0)
	{
		warning(length(excluded_subjects), " subjects have been excluded for incomplete data:", immediate. = TRUE)
		print(excluded_subjects)
		write.table(excluded_subjects, file = "subjects.excluded.for.incomplete.data", quote = FALSE, row.names = FALSE, col.names = FALSE)
	}
	
	subjects <- subjects[subjects.available.freesurfer & subjects.available.data]
	
	# Create variables for the factors
	for(factor in factors)
	{
		assign(factor, data[match(subjects, data$ID),factor])
	}
	
	# Demean continuous variables
	if (demean)
	{
		for(factor in factors)
		{
			if (is.numeric(get(factor)))
			{
				assign(factor, get(factor) - mean(get(factor)))
			}
		}
	}
	
	# Get surface datasets
	surface.datasets <- freesurfer.get.surface.datasets(subjects, meas = outcome, hemi = hemi, template = template, fwhm = fwhm, freesurfer_subjects_dir = freesurfer_subjects_dir)
	vertex_number <- ncol(surface.datasets)
	
	# Reduce vertex number for testing purposes
	# Preallocate a list for the per-vertex GLMs
	# Do the vertex-wise GLM
	if(debug.mode) vertex_number <- 1000
	message("Running vertex-wise GLM:")
	surface.lm <- vector("list", vertex_number)
	progress_bar <- txtProgressBar(max = vertex_number, style = 3)
	for(i in 1:vertex_number)
	{
		assign(outcome, surface.datasets[,i])
		surface.lm[[i]] <- lm(formula, data = environment(), x = TRUE, y = TRUE)
		setTxtProgressBar(progress_bar, i)
	}
	close(progress_bar)
	
	return(surface.lm)
}




# Draw plots of FreeSurfer surface-based glm for a given vertex
# surface.lm: list as returned by freesurfer.group.analysis.lm()
# vertex: index of the vertex for which to make plots
# other arguments: see plot.lm.results()
freesurfer.group.analysis.lm.plot <- function(surface.lm, vertex, title = NULL, conf.int = TRUE, add.mean.to.residuals = FALSE, interactions = TRUE, interactive = TRUE)
{
	vertex.lm <- surface.lm[[vertex]]
	
	if(is.null(title))
		title <- paste("vertex", vertex)
	
	plot.lm.results(vertex.lm, title = title, conf.int = conf.int, add.mean.to.residuals = add.mean.to.residuals, interactions = interactions, interactive = interactive)
}




# Extract t- or p-values from surface glm
# surface.lm: list as returned by freesurfer.group.analysis.lm()
# type.of.value: "t" or "p" or "p_log10"
# correction: Type of multiple comparison correction to apply. "none" or "fdr"
# correction.threshold: Desired theshold for correction
# TODO: This function takes forever. Is there a better way to extract stats than calling summary()?
freesurfer.group.analysis.lm.stats <- function(surface.lm, type.of.value, correction = "none", correction.threshold = 0.05)
{
	vertex_number <- length(surface.lm)
	
	# Extract t values from the models for each term
	terms <- rownames(summary(surface.lm[[1]])$coefficients)
	
	# Create usable variable names (and filenames) from terms:
	# '(Intercept)' -> 'Intercept' (This means that you can't name variables 'Intercept')
	# 'Factor1:Factor2' -> 'Factor1_x_Factor2'
	terms.out_name <- gsub("[()]", "", terms)
	terms.out_name <- gsub(":", "_x_", terms.out_name)
	
	# Preallocate list of per-term stats
	# Extract t values for each term
	stats <- vector("list", length(terms))
	names(stats) <- terms.out_name
	for(i in seq_along(terms))
	{
		term <- terms[i]
		term.out_name <- terms.out_name[i]
		
		message("Extracting ", type.of.value, " values for ", term.out_name, "...", appendLF = FALSE)
		
		# Preallocate a vector for vertex-wise stats of that term
		term.stats <- vector("numeric", vertex_number)
		for(i in 1:vertex_number)
		{
			if(type.of.value == "t")
				term.stats[i] <- summary(surface.lm[[i]])$coefficients[term, "t value"]
			if(type.of.value == "p")
				term.stats[i] <- summary(surface.lm[[i]])$coefficients[term, "Pr(>|t|)"]
			if(type.of.value == "p_log10")
			{
				t <- summary(surface.lm[[i]])$coefficients[term, "t value"]
				p <- summary(surface.lm[[i]])$coefficients[term, "Pr(>|t|)"]
				term.stats[i] <- ifelse(t > 0, -log10(p), log10(p))
			}
		}
		
		stats[[term.out_name]] <- term.stats
		message("DONE")
	}
	
	if(correction == "fdr")
	{
		message("Doing FDR correction...", appendLF = FALSE)
		p <- suppressMessages(freesurfer.group.analysis.lm.stats(surface.lm, type.of.value = "p", correction = "none"))
		for(i in seq_along(terms))
		{
			term <- terms[i]
			term.out_name <- terms.out_name[i]
			
			p.corrected <- p.adjust(p[[term.out_name]], method = "fdr")
			mask <- ifelse(p.corrected < correction.threshold, TRUE, FALSE)
			stats[[term.out_name]][!mask] <- 0
		}
		attr(stats, "fdr.corrected") <- TRUE
		message("DONE")
	}
	
	attr(stats, "type.of.value") <- type.of.value
	return(stats)
}




# Write t or p values from freesurfer glm to mgh files and optionally launch freeview to view them
# surface.stats: list as returned by freesurfer.group.analysis.lm.tvalues() or reesurfer.glm.lm.pvalues()
# hemi: "lh" or "rh"
# launch.suma: Logical, whether to launch SUMA after writing the files
# freesurfer_subjects_dir: location of FreeSurfer subjects
# NOTE: There seems to be a bug that causes freeview to render datasets that contain only zeroes entirely in yellow (instead of transparent). This may be a problem with FDR-corrected datasets where nothing remains.
# TODO: Make a proper launch.freeview() function, like with SUMA
# TODO: both hemispheres at once
# TODO: Add ability to choose template (fsaverage is default)
freesurfer.group.analysis.lm.stats.to.mgh <- function(surface.stats, hemi, template = "fsaverage", freesurfer_subjects_dir = "~/subjects", launch.freeview = TRUE)
{
# 	freesurfer.prepare.template.for.suma(template = template, freesurfer_subjects_dir = freesurfer_subjects_dir)
	
# 	# Check if vertex numbers match
# 	dataset.vertex_number <- length(surface.stats[[1]])
# 	template.vertex_number <- system(paste0("cat ", freesurfer_subjects_dir, "/", template, "/SUMA/", hemi, ".white.asc | sed '/^#/d' | head -1 | cut -d' ' -f1"), intern = TRUE)
# 	if(dataset.vertex_number != template.vertex_number) warning("Dataset vertex number (", dataset.vertex_number, ") and template vertex number (", template.vertex_number, ") are not the same. Something is probably wrong", immediate. = TRUE)
	
	type.of.value <- attr(surface.stats, "type.of.value")
	
	terms <- names(surface.stats)
	# If surface.stats has the 'fdr.corrected' attribute set, add a string to the filename
	corrected_string <- ifelse(attr(surface.stats, "fdr.corrected") == TRUE, ".fdr-corrected", "")
	out_files <- paste0("./", terms, ".", type.of.value, "-values", corrected_string, ".", hemi, ".mgh")
	
	for(i in seq_along(terms))
	{
		term <- terms[i]
		out_file <- out_files[i]
		term.stats <- surface.stats[[term]]
		# The following structure is required for save.mgh to create a valid mgh file
		out_vol <- list()
		out_vol$x <- term.stats
		out_vol$ndim1 <- length(term.stats)
		out_vol$ndim2 <- 1
		out_vol$ndim3 <- 1
		message("Writing file '", out_file, "'")
		save.mgh(out_vol, fname = out_file)
	}
	
	if(launch.freeview)
	{
		system(paste0("freeview --viewport 3d --surface ", freesurfer_subjects_dir, "/", template, "/surf/", hemi, ".inflated", paste0(":overlay=", out_files, collapse = "")))
	}
}




# Write t or p values from freesurfer glm to dset files and optionally launch SUMA to view them
# surface.stats: list as returned by freesurfer.group.analysis.lm.tvalues() or reesurfer.glm.lm.pvalues()
# hemi: "lh" or "rh"
# launch.suma: Logical, whether to launch SUMA after writing the files
# freesurfer_subjects_dir: location of FreeSurfer subjects
# TODO: both hemispheres at once
# TODO: Add ability to choose template (fsaverage is default)
freesurfer.group.analysis.lm.stats.to.suma <- function(surface.stats, hemi, template = "fsaverage", freesurfer_subjects_dir = "~/subjects", launch.suma = TRUE)
{
	freesurfer.prepare.template.for.suma(template = template, freesurfer_subjects_dir = freesurfer_subjects_dir)
	
	# Check if vertex numbers match
	dataset.vertex_number <- length(surface.stats[[1]])
	template.vertex_number <- system(paste0("cat ", freesurfer_subjects_dir, "/", template, "/SUMA/", hemi, ".white.asc | sed '/^#/d' | head -1 | cut -d' ' -f1"), intern = TRUE)
	if(dataset.vertex_number != template.vertex_number) warning("Dataset vertex number (", dataset.vertex_number, ") and template vertex number (", template.vertex_number, ") are not the same. Something is probably wrong", immediate. = TRUE)
	
	type.of.value <- attr(surface.stats, "type.of.value")
	
	terms <- names(surface.stats)
	# If surface.stats has the 'fdr.corrected' attribute set, add a string to the filename
	corrected_string <- ifelse(attr(surface.stats, "fdr.corrected") == TRUE, ".fdr-corrected", "")
	out_files <- paste0("./", terms, ".", type.of.value, "-values", corrected_string, ".", hemi, ".dset")
	
	for(i in seq_along(terms))
	{
		term <- terms[i]
		out_file <- out_files[i]
		term.stats <- surface.stats[[term]]
		freesurfer.dataset.to.suma.dset(term.stats, file = out_file)
	}
	
	# TODO: Not sure if this is working yet
	for(out_file in out_files)
	{
		# Find peaks in dataset and write them to a text file
		system(paste0("SurfExtrema -i ", freesurfer_subjects_dir, "/", template, "/SUMA/lh.smoothwm.asc -input ", out_file, " -table ", out_file, ".extrema.txt -extype ABS -overwrite"))
	}
	
	if(launch.suma)
	{
		freesurfer.launch.suma(template = template, hemi = hemi, freesurfer_subjects_dir = freesurfer_subjects_dir)
		for(out_file in out_files)
		{
			# Load dataset file into suma
			system(paste("DriveSuma -com surf_cont -load_dset", out_file))
			readline(prompt = "Hit any key to see the next dataset:")
		}
	}
}




