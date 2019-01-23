# tools that are required by freesurfer.R but are not FreeSurfer-specific




# Check availability of subjects plus specified respective variables in data
# This assumes that bogus entries are already NA
# variables: List of column names, e.g. 'c("Age", "Gender")'
# data: The usual data frame, must have an ID column
data.subjects.available <- function(subjects, variables, data)
{
	subjects.available <- !is.na(match(subjects, data$ID))
	for (variable in variables)
	{
		subjects.available <- subjects.available & !is.na(data[match(subjects, data$ID), variable])
	}
	return(subjects.available)
}




# For each term of a linear model, plot the outcome, corrected for all other terms, against that term
# lm: A linear model as retuned by lm()
# title: character, title for display
# conf.int: logical, whether to plot confidence intervals as shaded areas
# add.mean.to.residuals: logical, whether to add the y mean to the residuals
# interactive: whether to prompt for input before advancing to next plot. set this to false when plotting to a file
# Do not call this function directly, call plot.lm.results() instead
# TODO: integrade add.mean.to.residuals better
plot.lm.results.ggplot <- function(lm, title = NULL, conf.int = TRUE, add.mean.to.residuals = FALSE, interactive = TRUE)
{
	# TODO: Change this, there should be an easier way
	outcome <- rownames(attributes(lm$terms)$factors)[1]
	terms   <- rownames(attributes(lm$terms)$factors)[-1]
	
	# One plot for each term
	for(term in terms)
	{
		other_terms <- terms[terms != term]
		plot.data <- lm$model
		if(length(other_terms) == 0)
		{
			plot <- ggplot(plot.data, aes_string(x = term, y = outcome))
		}
		else
		{
			# Construct a formula that contains all the terms of the linear model, except for the term that we want to plot against
			plot.formula <- formula(paste(outcome, paste(other_terms, collapse=" + "), sep=" ~ "))
			plot.lm <- lm(plot.formula, data = plot.data)
			plot.data$residuals <- plot.lm$residuals
			if(add.mean.to.residuals)
				plot.data$residuals <- plot.data$residuals + mean(plot.data[,outcome])
			plot <- ggplot(plot.data, aes_string(x = term, y = "residuals"))
			plot <- plot + labs(y = paste(outcome, ifelse(add.mean.to.residuals,"(mean + residuals)" , "(residuals)")))
		}
		if(is.factor(plot.data[,term]))
		{
			plot <- plot + geom_point(alpha = 0.25, position = position_jitter(width = 0.1, height = 0))
			# OK, this is a bit of a workaround: geom_crossbar has lines everywhere, we only want a line for the mean,
			# not for the bounding box. so we make two crossbars, one with invisible lines for the shaded box, and one
			# with visible lines for the mean, and the upper and lower bounds of the box are collapsed on to the mean
			# via yim and ymax. The small remaining issue of the line exceeding the box by half a linewidth on either
			# side (to be expected) is fixed by setting size extremely small and fatten extremely large - the fattened
			# line stays within the box and the normal line that stands out is so thin it's practically invisible.
			plot <- plot + stat_summary(fun.data = mean_cl_normal, geom = "crossbar" , linetype = 0, width = 0.25, fill = "black", alpha = ifelse(conf.int, 0.2, 0))
			plot <- plot + stat_summary(fun.y = mean, fun.ymin = mean, fun.ymax = mean, geom = "crossbar" , size = 0.000000001, fatten = 500000000, width = 0.25)
		}
		else
		{
			plot <- plot + geom_point(alpha = 0.25)
			plot <- plot + geom_smooth(method = "lm", size = 0.5, color = "black", se = conf.int, alpha = 0.4)
		}
		plot <- plot + labs(title = title)
		print(plot)
		
		if(interactive)
			readline(prompt = "Hit any key to see the next plot:")
	}
}




# For each combination of a categorial and continuous term of a linear model, plot the outcome, corrected for all other 
# terms, against the continuous term on x and the categorial term as color
# lm: A linear model as retuned by lm()
# title: character, title for display
# conf.int: logical, whether to plot confidence intervals as shaded areas
# add.mean.to.residuals: logical, whether to add the y mean to the residuals
# interactive: whether to prompt for input before advancing to next plot. set this to false when plotting to a file
# Do not call this function directly, call plot.lm.results() instead
# TODO: integrade add.mean.to.residuals better
plot.lm.results.ggplot.interactions <- function(lm, title = NULL, conf.int = TRUE, add.mean.to.residuals = FALSE, interactive = TRUE)
{
	# TODO: Change this, there should be an easier way
	outcome <- rownames(attributes(lm$terms)$factors)[1]
	terms   <- rownames(attributes(lm$terms)$factors)[-1]
	plot.data <- lm$model
	terms.categorial <- sapply(plot.data[terms], is.factor)
	categorial_terms <- terms[terms.categorial]
	continuous_terms <- terms[!terms.categorial]
	
	# One plot for each combination of a categorial and continuous term
	for(categorial_term in categorial_terms)
	{
		for(continuous_term in continuous_terms)
		{
			other_terms <- terms[!terms %in% c(categorial_term, continuous_term)]
			plot.data <- lm$model
			if(length(other_terms) == 0)
			{
				plot <- ggplot(plot.data, aes_string(x = continuous_term, y = outcome, color = categorial_term))
			}
			else
			{
				# Construct a formula that contains all the terms of the linear model, except for the term that we want to plot against
				plot.formula <- formula(paste(outcome, paste(other_terms, collapse=" + "), sep=" ~ "))
				plot.lm <- lm(plot.formula, data = plot.data)
				plot.data$residuals <- plot.lm$residuals
				if(add.mean.to.residuals)
					plot.data$residuals <- plot.data$residuals + mean(plot.data[,outcome])
				plot <- ggplot(plot.data, aes_string(x = continuous_term, y = "residuals", color = categorial_term))
				plot <- plot + labs(y = paste(outcome, ifelse(add.mean.to.residuals,"(mean + residuals)" , "(residuals)")))
			}
			plot <- plot + geom_point(alpha = 0.5)
			plot <- plot + geom_smooth(method = "lm", size = 0.5, aes_string(fill = categorial_term), se = conf.int, alpha = 0.2)
			plot <- plot + labs(title = title)
			print(plot)
			
			if(interactive)
				readline(prompt = "Hit any key to see the next plot:")
		}
	}
}




# Plot each term of a linear model vs outcome or residuals. Optionally plot interactions using colored groups.
# lm: a linear model as retuned by lm()
# title: character, optional title for the plot
# conf.int: logical, whether to plot confidence intervals as shaded areas
# add.mean.to.residuals: logical, whether to add the y mean to the residuals
# interactions: logical, whether to plot interactions of continuous with categorial terms using colored groups
# interactive: logical, whether to prompt for input before advancing to next plot. set this to false when plotting to a file
plot.lm.results <- function(lm, title = NULL, conf.int = TRUE, add.mean.to.residuals = FALSE, interactions = TRUE, interactive = TRUE)
{
	plot.lm.results.ggplot(lm, title = title, conf.int = conf.int, add.mean.to.residuals = add.mean.to.residuals, interactive = interactive)
	if(interactions)
		plot.lm.results.ggplot.interactions(lm, title = title, conf.int = conf.int, add.mean.to.residuals = add.mean.to.residuals, interactive = interactive)
}




