\name{plot.lm.results}
\alias{plot.lm.results}
\title{Make nice plots for a linear model}


\description{
Plot each term of a linear model vs outcome or residuals. Optionally plot interactions using colored groups.
}


\usage{
plot.lm.results(lm, title = NULL, conf.int = TRUE, add.mean.to.residuals = FALSE, interactions = TRUE, interactive = TRUE)
}


%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lm}{
a linear model as retuned by lm()
}
  \item{title}{
optional title for the plot
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
# Load example datasets
require(datasets)

lm <- lm(len ~ supp + dose, data = ToothGrowth)
summary(lm)
plot.lm.results(lm)
}
