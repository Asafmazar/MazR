% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mazR_squared.R
\name{mazar_squared}
\alias{mazar_squared}
\title{MazR Squared}
\usage{
mazar_squared(model)
}
\arguments{
\item{model}{A model object.}
}
\value{
The squared Spearman correlation between the observed and fitted values of the model.
}
\description{
Computes the squared Spearman correlation between the observed and fitted values of a given model. Please do not use this as an actual measure of explained variance.
}
\examples{
\dontrun{
library(stats)
fit <- lm(mpg ~ wt, data = mtcars)
mazar_squared(fit)
}
}
