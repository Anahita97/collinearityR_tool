% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/collinearityR.R
\name{col_identify}
\alias{col_identify}
\title{Identifies multicollinearity using
Pearson's coefficient and suggests the variable
with the highest VIF score out of the correlated pair
for elimination.}
\usage{
col_identify(df, X, y, corr_min = -0.8, corr_max = 0.8, vif_limit = 4)
}
\arguments{
\item{df}{An input dataframe}

\item{X}{Explanatory variables (vector of characters)}

\item{y}{Response variable (single vector)}

\item{corr_min}{(optional) A decimal number that serves as a threshold for selecting
a pair. This is a Pearson coefficient value. Default set at -0.8.}

\item{corr_max}{(optional) A decimal number that serves as a threshold for selecting
a pair. This is a Pearson coefficient value. Default set at 0.8.}

\item{vif_limit}{(optional) A decimal number that serves as a threshold for selecting
a pair. This is a VIF value. Default set at 4.}
}
\description{
Returns a DataFrame containing Pearson's coefficient,
VIF with explanatory variables suggested for elimination.
An empty dataframe means no multicollinearity detected.
}
\examples{
col_identify(iris, c("Sepal.Width", "Petal.Length"),
             "Petal.Width", vif_limit = 0, corr_max = 0.3, corr_min = -0.3)
}
