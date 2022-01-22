
<!-- README.md is generated from README.Rmd. Please edit that file -->

# collinearityR

<!-- badges: start -->
<!-- badges: end -->

Identify multicollinearity issues by correlation, VIF, and visualizations. This package is designed for beginners of R who want to identify multicollinearity issues by applying a simple function. It automates the process of building a longer form of correlation matrix, creating correlation heat map and identifying pairwise highly correlated variables. A python version of package is also in the progress of development.

## Functions 

The following four functions are in the collinearityR package:
- `corr_matrix`: A function that returns a generic and the longer form of a correlation matrix for all numerical variables in a data frame.
- `corr_heatmap`: A function that returns a correlation heatmap given a dataframe.
- `vif_bar_plot`: A function that returns a list containing a data frame for Variable Inflation Factors (VIF) and a bar chart of the VIFs for each explanatory variable in a multiple linear regression model.
- `ol_identify`: A function that identifies multicollinearity based on highly correlated pairs (using Pearson coefficient) with VIF values exceeding the threshold.

## Installation

You can install the released version of collinearityR from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("collinearityR")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("UBC-MDS/collinearityR_tool")
```

## Usage

This is a basic example which shows you how to solve a common problem:

``` r
library(collinearityR)
## basic example code
```
