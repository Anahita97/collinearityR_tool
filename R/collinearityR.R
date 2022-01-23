#' Calculate the Pearson correlation coefficients for all numeric variables.
#' Round the outcome to desired decimals.
#'    
#' @param df tibble, the input dataframe
#'
#' @param decimals int, the number of decimals
#'
#' @return a generic correlation matrix and its longer form
#' that can be passed to other functions in this package
#'
#' @examples
#' corr_matrix(tibble(x = 1:5, y = 2:6))
corr_matrix <- function(df, decimals = 2) {}


#' Plot rectangular data as a color-encoded Pearson correlaiton matrix.
#' 
#' @description The rows and the columns contain variable names, while 
#' the heatmap tiles contain Pearson correlation coefficient and 
#' corresponding colours.
#' 
#' @param df dataframe. 2-D dataframe.
#' 
#' @usage corr_heatmap(df)
#' @return The heatmap.
#' @export
#' @details The inputs must be a 2-D data frame
#' @examples
#' corr_heatmap(data)
corr_heatmap <- function(df){}


#' Multicollinearity identification function highly correlated pairs 
#' (Pearson coefficient) with VIF values exceeding the threshold.

#' This function returns a DataFrame containing Pearson's coefficient,
#' VIFF, and the suggestion to eliminate or keep a variable based on 
#' VIFF and Pearson's coefficient thresholds.
#'
#' @param df An input dataframe
#' @param corr_min A decimal number that serves as a threshold for selecting
#' a pair. This is a Pearson coefficient value. Default set at -0.8.
#' @param corr_max A decimal number that serves as a threshold for selecting
#' a pair. This is a Pearson coefficient value. Default set at 0.8.
#' @param vif_limit A decimal number that serves as a threshold for selecting
#' a pair. This is a VIF value. Default set at 4.
#' @export
#' @examples
#' col_identify(df, 0.9, 5)
col_identify <- function(diff, corr_min = -0.8, corr_max = 0.8, vif_limit = 4){}