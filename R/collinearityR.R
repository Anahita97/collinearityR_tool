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

