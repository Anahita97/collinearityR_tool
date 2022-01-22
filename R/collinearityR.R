#' Calculate the Pearson correlation coefficients for all numeric variables.
#' Round the outcome to desired decimals.
#'    
#' @param df tibble, the input dataframe
#' 
#' @return a longer form of the correlation matrix 
#' that can be passed to other functions in this package
#'
#' @examples
#' corr_longer(tibble(x = 1:5, y = 2:6))

corr_longer <- function(df, decimals = 2) {}