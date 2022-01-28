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

library(tidyverse)
library(assertthat)

corr_matrix <- function(df, decimals = 2) {
    
    assert_that(is.data.frame(df), msg = "The input must be a data frame.")
    assert_that(decimals %% 1 == 0, msg = "The input decimals must be a positive integer.")
    assert_that(decimals > 0, msg = "The input decimals must be a positive integer.")
    assert_that(length(select_if(df, is.numeric)) != 0, msg = "The input data frame must contain at least one numeric column.")
    assert_that(nrow(df) > 1, msg = "The input dataframe should contain at least two observations.")
    
    corr_matrix <- df |> 
        select_if(is.numeric) |> 
        cor(method = "pearson") |> 
        round(digits = decimals)


    corr_matrix_longer <- corr_matrix |> 
        as.data.frame() |> 
        rownames_to_column("var1") |>
        pivot_longer(-var1, names_to = "var2", values_to = "corr")
    
    result <-  list(corr_matrix_longer, corr_matrix)
}