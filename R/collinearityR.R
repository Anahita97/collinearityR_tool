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

corr_matrix <- function(df, decimals = 2) {
    corr_matrix <- df |> 
        select_if(is.numeric) |> 
        cor(method = "pearson") |> 
        round(digits = decimals)


    corr_matrix_longer <- corr_matrix |> 
        as.data.frame() |> 
        rownames_to_column("var1") |>
        pivot_longer(-var1, names_to = "var2", values_to = "corr")
    
    result <-  list(corr_matrix, corr_matrix_longer)
}