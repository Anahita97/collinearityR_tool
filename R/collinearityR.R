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

corr_matrix <- function(df, decimals = 2) {
    
    variable1 <- variable2 <- correlation <- NULL
    
    if (!is.data.frame(df)){
        stop("The input must be a data frame.")
    }
    if (!(decimals %% 1 == 0)){
        stop("The input decimals must be a positive integer.")
    }
    if (!(decimals > 0)){
        stop("The input decimals must be a positive integer.")
    }
    if (length(dplyr::select_if(df, is.numeric)) == 0){
        stop("The input data frame must contain at least one numeric column.")
    }
    if (!(nrow(df) > 1)){
        stop("The input dataframe should contain at least two observations.")
    }
    
    corr_matrix <- df |> 
        dplyr::select_if(is.numeric) |> 
        stats::cor(method = "pearson")

    corr_matrix_longer <- corr_matrix |> 
        as.data.frame() |> 
        tibble::rownames_to_column("variable1") |>
        tidyr::pivot_longer(-variable1, names_to = "variable2", values_to = "correlation")|> 
        dplyr::mutate(rounded_corr = round(correlation, digits = decimals))
    
    result <-  list(corr_matrix_longer, corr_matrix)
}