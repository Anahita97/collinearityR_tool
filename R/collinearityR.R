#' Calculate the Pearson correlation coefficients for all numeric variables.
#' Round the outcome to desired decimals.
#'
#' @param df tibble, the input dataframe
#'
#' @param decimals int, the number of decimals
#'
#' @return a generic correlation matrix and its longer form that can be passed to other functions in this package.
#' @export
#'
#' @examples
#' corr_matrix(tibble::tibble(x = 1:5, y = 2:6))
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
    
    corr_matrix_generic <- df |> 
        dplyr::select_if(is.numeric) |> 
        stats::cor(method = "pearson")

    corr_matrix_longer <- corr_matrix_generic |> 
        as.data.frame() |> 
        tibble::rownames_to_column("variable1") |>
        tidyr::pivot_longer(-variable1, names_to = "variable2", values_to = "correlation")|> 
        dplyr::mutate(rounded_corr = round(correlation, digits = decimals))
    
    result <- list(corr_matrix_longer, corr_matrix_generic)
}

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
#' corr_heatmap(mtcars[, c(1,3,4,5,6,7)])
corr_heatmap <- function(df) {
  corr.matrix <- corr_matrix(df)[[1]]
  variable1 <- variable2 <- correlation <- NULL

  heatmap <- ggplot2::ggplot(corr.matrix, ggplot2::aes(x = variable1, y = variable2, fill = correlation)) +
    ggplot2::geom_tile() +
    ggplot2::geom_text(ggplot2::aes(label = round(correlation, 2))) +
    ggplot2::scale_fill_gradient2(low = "dodgerblue4", mid = "white", high = "sienna4", midpoint = 0) +
    ggplot2::theme(
      axis.title.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank()
    )

  return(heatmap)
}

#' Returns a list containing a dataframe that includes Variance Inflation Factor (VIF) score and
#' a bar chart for the VIF scores alongside the specified threshold for each explanatory variable
#' in a linear regression model.
#'
#' @param x A list of the names of the explanatory variables.
#' @param y A string specifying the response variable.
#' @param df A data frame containing the data.
#' @param thresh An integer specifying the threshold.
#'
#' @return A list containing a data frame for VIFs and a bar chart of the VIFs for each explanatory variable alongside the threshold.
#' @export
#'
#' @examples
#' vif_bar_plot(["exp1", "exp2", "exp3"], "response", data, 5)
vif_bar_plot <- function(x, y, df, thresh){
}

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
