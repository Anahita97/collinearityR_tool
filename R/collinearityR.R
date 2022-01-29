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
corr_heatmap <- function(df){}

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
vif_bar_plot <- function(x, y, df, thresh){
}

#' Multicollinearity identification function highly correlated pairs
#' (Pearson coefficient) with VIF values exceeding the threshold.
#'
#' This function returns a DataFrame containing Pearson's coefficient,
#' VIFF with explanatory variables suggested for elimination.
#' An empty dataframe means no multicollinearity detected.
#'
#' @param df An input dataframe
#' @param X Explanatory variables (vector of characters)
#' @param y Response variable (single vector)
#' @param corr_min (optional) A decimal number that serves as a threshold for selecting
#' a pair. This is a Pearson coefficient value. Default set at -0.8.
#' @param corr_max (optional) A decimal number that serves as a threshold for selecting
#' a pair. This is a Pearson coefficient value. Default set at 0.8.
#' @param vif_limit (optional) A decimal number that serves as a threshold for selecting
#' a pair. This is a VIF value. Default set at 4.
#' @export
#' @examples
col_identify <- function(
    df, X, y, corr_min = -0.8, corr_max = 0.8, vif_limit = 4) {

    if (!is.character(X)) {
        stop("X must be a vector with character variables")
    }

    if (!is.character(y)) {
        stop("y must be a character vector")
    }

    if (!is.data.frame(df)) {
        stop('col_identify df argument expects a dataframe')
    }

    if (!is.numeric(corr_min)) {
         stop("corr_min should be numeric")
    }

    if (!is.numeric(corr_max)) {
         stop("corr_max should be numeric")
    }

    if (!is.numeric(vif_limit)) {
         stop("vif_limit should be numeric")
    }

    input_corr <- df |>
        dplyr::select(tidyselect::all_of(X))

    input_corr <- corr_matrix(input_corr)[[1]] |>
        dplyr::filter(
            correlation <= corr_min |
            correlation >= corr_max &
            variable1 != variable2)

    pair_maker <- function(x, y) {
        list_vector <- c(x, y) |>
        stringr::str_sort()
        list_vector
        }

    input_corr$pair <- purrr::map2(input_corr$variable1,
                            input_corr$variable2,
                            pair_maker)

    vif_output <- vif_bar_plot(X, y, df, 4)[[1]] |>
        dplyr::rename(variable1 = explanatory_var)

    results_df <- dplyr::inner_join(input_corr, vif_output) |>
        dplyr::arrange(desc(pair)) |>
        dplyr::select(-variable2) |>
        dplyr::rename(variable = variable1) |>
        dplyr::group_by(pair) |>
        dplyr::top_n(1, vif_score)

    results_df
}


