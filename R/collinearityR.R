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
#' @param color_neg string. The color for the negative correlation.
#' @param color_pos string. The color for the positive correlation.
#'
#' @usage corr_heatmap(df, color_neg, color_pos)
#' @return The heatmap.
#' @export
#' @details The inputs must be a 2-D data frame
#' @examples
#' corr_heatmap(mtcars[, c(1,3,4,5,6,7)])
corr_heatmap <- function(df, color_neg = "dodgerblue4", color_pos = "sienna4") {

  variable1 <- variable2 <- correlation <- NULL

  if (!((color_neg %in% grDevices::colors()) & (color_pos %in% grDevices::colors()))){
    stop("The color name is not a valid color. Use colors() to check the full color list.")
  }

  corr.matrix <- corr_matrix(df)[[1]]

  heatmap <- ggplot2::ggplot(corr.matrix, ggplot2::aes(x = variable1, y = variable2, fill = correlation)) +
    ggplot2::geom_tile() +
    ggplot2::geom_text(ggplot2::aes(label = round(correlation, 2))) +
    ggplot2::scale_fill_gradient2(low = color_neg, mid = "white", high = color_pos, midpoint = 0) +
    ggplot2::theme(
      axis.title.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank()
    )

  return(heatmap)
}



#' Returns a list containing a tibble that includes Variance Inflation Factor (VIF) score and
#' a bar chart for the VIF scores alongside the specified threshold for each explanatory variable
#' in a linear regression model.
#'
#' @param x A vector of the names of the explanatory variables.
#' @param y A string specifying the response variable.
#' @param df A tibble containing the data.
#' @param thresh An integer specifying the threshold.
#'
#' @return A list containing a tibble for VIFs and a bar chart of the VIFs for each explanatory variable alongside the threshold.
#' @export
#'
#' @examples
#' vif_bar_plot(c("Sepal.Width", "Sepal.Length"), "Petal.Width", iris, 5)
vif_bar_plot <- function(x, y, df, thresh) {

  if (!is.character(x)) {
    stop("x must be a vector of explanatory variables!")
  }
  if (!is.character(y)) {
    stop("y must be a string!")
  }
  if (!is.data.frame(df)) {
    stop("df must be a pandas data frame!")
  }
  if (!is.numeric(thresh)) {
    stop("thresh must be a numeric value!")
  }

  lm <- explanatory_var <- NULL

  # Data frame containing VIF scores
  explan_var <- paste(x, collapse = " + ") |>
    noquote()
  response <- noquote(y)

  lm_model <- lm(paste(response, "~", explan_var), data = df)

  vif_score <- car::vif(lm_model)
  vif_df <- tibble::tibble(vif_score)
  vif_df$explanatory_var <- x


  # Plotting the VIF scores
  hbar_plot <- vif_df |>
    ggplot2::ggplot(ggplot2::aes(x = vif_score, y = explanatory_var)) +
    ggplot2::geom_bar(stat = "identity",
                      fill = "dodgerblue3",
                      color = "lightgrey") +
    ggplot2::geom_vline(xintercept = thresh, color = "red", lty = 5) +
    ggplot2::labs(x = "VIF Score",
                  y = "Explanatory Variable",
                  title = "VIF Scores for Each Explanatory Variable in Linear Regression")

  list(vif_df, hbar_plot)
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
#' col_identify(iris, c("Sepal.Width", "Petal.Length"),
#'              "Petal.Width", vif_limit = 0, corr_max = 0.3, corr_min = -0.3)
col_identify <- function(
    df, X, y, corr_min = -0.8, corr_max = 0.8, vif_limit = 4) {

    variable1 <- variable2 <- correlation <- NULL
    explanatory_var <- vif_score <- pair <- desc <- NULL

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
        dplyr::rename(variable1 = explanatory_var) |>
        dplyr::filter(vif_score >= vif_limit)

    results_df <- dplyr::inner_join(input_corr, vif_output) |>
        dplyr::arrange(desc(pair)) |>
        dplyr::select(-variable2) |>
        dplyr::rename(variable = variable1) |>
        dplyr::group_by(pair) |>
        dplyr::top_n(1, vif_score)

    results_df
}


