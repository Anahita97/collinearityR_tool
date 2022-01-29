
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



#' Returns a list containing a tibble that includes Variance Inflation Factor (VIF) score and
#' a bar chart for the VIF scores alongside the specified threshold for each explanatory variable
#' in a linear regression model.
#'
#' @param x A vector of the names of the explanatory variables.
#' @param y A string specifying the response variable.
#' @param tbl A tibble containing the data.
#' @param thresh An integer specifying the threshold.
#'
#' @return A list containing a tibble for VIFs and a bar chart of the VIFs for each explanatory variable alongside the threshold.
#' @export
#'
#' @examples
#' vif_bar_plot(c("exp1", "exp2", "exp3"), "response", data, 5)
vif_bar_plot <- function(x, y, df, thresh) {

  if (!is.vector(x)) {
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
