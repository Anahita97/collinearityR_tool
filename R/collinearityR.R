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

col_identify <- function(diff, corr_min = -0.8, corr_max = 0.8, vif_limit = 4)