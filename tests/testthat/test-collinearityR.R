# col_identify() tests

real_df <- read_csv("https://raw.githubusercontent.com/tidyverse/ggplot2/main/data-raw/mpg.csv")
exp <- c("cyl", "year")
resp <- c("cty")

test_that("col_identify should return a datafame", {
           expect_true("data.frame" %in% c(class(col_identify(real_df, exp, resp))))
         }
)


fake_df <- c("apples", "bananas")
fake_exp <- 100
fake_resp <- c(100, 200)

test_that('function should throw an error if the arguments are the wrong type', {
    expect_error(col_identify(fake_df, exp, resp))
    expect_error(col_identify(real_df, fake_exp, resp))
    expect_error(col_identify(real_df, exp, fake_resp))
})

variable <- c("cyl", "cyl")
correlation <- c(-0.8058, 0.9302)
rounded_corr <- c(-0.81, 0.93)
pair <- list(c("cty", "cyl"), c("cyl", "displ"))
vif_score <- c(8.0817, 8.0817)

toy_df_col <- tibble(variable, correlation, rounded_corr, pair, vif_score)
col_output <- col_identify(df, X, y) |>
    mutate(correlation = round(correlation, 4),
           vif_score = round(vif_score, 4))

test_that("col_identify should return correct matches", {
           expect_equivalent(col_output, toy_df_col)})

# col_identify() tests end
