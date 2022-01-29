test_that("col_identify should return a datafame", {

  mpg <- system.file("extdata", "mpg.csv", package="collinearityR")
  real_df <- read.csv(mpg)
  exp <- c("cyl", "cty", "displ", "year")
  resp <- c("hwy")
  expect_true("data.frame" %in% c(class(col_identify(real_df, exp, resp))))
})


test_that('function should throw an error if the arguments are the wrong type', {

  fake_df <- c("apples", "bananas")
  fake_exp <- 100
  fake_resp <- c(100, 200)
  mpg <- system.file("extdata", "mpg.csv", package="collinearityR")
  real_df <- read.csv(mpg)
  exp <- c("cyl", "cty", "displ", "year")
  resp <- c("hwy")

  expect_error(col_identify(fake_df, exp, resp))
  expect_error(col_identify(real_df, fake_exp, resp))
  expect_error(col_identify(real_df, exp, fake_resp))
})


test_that("col_identify should return correct matches", {

  variable <- c("cyl", "cyl")
  correlation <- c(0.9302, -0.8058)
  rounded_corr <- c(0.93, -0.81)
  pair <- list(c("cyl", "displ"), c("cty", "cyl"))
  vif_score <- c(8.0817, 8.0817)

  mpg <- system.file("extdata", "mpg.csv", package="collinearityR")
  real_df <- read.csv(mpg)
  exp <- c("cyl", "cty", "displ", "year")
  resp <- c("hwy")

  toy_df_col <- tibble::tibble(variable, correlation, rounded_corr, pair, vif_score)
  col_output <- col_identify(real_df, exp, resp) |>
    dplyr::mutate(correlation = round(correlation, 4),
                  vif_score = round(vif_score, 4))

  expect_equivalent(col_output, toy_df_col)
})
