# COL_IDENTIFY

test_that("col_identify should return a datafame", {

    a <- c(1, 2, 9, 10, 10, 9)
    b <- c(2, 10, 16, 4, 7, 11)
    c <- c(2, 9, 10, 10, 6, 10)
    y <- c(5, 11, 100, 1, 1, 6)

    toy <- data.frame(a, b, c, y)
    toy_x <- c("a", "b", "c")
    toy_y <- c("y")
    
    expect_true("data.frame" %in% c(class(
      col_identify(toy, toy_x, toy_y, vif_limit = 2, corr_max = 0.65)
  ))
)
})


test_that('function should throw an error if the arguments are the wrong type', {

    fake_df <- c("apples", "bananas")
    fake_exp <- 100
    fake_resp <- c(100, 200)

    a <- c(1, 2, 9, 10, 10, 9)
    b <- c(2, 10, 16, 4, 7, 11)
    c <- c(2, 9, 10, 10, 6, 10)
    y <- c(5, 11, 100, 1, 1, 6)

    toy <- data.frame(a, b, c, y)
    toy_x <- c("a", "b", "c")
    toy_y <- c("y")

    expect_error(col_identify(fake_df, toy_x, toy_y))
    expect_error(col_identify(toy, fake_exp, toy_y))
    expect_error(col_identify(toy, toy_x, fake_resp))
})


test_that("col_identify should return correct matches", {

    variable <- c("c")
    correlation <- c(0.6573)
    rounded_corr <- c(0.66)
    pair <- list(c("b", "c"))
    vif_score <- c(2.3876)

    a <- c(1, 2, 9, 10, 10, 9)
    b <- c(2, 10, 16, 4, 7, 11)
    c <- c(2, 9, 10, 10, 6, 10)
    y <- c(5, 11, 100, 1, 1, 6)

    toy <- data.frame(a, b, c, y)
    toy_x <- c("a", "b", "c")
    toy_y <- c("y")

    test_df <- tibble::tibble(variable, correlation, rounded_corr, pair, vif_score)
    col_output <- col_identify(toy, toy_x, toy_y, vif_limit = 2, corr_max = 0.65) |>
    dplyr::mutate(correlation = round(correlation, 4),
                  vif_score = round(vif_score, 4))

  expect_equivalent(col_output, test_df)
})
