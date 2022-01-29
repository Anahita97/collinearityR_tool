
# tests for vif_bar_plot function

vif <- vif_bar_plot(c("Sepal.Width", "Sepal.Length"), "Petal.Width", iris, 5)

vif_df_example <- tibble::tibble(vif_score = c(1.014, 1.014),
                                 explanatory_var = c("Sepal.Width", "Sepal.Length"))

## wrong inputs
test_that("Wrong input data type.", {
  expect_error(vif_bar_plot(5, "Petal.Width", iris, 5))
  expect_error(vif_bar_plot(c("Sepal.Width", "Sepal.Length"), list("Petal.Width"), iris, 5))
  expect_error(vif_bar_plot(c("Sepal.Width", "Sepal.Length"), "Petal.Width", "iris", 5))
  expect_error(vif_bar_plot(c("Sepal.Width", "Sepal.Length"), "Petal.Width", iris, "5"))
})


## data types of outputs
test_that("The output should be a list.", {
  expect_equal(class(vif), "list")
})
test_that("The first element of the list should be a tibble.", {
  expect_true("tbl" %in% c(class(vif[[1]])))
})
test_that("The second element of the list should be a ggplot object.", {
  expect_true("ggplot" %in% c(class(vif[[2]])))
})


## dataframe tests
test_that("Incorrect results.", {
  expect_equal(vif[[1]] |> dplyr::mutate_if(is.numeric, round, 3),
               vif_df_example,
               ignore_attr = TRUE)
})
test_that("Wrong column names.", {
  expect_equal(vif[[1]] |> colnames(),
               c("vif_score", "explanatory_var"))
})
test_that("Wrong data type for the VIF scores column.", {
  expect_equal(vif[[1]]$vif_score |> class(),
               "numeric")
})
test_that("Wrong data type for the explantory variable column.", {
  expect_equal(vif[[1]]$explanatory_var |> class(),
               "character")
})
test_that("Wrong tibble dimensions.", {
  expect_equal(dim(vif[[1]]),
               c(length(c("Sepal.Width", "Sepal.Length")), 2))
})

## plot tests

test_that("Must be a ggplot bar graph.", {
  expect_true("GeomBar" %in% c(class(vif[[2]]$layers[[1]]$geom)))
})
test_that("x-axis should be mapped to vif_score.", {
  expect_true(rlang::get_expr(vif[[2]]$mapping$x) == "vif_score")
})
test_that("y-axis should be mapped to explanatory_var.", {
  expect_true(rlang::get_expr(vif[[2]]$mapping$y) == "explanatory_var")
})
test_that("The threshold must be vertical line", {
  expect_true("GeomVline" %in% c(class(vif[[2]]$layers[[2]]$geom)))
})
test_that("The threshold should be a red line.", {
  expect_true(vif[[2]]$layers[[2]]$aes_params$colour == "red")
})
