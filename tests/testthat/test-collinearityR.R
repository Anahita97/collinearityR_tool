# tests for corr_matrix function

data_1 <-  data.frame('A'=c(1,2,3,4,5),
                      'B'=c(2,4,6,8,10),
                      'C'=c(10,8,6,4,2),
                      "D"=c('abc','322', '324', '32', '23')
)
result_1 <-  tibble::tibble("variable1"=c("A","A","A", "B","B","B", "C", "C","C"),
                            "variable2"=c ("A", "B", "C","A", "B", "C","A", "B", "C"),
                            "correlation"=c(1.0, 1.0, -1.0, 1.0, 1.0, -1.0, -1.0, -1.0, 1.0),
                            "rounded_corr" = c(1.0, 1.0, -1.0, 1.0, 1.0, -1.0, -1.0, -1.0, 1.0)
                           )
data_2 <- data.frame("A" = c(1, 2),
                     "B" = c("2", "4")
                    )
result_2 <-  tibble::tibble("variable1" = c("A"),
                            "variable2" = c("A"),
                            "correlation" = c(1.0),
                            "rounded_corr" = c(1.0)
                   )
data_3 <- data.frame("A" = c(1, 2, 3),
                     "B" = c(3, 2, 1)
                    )
result_3 <-  tibble::tibble("A" = c(1, -1),
                            "B" = c(-1, 1),
                            "index" = c("A", "B")
                           )|> tibble::column_to_rownames('index')

## output error tests
test_that("The correlation matrix in its longer form is wrong", {
  expect_equal(corr_matrix(data_1)[[1]], result_1)
})

test_that("The longer form should return only one row for a one numeric data frame", {
  expect_equal(corr_matrix(data_2)[[1]], result_2)
})

test_that("The generic correlation matrix is wrong", {
  expect_equal(as.data.frame(corr_matrix(data_3)[[2]]), result_3)
})


## dataframe tests
test_that("The number of rows of output is wrong", {
  expect_equal(nrow(corr_matrix(data_1)[[1]]), 9)
  expect_equal(nrow(corr_matrix(data_2)[[1]]), 1)
  expect_equal(nrow(corr_matrix(data_3)[[2]]), 2)
})

## input data type tests
test_that("The input data type is incorrect", {
  expect_error(corr_matrix(123, 2), "The input must be a data frame.")
  expect_error(corr_matrix(iris, 2.5), "The input decimals must be a positive integer.")
  expect_error(corr_matrix(iris, -3), "The input decimals must be a positive integer.")
  expect_error(corr_matrix(iris|> dplyr::select(Species)), "The input data frame must contain at least one numeric column.")
  expect_error(corr_matrix(iris[sample(nrow(iris), 1),]), "The input dataframe should contain at least two observations.")
})

# tests for corr_heatmap function

test_that('Base layer of heatmap should use geom_tile.', {
  expect_true("GeomTile" %in% c(class(corr_heatmap(data_1)$layers[[1]]$geom)))
})
test_that('Base layer of heatmap should map correlaiton to fill property.', {
  expect_true("correlation" == rlang::get_expr(corr_heatmap(data_1)$mapping$fill))
})
test_that('The x axis should be mapped to the explanatory variables.', {
  expect_true("variable1" == rlang::get_expr(corr_heatmap(data_1)$mapping$x))
})
test_that('The y axis should be mapped to the explanatory variables.', {
  expect_true("variable2" == rlang::get_expr(corr_heatmap(data_1)$mapping$y))
})
test_that('Text layer of heatmap should use geom_text.', {
  expect_true("GeomText" %in% c(class(corr_heatmap(data_1)$layers[[2]]$geom)))
})



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


  
# tests for col_identify function

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

  expect_equal(col_output, test_df, ignore_attr = TRUE)
})