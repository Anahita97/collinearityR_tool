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

# tests for corr_heatmap function


test_that("The correlation matrix in its longer form is wrong", {
  expect_equal(corr_matrix(data_1)[[1]], result_1)
})

test_that("The number of rows of longer form is wrong", {
  expect_equal(nrow(corr_matrix(data_1)[[1]]), 9)
  expect_equal(nrow(corr_matrix(data_2)[[1]]), 1)
})

test_that("The longer form should return only one row", {
  expect_equal(corr_matrix(data_2)[[1]], result_2)
})

test_that("The generic correlation matrix is wrong", {
  expect_equal(as.data.frame(corr_matrix(data_3)[[2]]), result_3)
})

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
                     )



