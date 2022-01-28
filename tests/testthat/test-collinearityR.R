data_1 <-  data.frame('A'=c(1,2,3,4,5),
                'B'=c(2,4,6,8,10),
                'C'=c(10,8,6,4,2),
                "D"=c('abc','322', '324', '32', '23')
               )

result_1 <-  tibble("var1"=c("A","A","A", "B","B","B", "C", "C","C"),
                "var2"=c ("A", "B", "C","A", "B", "C","A", "B", "C"),
                "corr"=c (1.0, 1.0, -1.0, 1.0, 1.0, -1.0, -1.0, -1.0, 1.0)
               )


data_2 <- data.frame("A" = c(1, 2),
                     "B" = c("2", "4")
                    )

result_2 <-  tibble("var1" = c("A"),
                    "var2" = c("A"),
                    "corr" = c(1.0)
                   )

data_3 <- data.frame("A" = c(1, 2, 3),
                     "B" = c(3, 2, 1)
                    )

result_3 <-  tibble("A" = c(1, -1),
                    "B" = c(-1, 1),
                    "index" = c("A", "B")
                   )|> 
column_to_rownames('index')


test_that("The correlation matrix in its longer form is wrong", {
  expect_equal(corr_matrix(data_1)[[1]], result_1)
})

test_that("The number of rows of longer form is wrong", {
  expect_equal(nrow(corr_matrix(data_1)[[1]]), 9)
})

test_that("The longer form should return only one row", {
  expect_equal(corr_matrix(data_2)[[1]], result_2)
})

test_that("The number of rows of longer form is wrong", {
  expect_equal(nrow(corr_matrix(data_2)[[1]]), 1)
})

test_that("The generic correlation matrix is wrong", {
  expect_equal(as.data.frame(corr_matrix(data_3)[[2]]), result_3)
})
