data_1 <-  data.frame('A'=c(1,2,3,4,5),
                      'B'=c(2,4,6,8,10),
                      'C'=c(10,8,6,4,2),
                      "D"=c('abc','322', '324', '32', '23')
)

test_that('Base layer of heatmap should use geom_tile and map correlaiton to fill property.', {
  expect_true("GeomTile" %in% c(class(corr_heatmap(data_1)$layers[[1]]$geom)))
  expect_true("correlation" == rlang::get_expr(corr_heatmap(data_1)$mapping$fill))
})

test_that('Text layer of heatmap should use geom_text.', {
  expect_true("GeomText" %in% c(class(corr_heatmap(data_1)$layers[[2]]$geom)))
})
