test_that("freqandrev works", {
  expect_that(((freqandrev(table1_matrix,table2)[3]) >0),  is_true())
})
