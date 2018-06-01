test_that("NPV_WORKS", {
  expect_that(compute_NPV(value=100,
  time=100, 
  discount=0),
  equals(100))
  
})