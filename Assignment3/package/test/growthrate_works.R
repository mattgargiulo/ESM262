test_that("growthrate_works",{
  expect_that(growthrate(a=-0.2425, b=0.1519, c=0.05520, d= -0.002931, T=13.8), equals(4.66))
  expect_that(growthrate(a=-0.1596, b=0.1498, c=0.02442, d= -0.001515, T=13.2), equals(2.59))
  expect_that(growthrate(a= 0.0034, b=0.1702, c=0.00769, d= -0.000813, T=12.1), equals(1.75)) 
  })