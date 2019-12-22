context("Select Percentile")

data2 <- get(load("../data/example2.RData"))

test_that("Results are correct", {
  expect_setequal(selectPercentile(data2, 'clase', giniIndex, 20)$featuresSelected, c('x6'))
  expect_setequal(selectPercentile(data2, 'clase', giniIndex, 30)$featuresSelected, c('x6', 'x2'))
  expect_setequal(selectPercentile(data2, 'clase', giniIndex, 50)$featuresSelected, c('x6', 'x2', 'x3'))
})

test_that("Name is set", {
  expect_equal(attr(selectPercentile,'name'),"Select Percentile");
})