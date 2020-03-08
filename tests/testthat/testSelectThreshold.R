context("Select Threshold")

data2 <- get(load("../data/example2.RData"))

test_that("SelectThreshold works", {
  expect_setequal(selectThreshold(data2, 'clase', IEPConsistency, 0.1)$featuresSelected, c('x1', 'x2', 'x4', 'x5', 'x3', 'x6'))
  expect_setequal(selectThreshold(data2, 'clase', IEPConsistency, 0.6)$featuresSelected, c('x2', 'x4', 'x5', 'x3', 'x6'))
  expect_setequal(selectThreshold(data2, 'clase', IEPConsistency, 0.9)$featuresSelected, c('x6'))
})

test_that("Name is set", {
  expect_equal(attr(selectThreshold,'name'),"Select Threshold");
  expect_equal(attr(selectThreshold,'shortName'),"selectThreshold");
})