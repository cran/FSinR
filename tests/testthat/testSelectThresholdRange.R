context("Select ThresholdRange")

data1 <- get(load('../data/example.RData'))
data2 <- get(load("../data/example2.RData"))

test_that("Results are correct", {
  expect_setequal(selectThresholdRange(data1, 'clase', IEPConsistency, 0.9)$featuresSelected, c('x3'))
  expect_setequal(selectThresholdRange(data2, 'clase', IEPConsistency, 0.6)$featuresSelected, c('x3', 'x6'))
})

test_that("Name is set", {
  expect_equal(attr(selectThresholdRange,'name'),"Select Threshold Range");
})