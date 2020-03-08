context("Select K Best")

data1 <- get(load('../data/example.RData'))
data2 <- get(load("../data/example2.RData"))
data3 <- get(load("../data/continuous.RData"))

test_that("Results are correct", {
  expect_setequal(selectKBest(data1, 'clase', IEPConsistency, 1)$featuresSelected, c('x3'))
  expect_setequal(selectKBest(data1, 'clase', giniIndex, 1)$featuresSelected, c('x2'))
  expect_setequal(selectKBest(data2, 'clase', giniIndex, 4)$featuresSelected, c('x6', 'x2', 'x3', 'x4'))
  expect_setequal(selectKBest(data3, 'clase', determinationCoefficient, 3)$featuresSelected, c('x3', 'x1', 'x2'))
})

test_that("Name is set", {
  expect_equal(attr(selectKBest,'name'),"Select K Best");
  expect_equal(attr(selectKBest,'shortName'),"selectKBest");
})