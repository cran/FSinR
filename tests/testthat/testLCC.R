context("Linear Consistency-Constrained")

test_that("Feature set is ordered correctly", {
  orderedFeatures <- orderFeatures(iris, 'Species', c('Sepal.Length', 'Sepal.Width', 'Petal.Length', 'Petal.Width'), symmetricalUncertain)
  expect_equal(orderedFeatures, c('Petal.Width', 'Petal.Length', 'Sepal.Length', 'Sepal.Width'));
  myMeasure <- symmetricalUncertain
  attr(myMeasure,'maximize') <- FALSE
  orderedFeatures <- orderFeatures(iris, 'Species', c('Sepal.Length', 'Sepal.Width', 'Petal.Length', 'Petal.Width'), myMeasure)
  expect_equal(orderedFeatures, c('Sepal.Width', 'Sepal.Length', 'Petal.Length', 'Petal.Width'));
})

test_that("Algorithm exits if value is lower than threshold", {
  expect_identical(LCC(iris[,-(2:4)], 'Species', IEConsistency)$bestFeatures[1,], c('Sepal.Length' = 1))
})

test_that("Algorithm performs correctly", {
  expect_identical(LCC(iris, 'Species', IEConsistency, threshold = 0.99999)$bestFeatures[1,], c('Sepal.Length' = 1, 'Sepal.Width' = 0, 'Petal.Length' = 1, 'Petal.Width' = 1))
  expect_identical(LCC(iris, 'Species', IEConsistency, threshold = 0.99)$bestFeatures[1,], c('Sepal.Length' = 0, 'Sepal.Width' = 0, 'Petal.Length' = 1, 'Petal.Width' = 1))
  expect_identical(LCC(iris, 'Species', IEConsistency, threshold = 0.9)$bestFeatures[1,], c('Sepal.Length' = 0, 'Sepal.Width' = 0, 'Petal.Length' = 0, 'Petal.Width' = 1))
})

test_that("Name is set", {
  expect_equal(attr(LCC,'name'),"Linear Consistency-Constrained");
  expect_equal(attr(LCC,'shortName'),"LCC");
})