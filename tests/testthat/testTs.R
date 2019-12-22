context("Tabu Search")

test_that("Tabu search works", {
  result <- ts(iris, 'Species', IEConsistency, iter = 20)
  expect_true(sum(result$bestFeatures) == 3)
  expect_equal(result$bestFitness,1)
})

test_that("Name is set", {
  expect_equal(attr(ts,'name'),"Tabu Search");
})