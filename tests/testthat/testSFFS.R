context("SFFS")

data1 <- get(load('../data/example.RData'))
data2 <- get(load("../data/example2.RData"))

test_that("Returns correct results", {
  expect_identical(sffs(data1, 'clase', roughsetConsistency)$bestFeatures[1,], c('x1' = 0, 'x2' =  1, 'x3' = 1))
  expect_equal(sffs(data2, 'clase', gainRatio)$bestFeatures[1,], c('x1' = 0, 'x2' = 0, 'x3' = 0, 'x4' = 0, 'x5' = 0, 'x6' = 1))
})

test_that("Name is set", {
  expect_equal(attr(sffs,'name'),"Sequential Floating Forward Selection");
  expect_equal(attr(sffs,'shortName'),"sffs");
})