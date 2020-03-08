context("Ant Colony Optimization")

test_that("Name is set", {
  expect_equal(attr(aco,'name'),"Ant Colony Optimization");
  expect_equal(attr(aco,'shortName'),"aco");
})