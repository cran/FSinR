context("Whale Optimization Algorithm")

test_that("Name is set", {
  expect_equal(attr(woa,'name'),"Whale Optimization Algorithm");
  expect_equal(attr(woa,'shortName'),"woa");
})