context("Simmulated Annealing")

test_that("Name is set", {
  expect_equal(attr(sa,'name'),"Simmulated Annealing");
  expect_equal(attr(sa,'shortName'),"sa");
})