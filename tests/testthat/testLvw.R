context("Las Vegas Wrapper")

test_that("Name is set", {
  expect_equal(attr(lvw,'name'),"Las Vegas Wrapper");
  expect_equal(attr(lvw,'shortName'),"lvw");
})