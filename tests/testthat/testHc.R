context("Hill Climbing")

test_that("Name is set", {
  expect_equal(attr(hc,'name'),"Hill Climbing");
  expect_equal(attr(hc,'shortName'),"hc");
})