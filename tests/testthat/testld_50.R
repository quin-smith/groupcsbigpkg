context("ld_50() test")

test_that("170 lbs returns 122 cups", {
  expect_output(ld_50(170), cat(c(122, "cups will kill you")))
})
