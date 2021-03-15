context("ld_50() test")

test_that("170 lbs returns 122 cups", {
  expect_output(ld_50(170), cat(c(122, "cups will kill you")))
})

test_that("Error checking message works", {
  expect_match(ld_50(0), "Please enter a positive number for weight")
  expect_match(ld_50(-1), "Please enter a positive number for weight")
})

test_that("1 lbs returns 1 cup", {
  expect_match(ld_50(1), "1 cup will kill you")
})
