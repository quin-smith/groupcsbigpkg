context("Test for function years_to_K()")

test_that("Error Checking Works",{
  expect_match(years_to_K(starting_stock = -10, 0.5, 100),
              "starting stock out of bounds!")
})

test_that("Output is numeric", {
  expect_is(years_to_K(10, 0.5, 100),
            "numeric")
})

