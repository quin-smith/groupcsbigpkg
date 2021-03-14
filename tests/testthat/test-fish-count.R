context("Test for function fish_count()")

test_that("Error checking works", {
  test.1 <- c(1:10)

  expect_error(fish_count(fish = test.1),
               "fish is not a character vector!")
})


test_that("Check the class and value of outputs",{
  test.2 <- c("fish.a","fish.b","fish.c","fish.a","fish.b","fish.a")

  expect_is(fish_count(test.2)$most, "character")
  expect_is(fish_count(test.2)$rarest, "character")
  expect_is(fish_count(test.2)$total, "integer")
  expect_equal(fish_count(test.2)$total, 6)
})

test_that("Plot default setting", {
  test.2 <- c("fish.a","fish.b","fish.c","fish.a","fish.b","fish.a")

  expect_is(fish_count(fish = test.2,
                       hist.plot = TRUE)$fish_plot,
            "matrix")

  expect_false(class(fish_count(fish = test.2)$fish_plot) == "matrix")
})
