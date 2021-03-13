context("Test for function safe_speed()")

test_that("safe-speed negative values test", {
  test_df1 <- data.frame(v1 = -1, mu1 = 0.5, sd1 = 100)

  expect_error(safe_speed(v = test_df1$v1,
                          mu = test_df1$mu1,
                          safe_distance = test_df1$sd1),
               "Speed cannot be less than zero!")

  test_df2 <- data.frame(v2 = 2, mu2 = 0.5, sd2 = -50)

  expect_error(safe_speed(v = test_df2$v2,
                          mu = test_df2$mu2,
                          safe_distance = test_df2$sd2),
               "Target Distance cannot be less than zero!")

  test_df3 <- data.frame(v = 2, mu = 0, sd = 50)

  expect_error(safe_speed(v = test_df3$v,
                          mu = test_df3$mu,
                          safe_distance = test_df3$sd),
               "Zero or negative friction coefficients are impossible!")

})

test_that("stop the loop in safe-speed() after 999 times", {
  test_df4 <- data.frame(v = 0, mu = 0.1, sd = 10000)

  expect_equal(safe_speed(v = test_df4$v,
                          mu = test_df4$mu,
                          safe_distance = test_df4$sd),
               99.9)
})
