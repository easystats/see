# test_that("`plot.see_parameters_simulate()` works", {
#   m <- lm(mpg ~ wt + cyl + gear, data = mtcars)
#   result <- simulate_parameters(m)
#
#   expect_s3_class(plot(result), "gg")
# })
