# test_that("`plot.see_check_distribution()` works", {
#   if (require("randomForest")) {
#     library(performance)
#     m <- lm(mpg ~ wt + cyl + gear + disp, data = mtcars)
#     result <- check_distribution(m)
#
#     expect_s3_class(plot(result), "gg")
#   }
# })
