# test_that("`print.see_performance_pp_check()` works", {
#   if (require("performance")) {
#     model <- lm(Sepal.Length ~ Species * Petal.Width + Petal.Length,
#       data = iris
#     )
#
#     expect_s3_class(plot(pp_check(model)), "gg")
#   }
# })
