test_that("`plot.see_n_factors()` works", {
  if (require("parameters") && require("nFactors")) {
    data(mtcars)
    result <- n_factors(mtcars, type = "PCA")
    result
    expect_s3_class(plot(result, type = "line"), "gg")
  }
})
