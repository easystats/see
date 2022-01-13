if (require("correlation") && require("testthat") && require("see") && utils::packageVersion("correlation") > "0.7.1") {
  test_that("`plot.see_easycormatrix()` works", {
    result <- correlation(mtcars[, -c(8:9)])
    s <- summary(result)
    expect_s3_class(plot(s), "gg")
  })
}
