test_that("`plot.see_rope()` works", {
  if (require("bayestestR") && require("rstanarm")) {
    set.seed(123)
    m <- stan_glm(Sepal.Length ~ Petal.Width * Species,
      data = iris,
      refresh = 0
    )
    result <- rope(m)

    expect_s3_class(plot(result), "gg")
  }
})
