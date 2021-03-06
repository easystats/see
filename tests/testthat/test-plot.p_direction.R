test_that("`plot.see_p_direction()` works", {
  if (require("bayestestR") && require("rstanarm")) {
    set.seed(123)
    m <<- stan_glm(Sepal.Length ~ Petal.Width * Species,
      data = iris,
      refresh = 0
    )
    result <- p_direction(m)
    expect_s3_class(plot(result), "gg")
  }
})
