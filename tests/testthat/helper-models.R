# commonly utilized models
if (require("rstanarm", quietly = TRUE)) {
  m_rstan <<- suppressWarnings(
    rstanarm::stan_glm(
      Sepal.Length ~ Petal.Width * Species,
      data = datasets::iris,
      refresh = 0
    )
  )
}
