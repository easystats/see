# commonly utilized models
if (suppressPackageStartupMessages(require("rstanarm", warn.conflicts = FALSE, character.only = TRUE))) {
  m_rstan <<- suppressWarnings(
    rstanarm::stan_glm(
      Sepal.Length ~ Petal.Width * Species,
      data = datasets::iris,
      refresh = 0
    )
  )
}
