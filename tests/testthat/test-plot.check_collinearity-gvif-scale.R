test_that("collinearity plots label dimension-adjusted GVIF data", {
  cc <- performance::check_collinearity(lm(mpg ~ wt + cyl, data = mtcars))
  # Supply the plotting contract directly, also with older performance versions.
  attr(cc, "data") <- data.frame(
    Term = c("factor", "numeric"), VIF = c(2, 6), df = c(11, 1)
  )
  attr(cc, "CI") <- data.frame(VIF_CI_low = c(1, 4), VIF_CI_high = c(3, 8))

  p <- plot(cc)
  expect_identical(
    p$labels$y,
    expression("Variance Inflation Factor (" * plain(GVIF)^{1 / plain(df)} * ", log-scaled)")
  )
  expect_equal(p$data$y, c(2, 6))
  expect_equal(p$data$VIF_CI_high, c(3, 8))
  expect_equal(as.character(p$data$group), c("Low (< 5)", "Moderate (< 10)"))

  # The check_model() panel uses the same label, split over two lines.
  panel <- .plot_diag_vif(
    p$data[, c("x", "y", "df")] |> transform(group = c("low", "moderate")),
    size_point = 3.5, size_line = 0.8, is_check_model = TRUE
  )
  expect_identical(panel$labels$y, expression(atop(
    "Variance Inflation",
    "Factor (" * plain(GVIF)^{1 / plain(df)} * ", log-scaled)"
  )))

  # Older results without df metadata keep the original label.
  attr(cc, "data")$df <- NULL
  expect_identical(plot(cc)$labels$y, "Variance Inflation Factor (VIF, log-scaled)")
})
