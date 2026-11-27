test_that("plot.see_bayesfactor_models() labels pairwise probabilities in many-pies mode", {
  lm0 <- stats::lm(qsec ~ 1, data = mtcars)
  lm1 <- stats::lm(qsec ~ drat, data = mtcars)
  lm2 <- stats::lm(qsec ~ wt, data = mtcars)
  lm3 <- stats::lm(qsec ~ drat + wt, data = mtcars)
  result <- bayestestR::bayesfactor_models(lm1, lm2, lm3, denominator = lm0)

  p_many <- plot(result, n_pies = "many", value = "probability")
  dat <- p_many$data

  expected_labels <- insight::format_value(dat$bar_pos, 1, as_percent = TRUE)
  expect_identical(dat$label, expected_labels)

  # pairwise probabilities in each pie match BF / (1 + BF) and sum to 100%
  for (panel in unique(dat$panel)) {
    sub <- dat[dat$panel == panel, ]
    bf_model <- exp(result$log_BF[result$Model == panel])
    expected_pair <- c(1 / (1 + bf_model), bf_model / (1 + bf_model))
    expect_equal(sub$bar_pos, expected_pair, tolerance = 1e-10)
    expect_identical(
      sub$label,
      insight::format_value(expected_pair, 1, as_percent = TRUE)
    )
    pct <- as.numeric(sub("%", "", sub$label, fixed = TRUE))
    expect_equal(sum(pct), 100, tolerance = 0.1)
  }

  # custom prior_odds are reflected in pairwise labels
  p_po <- plot(
    result,
    n_pies = "many",
    value = "probability",
    prior_odds = c(2, 1, 0.5)
  )
  expect_identical(
    p_po$data$label,
    insight::format_value(p_po$data$bar_pos, 1, as_percent = TRUE)
  )

  # single-pie mode still labels overall posterior probabilities
  p_one <- plot(result, n_pies = "one", value = "probability")
  expect_identical(
    p_one$data$label,
    insight::format_value(p_one$data$PostProb, 1, as_percent = TRUE)
  )
})
