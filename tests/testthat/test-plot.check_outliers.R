test_that("`plot.see_check_outliers()` works", {
  mt1 <- mtcars[, c(1, 3, 4)]
  mt2 <- rbind(
    mt1,
    data.frame(
      mpg = c(37, 40),
      disp = c(300, 400),
      hp = c(110, 120)
    )
  )
  model <<- stats::lm(disp ~ mpg + hp, data = mt2)
  expect_s3_class(plot(performance::check_outliers(model, verbose = FALSE)), "gg")
})
