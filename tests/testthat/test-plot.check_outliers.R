test_that("`plot.see_check_outliers()` works", {
  library(performance)
  mt1 <- mtcars[, c(1, 3, 4)]
  mt2 <- rbind(mt1, data.frame(
    mpg = c(37, 40), disp = c(300, 400),
    hp = c(110, 120)
  ))
  model <- lm(disp ~ mpg + hp, data = mt2)
  expect_s3_class(plot(check_outliers(model)), "gg")
})
