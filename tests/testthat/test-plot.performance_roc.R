test_that("`plot.see_performance_roc()` works", {
  library(performance)
  set.seed(123)
  iris$y <- rbinom(nrow(iris), size = 1, 0.3)
  folds <- sample(nrow(iris), size = nrow(iris) / 8, replace = FALSE)
  test_data <- iris[folds, ]
  train_data <- iris[-folds, ]
  model <- glm(y ~ Sepal.Length + Sepal.Width,
    data = train_data,
    family = "binomial"
  )
  result <- performance_roc(model, new_data = test_data)

  expect_s3_class(plot(result), "gg")
})
