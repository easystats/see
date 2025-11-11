# Plot method for ROC curves

The [`plot()`](https://rdrr.io/r/graphics/plot.default.html) method for
the
[`performance::performance_roc()`](https://easystats.github.io/performance/reference/performance_roc.html)
function.

## Usage

``` r
# S3 method for class 'see_performance_roc'
plot(x, ...)
```

## Arguments

- x:

  An object.

- ...:

  Arguments passed to or from other methods.

## Value

A ggplot2-object.

## Examples

``` r
library(performance)
data(iris)
set.seed(123)
iris$y <- rbinom(nrow(iris), size = 1, .3)

folds <- sample(nrow(iris), size = nrow(iris) / 8, replace = FALSE)
test_data <- iris[folds, ]
train_data <- iris[-folds, ]

model <- glm(y ~ Sepal.Length + Sepal.Width, data = train_data, family = "binomial")
result <- performance_roc(model, new_data = test_data)
result
#> AUC: 37.66%
plot(result)
```
