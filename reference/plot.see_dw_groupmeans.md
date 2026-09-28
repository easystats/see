# Plot method for grouped means.

The [`plot()`](https://rdrr.io/r/graphics/plot.default.html) method for
the
[`datawizard::means_by_group()`](https://easystats.github.io/datawizard/reference/means_by_group.html)
function.

## Usage

``` r
# S3 method for class 'see_dw_groupmeans'
plot(x, title = "", ci = TRUE, caption = TRUE, ...)
```

## Arguments

- x:

  An object returned
  [`datawizard::means_by_group()`](https://easystats.github.io/datawizard/reference/means_by_group.html).

- title:

  String, can be used to specify a plot title.

- ci:

  Logical, indicating if the confidence intervals should be included in
  the graph.

- caption:

  Logical, indicating if a caption summarizing the anova results for the
  analysis should be included.

- ...:

  Currently not used.

## Details

Produces a faceted plot when there is more than one means-table.

## Examples

``` r
# \dontrun{
group_means_object <-  datawizard::means_by_group(iris$Sepal.Width, iris$Species)
plot(group_means_object, title = "group means", ci = FALSE, caption = FALSE)


group_means_object <- datawizard::means_by_group(
  iris,
  c("Sepal.Width", "Petal.Width"),
  "Species"
)
plot(group_means_object, title = "group means")

# }
```
