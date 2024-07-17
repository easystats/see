# graphics engine changed in R 4.3, and so snapshots generated on
# previous R version won't work with later R releases
if (require("testthat", quietly = TRUE) && require("vdiffr", quietly = TRUE) && getRversion() >= "4.4.0") {
  library(testthat)
  library(see)

  test_check("see")
}
