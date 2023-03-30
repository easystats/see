# graphics engine changed in R 4.1, and so snapshots generated on
# previous R version won't be compatible
if (require("testthat") && require("vdiffr") && getRversion() > "4.1.0") {
  library(testthat)
  library(see)

  test_check("see")
}
