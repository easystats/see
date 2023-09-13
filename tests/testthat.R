# graphics engine changed in these versions, and so snapshots generated on
# previous R version won't work
if (require("testthat") && require("vdiffr") && getRversion() > "4.1.0" && getRversion() < "4.4.0") {
  library(testthat)
  library(see)

  test_check("see")
}
