# graphics engine changed in R 4.4, and so snapshots generated on
# previous R version won't work
if (getRversion() >= "4.4.0") {
  library(testthat)
  library(see)

  test_check("see")
}
