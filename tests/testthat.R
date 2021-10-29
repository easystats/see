if (require("testthat")) {
  library(see)

  if (length(strsplit(packageDescription("see")$Version, "\\.")[[1]]) > 3) {
    Sys.setenv("RunAllseeTests" = "yes")
  } else {
    Sys.setenv("RunAllseeTests" = "no")
  }

  osx <- tryCatch(
    {
      si <- Sys.info()
      if (!is.null(si["sysname"])) {
        si["sysname"] == "Darwin" || grepl("^darwin", R.version$os)
      } else {
        FALSE
      }
    },
    error = function(e) {
      FALSE
    }
  )

  if (!osx) {
    test_check("see")
  }
}
