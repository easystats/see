# Suppress R CMD check note
# Namespace in Imports field not imported from: PKG
#   All declared Imports should be used.
.ignore_unused_imports <- function() {
  correlation::correlation()
  modelbased::estimate_expectation()
  performance::model_performance()
}
