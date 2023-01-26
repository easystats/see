# defining global variables and functions to appease R CMD Check

utils::globalVariables(
  names = c(
    ".",
    "CI_high",
    "CI_low",
    "estimate",
    "Estimate",
    "grp",
    "Parameter",
    "predictor",
    "ROPE_Equivalence"
  )
)
