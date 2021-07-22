#' Golden Ratio
#'
#' Returns the golden ratio (1.618034...). Useful to easily obtain golden
#' proportions, for instance for a horizontal figure, if you want its height to
#' be 8, you can set its width to be `golden_ratio(8)`.
#'
#' @param x A number to be multiplied by the golden ratio. The default (`x = 1`)
#'   returns the value of the golden ratio.
#'
#' @examples
#' golden_ratio()
#' golden_ratio(10)
#' @export
golden_ratio <- function(x = 1) {
  x * (1 + sqrt(5)) / 2
}
