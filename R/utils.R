#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`


.as.data.frame_density <- function(x, ...) {
  data.frame(x = x$x, y = x$y)
}



.remove_intercept <- function(x, column = "Parameter", show_intercept) {
  if (!show_intercept) {
    remove <- which(x[[column]] %in% c("Intercept", "(Intercept)", "b_Intercept"))
    if (length(remove)) x <- x[-remove, ]
  }
  x
}



.normalize <- function(x) {
  as.vector((x - min(x, na.rm = TRUE)) / diff(range(x, na.rm = TRUE), na.rm = TRUE))
}



.compact_list <- function(x) {
  if (!is.null(x) && length(x) > 0 && is.list(x)) {
    x[!sapply(x, function(i) length(i) == 0 || is.null(i) || any(i == "NULL"))]
  } else {
    x
  }
}



# is string empty?
.is_empty_object <- function(x) {
  if (is.list(x)) {
    x <- tryCatch(
      {.compact_list(x)},
      error = function(x) { x }
    )
  }
  # this is an ugly fix because of ugly tibbles
  if (inherits(x, c("tbl_df", "tbl"))) x <- as.data.frame(x)
  x <- suppressWarnings(x[!is.na(x)])
  length(x) == 0 || is.null(x)
}




# safe conversion from factor to numeric
#' @importFrom stats na.omit
.factor_to_numeric <- function(x) {
  if (is.numeric(x))
    return(x)

  if (anyNA(suppressWarnings(as.numeric(as.character(stats::na.omit(x)))))) {
    if (is.character(x)) {
      x <- as.factor(x)
    }
    levels(x) <- 1:nlevels(x)
  }

  as.numeric(as.character(x))
}
