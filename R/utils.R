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


## TODO remove once bayestestR >= 0.2.5 is on CRAN

#' @importFrom stats reshape
.reshape_ci <- function(x) {


  # Long to wide ----------------
  if ("CI_low" %in% names(x) & "CI_high" %in% names(x) & "CI" %in% names(x)) {

    ci_position <- which(names(x) == "CI")

    # Reshape
    if (length(unique(x$CI)) > 1) {

      if (!"Parameter" %in% names(x)) {
        x$Parameter <- x$CI
        remove_parameter <- TRUE
      } else {
        remove_parameter <- FALSE
      }

      x <- stats::reshape(
        x,
        idvar = "Parameter",
        timevar = "CI",
        direction = "wide",
        v.names = c("CI_low", "CI_high"),
        sep = "_"
      )
      row.names(x) <- NULL
      if (remove_parameter) x$Parameter <- NULL
    }

    # Replace at the right place
    ci_colname <- names(x)[c(grepl("CI_low_*", names(x)) | grepl("CI_high_*", names(x)))]
    colnames_1 <- names(x)[0:(ci_position - 1)][!names(x)[0:(ci_position - 1)] %in% ci_colname]
    colnames_2 <- names(x)[!names(x) %in% c(ci_colname, colnames_1)]
    x <- x[c(colnames_1, ci_colname, colnames_2)]


    # Wide to long --------------
  } else{

    if (!"Parameter" %in% names(x)) {
      x$Parameter <- 1:nrow(x)
      remove_parameter <- TRUE
    } else {
      remove_parameter <- FALSE
    }

    lows <- grepl("CI_low_*", names(x))
    highs <- grepl("CI_high_*", names(x))
    ci <- as.numeric(gsub("CI_low_", "", names(x)[lows]))
    if (paste0(ci, collapse = "-") != paste0(gsub("CI_high_", "", names(x)[highs]), collapse = "-")) {
      stop("Something went wrong in the CIs reshaping.")
      return(x)
    }
    if (sum(lows) > 1 & sum(highs) > 1) {
      low <- stats::reshape(
        x[!highs],
        direction = "long",
        varying = list(names(x)[lows]),
        sep = "_",
        timevar = "CI",
        v.names = "CI_low",
        times = ci
      )
      high <- stats::reshape(
        x[!lows],
        direction = "long",
        varying = list(names(x)[highs]),
        sep = "_",
        timevar = "CI",
        v.names = "CI_high",
        times = ci
      )
      x <- merge(low, high)
      x$id <- NULL
      x <- x[order(x$Parameter), ]
      row.names(x) <- NULL
      if (remove_parameter) x$Parameter <- NULL
    }

    # Replace at the right place
    ci_position <- which(lows)[1]
    ci_colname <- c("CI", "CI_low", "CI_high")
    colnames_1 <- names(x)[0:(ci_position - 1)][!names(x)[0:(ci_position - 1)] %in% ci_colname]
    colnames_2 <- names(x)[!names(x) %in% c(ci_colname, colnames_1)]
    x <- x[c(colnames_1, ci_colname, colnames_2)]
  }
  x
}
