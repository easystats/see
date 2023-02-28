.str_to_sym <- function(x) {
  insight::check_if_installed("rlang")
  if (!is.null(x) && is.character(x)) {
    x <- rlang::sym(x)
  }

  return(x)
}

.as.data.frame_density <- function(x, ...) {
  data.frame(x = x$x, y = x$y)
}

# safe conversion from factor to numeric
.factor_to_numeric <- function(x) {
  if (is.numeric(x)) {
    return(x)
  }

  if (anyNA(suppressWarnings(as.numeric(as.character(stats::na.omit(x)))))) {
    if (is.character(x)) {
      x <- as.factor(x)
    }
    levels(x) <- 1:nlevels(x)
  }

  as.numeric(as.character(x))
}



.clean_parameter_names <- function(params, grid = FALSE) {
  params <- unique(params)
  labels <- params

  # clean parameters names
  params <- gsub("(b_|bs_|bsp_|bcs_)(.*)", "\\2", params, perl = TRUE)
  params <- gsub("^zi_(.*)", "\\1 (Zero-Inflated)", params, perl = TRUE)
  params <- gsub("(.*)_zi$", "\\1 (Zero-Inflated)", params, perl = TRUE)
  params <- gsub("(.*)_disp$", "\\1 (Dispersion)", params, perl = TRUE)
  # clean random effect parameters names
  params <- gsub("r_(.*)\\.(.*)\\.", "(re) \\1", params)
  params <- gsub("b\\[\\(Intercept\\) (.*)\\]", "(re) \\1", params)
  params <- gsub("b\\[(.*) (.*)\\]", "(re) \\2", params)
  # clean smooth terms
  params <- gsub("^smooth_sd\\[(.*)\\]", "\\1 (smooth)", params)
  params <- gsub("^sds_", "\\1 (Smooth)", params)
  # remove ".1" etc. suffix
  params <- gsub("(.*)(\\.)(\\d)$", "\\1 \\3", params)
  # fix zero-inflation part in random effects
  params <- gsub("(.*)__zi\\s(.*)", "\\1 \\2 (Zero-Inflated)", params, perl = TRUE)
  # fix temporary random effects token
  params <- gsub("\\(re\\)\\s(.*)", "\\1 (Random)", params, perl = TRUE)
  # correlation and sd: brms
  cor_sd <- grepl("(sd_|cor_)(.*)", params)
  if (any(cor_sd)) {
    params[cor_sd] <- paste("SD/Cor: ", gsub("^(sd_|cor_)(.*?)__(.*)", "\\3", params[cor_sd], perl = TRUE))
    # replace "__" by "~"
    cor_only <- !is.na(params[cor_sd]) & startsWith(params[cor_sd], "cor_")
    if (any(cor_only)) {
      params[cor_sd][which(cor_sd)[cor_only]] <- sub("__", " ~ ", params[cor_sd][which(cor_sd)[cor_only]], fixed = TRUE)
    }
  }
  # correlation and sd: rstanarm
  cor_sd <- grepl("^Sigma\\[(.*)", params)
  if (any(cor_sd)) {
    parm1 <- gsub("^Sigma\\[(.*):(.*),(.*)\\]", "\\2", params[cor_sd], perl = TRUE)
    parm2 <- gsub("^Sigma\\[(.*):(.*),(.*)\\]", "\\3", params[cor_sd], perl = TRUE)
    params[which(cor_sd)] <- parm1
    rand_cor <- parm1 != parm2
    if (any(rand_cor)) {
      params[which(cor_sd)[rand_cor]] <- paste0(parm1[rand_cor], " ~ ", parm2[rand_cor])
    }
    params[cor_sd] <- paste("SD: ", params[cor_sd])
  }


  if (grid) {
    params <- trimws(gsub("(Zero-Inflated)", "", params, fixed = TRUE))
    params <- trimws(gsub("(Random)", "", params, fixed = TRUE))
    params <- trimws(gsub("(Dispersion)", "", params, fixed = TRUE))
  } else {
    params <- gsub("(Zero-Inflated) (Random)", "(Random, Zero-Inflated)", params, fixed = TRUE)
  }

  stats::setNames(params, labels)
}



.fix_facet_names <- function(x) {
  if ("Component" %in% names(x)) {
    x$Component <- as.character(x$Component)
    if (!"Effects" %in% names(x)) {
      x$Component[x$Component == "conditional"] <- "Conditional"
      x$Component[x$Component == "zero_inflated"] <- "Zero-Inflated"
      x$Component[x$Component == "dispersion"] <- "Dispersion"
      x$Component[x$Component == "simplex"] <- "Monotonic Effects"
    } else {
      x$Component[x$Component == "conditional"] <- "(Conditional)"
      x$Component[x$Component == "zero_inflated"] <- "(Zero-Inflated)"
      x$Component[x$Component == "dispersion"] <- "(Dispersion)"
      x$Component[x$Component == "simplex"] <- "(Monotonic Effects)"
    }
  }
  if ("Effects" %in% names(x)) {
    x$Effects <- as.character(x$Effects)
    x$Effects[x$Effects == "fixed"] <- "Fixed Effects"
    x$Effects[x$Effects == "random"] <- "Random Effects"
  }
  x
}



.intercepts <- function() {
  c(
    "(intercept)_zi",
    "intercept (zero-inflated)",
    "intercept",
    "zi_intercept",
    "(intercept)",
    "b_intercept",
    "b_zi_intercept"
  )
}


.has_intercept <- function(x) {
  x <- tolower(x)
  x %in% .intercepts() | !is.na(x) & startsWith(x, "intercept")
}


.in_intercepts <- .has_intercept


.remove_intercept <- function(x, column = "Parameter", show_intercept = FALSE) {
  if (!show_intercept) {
    remove <- which(.in_intercepts(x[[column]]))
    if (length(remove)) x <- x[-remove, ]
  }
  x
}


.percents <- function(x) {
  insight::format_value(x = x, as_percent = TRUE, digits = 0L)
}


.is_integer <- function(x) {
  is.numeric(x) && all(floor(x) == x, na.rm = TRUE)
}


#' Default value for `NULL`
#'
#' @param x,y If `x` is NULL, will return `y`; otherwise returns `x`.
#' @name op-null-default
#' @examples
#' 4 %||% 5
#' NULL %||% 1
#' @keywords internal
#' @noRd
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}
