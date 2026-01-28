# Helper function to sample large datasets for performance
# See issue #420: https://github.com/easystats/see/issues/420
.sample_for_plot <- function(data, maximum_dots = 2000, ...) {
  if (is.null(data) || !is.data.frame(data)) {
    return(data)
  }

  n_obs <- nrow(data)

  if (is.null(maximum_dots)) {
    maximum_dots <- 2000
  }

  # Only sample if dataset exceeds threshold
  if (n_obs > maximum_dots) {
    # Use stratified sampling if there are grouping variables
    # Otherwise use simple random sampling
    set.seed(123) # For reproducibility in plots
    sample_indices <- sample.int(n_obs, maximum_dots, replace = FALSE)
    data <- data[sample_indices, , drop = FALSE]

    # Add attribute to track sampling
    attr(data, "was_sampled") <- TRUE
    attr(data, "original_n") <- n_obs
    attr(data, "sampled_n") <- maximum_dots
  }

  data
}


# small helper to set default theme for plots
.set_default_theme <- function(
  x,
  theme = NULL,
  base_size = 10,
  size_axis_title = 10,
  size_title = 12,
  default_theme = NULL
) {
  if (is.null(theme)) {
    theme <- attr(x, "theme")
  }
  if (!is.null(theme)) {
    if (is.character(theme)) {
      theme_parts <- unlist(strsplit(theme, "::", fixed = TRUE))
      if (length(theme_parts) == 2) {
        theme <- get(theme_parts[2], asNamespace(theme_parts[1]))
      } else {
        theme <- get(theme_parts[1], mode = "function")
      }
    } else if (!is.function(theme) || !"theme" %in% class(theme)) {
      insight::format_error(
        "Plot theme must be a function, or a string naming a theme function."
      )
    }
  } else if (is.null(default_theme)) {
    theme <- theme_lucid(
      base_size = base_size,
      plot.title.space = 3,
      axis.title.space = 5,
      axis.title.size = size_axis_title,
      plot.title.size = size_title
    )
  } else {
    theme <- default_theme
  }
  theme
}


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


.has_multiple_panels <- function(x) {
  (!"Effects" %in% names(x) || insight::n_unique(x$Effects) <= 1L) &&
    (!"Component" %in% names(x) || insight::n_unique(x$Component) <= 1L)
}


.clean_parameter_names <- function(params, grid = FALSE) {
  params <- unique(params)
  parameter_labels <- params

  # clean parameters names
  params <- gsub("(b_|bs_|bsp_|bcs_)(.*)", "\\2", params, perl = TRUE)
  params <- gsub("^cond_(.*)", "\\1 (Conditional)", params, perl = TRUE)
  params <- gsub("(.*)_cond$", "\\1 (Conditional)", params, perl = TRUE)
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
  params <- gsub(
    "(.*)__zi\\s(.*)",
    "\\1 \\2 (Zero-Inflated)",
    params,
    perl = TRUE
  )
  # fix temporary random effects token
  params <- gsub("\\(re\\)\\s(.*)", "\\1 (Random)", params, perl = TRUE)
  # correlation and sd: brms
  cor_sd <- grepl("(sd_|cor_)(.*)", params)
  if (any(cor_sd)) {
    params[cor_sd] <- paste(
      "SD/Cor: ",
      gsub("^(sd_|cor_)(.*?)__(.*)", "\\3", params[cor_sd], perl = TRUE)
    )
    # replace "__" by "~"
    cor_only <- !is.na(params[cor_sd]) & startsWith(params[cor_sd], "cor_")
    if (any(cor_only)) {
      params[cor_sd][which(cor_sd)[cor_only]] <- sub(
        "__",
        " ~ ",
        params[cor_sd][which(cor_sd)[cor_only]],
        fixed = TRUE
      )
    }
  }
  # correlation and sd: rstanarm
  cor_sd <- grepl("^Sigma\\[(.*)", params)
  if (any(cor_sd)) {
    parm1 <- gsub(
      "^Sigma\\[(.*):(.*),(.*)\\]",
      "\\2",
      params[cor_sd],
      perl = TRUE
    )
    parm2 <- gsub(
      "^Sigma\\[(.*):(.*),(.*)\\]",
      "\\3",
      params[cor_sd],
      perl = TRUE
    )
    params[which(cor_sd)] <- parm1
    rand_cor <- parm1 != parm2
    if (any(rand_cor)) {
      params[which(cor_sd)[rand_cor]] <- paste0(
        parm1[rand_cor],
        " ~ ",
        parm2[rand_cor]
      )
    }
    params[cor_sd] <- paste("SD: ", params[cor_sd])
  }

  if (grid) {
    params <- trimws(gsub("(Zero-Inflated)", "", params, fixed = TRUE))
    params <- trimws(gsub("(Random)", "", params, fixed = TRUE))
    params <- trimws(gsub("(Dispersion)", "", params, fixed = TRUE))
  } else {
    params <- gsub(
      "(Zero-Inflated) (Random)",
      "(Random, Zero-Inflated)",
      params,
      fixed = TRUE
    )
  }

  stats::setNames(params, parameter_labels)
}


.fix_facet_names <- function(x) {
  if ("Component" %in% names(x)) {
    x$Component <- as.character(x$Component)
    if ("Effects" %in% names(x)) {
      x$Component[x$Component == "conditional"] <- "(Conditional)"
      x$Component[x$Component == "zero_inflated"] <- "(Zero-Inflated)"
      x$Component[x$Component == "dispersion"] <- "(Dispersion)"
      x$Component[x$Component == "simplex"] <- "(Monotonic Effects)"
    } else {
      x$Component[x$Component == "conditional"] <- "Conditional"
      x$Component[x$Component == "zero_inflated"] <- "Zero-Inflated"
      x$Component[x$Component == "dispersion"] <- "Dispersion"
      x$Component[x$Component == "simplex"] <- "Monotonic Effects"
    }
  }

  if ("Effects" %in% names(x)) {
    x$Effects <- as.character(x$Effects)
    x$Effects[x$Effects == "fixed"] <- "Fixed Effects"
    x$Effects[x$Effects == "random"] <- "Random Effects"
  }

  x
}


.intercept_names <-
  c(
    "(intercept)_zi",
    "intercept (zero-inflated)",
    "intercept",
    "zi_intercept",
    "(intercept)",
    "b_intercept",
    "b_zi_intercept"
  )


.is_intercept <- function(x) {
  x <- tolower(x)
  x %in% .intercept_names | grepl("(?i)intercept[^a-zA-Z]", x)
}

.remove_intercept <- function(x, column = "Parameter", show_intercept = FALSE) {
  if (!show_intercept) {
    to_remove <- which(.is_intercept(x[[column]]))
    if (length(to_remove)) x <- x[-to_remove, ]
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
