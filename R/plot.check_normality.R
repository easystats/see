#' @importFrom bayestestR estimate_density
#' @importFrom stats residuals sd dnorm rstudent ppoints pnorm fitted
#' @rdname data_plot
#' @export
plot.see_check_normality <- function(x, type = c("density", "qq", "pp"), data = NULL, size = .8, point_size = 2, ...) {
  type <- match.arg(type)

  if (is.null(data)) {
    model <- .retrieve_data(x)
  } else {
    model <- data
  }

  # check type
  if (!is.null(attributes(x)$effects) && attributes(x)$effects == "random") {
    .plot_diag_reqq(attributes(x)$re_qq, point_size = point_size, line_size = size)
  } else {
    if (type == "qq") {
      if (inherits(model, c("lme", "lmerMod", "merMod", "glmmTMB"))) {
        res_ <- sort(stats::residuals(model), na.last = NA)
      } else {
        res_ <- sort(stats::rstudent(model), na.last = NA)
      }

      fitted_ <- sort(stats::fitted(model), na.last = NA)
      dat <- stats::na.omit(data.frame(x = fitted_, y = res_))
      .plot_diag_qq(dat, point_size = point_size, line_size = size)
    } else if (type == "density") {
      r <- stats::residuals(model)
      dat <- as.data.frame(bayestestR::estimate_density(r))
      dat$curve <- stats::dnorm(
        seq(min(dat$x), max(dat$x), length.out = nrow(dat)),
        mean(r),
        stats::sd(r)
      )
      .plot_diag_norm(dat, line_size = size)
    } else if (type == "pp") {
      if (!requireNamespace("MASS", quietly = TRUE)) {
        stop("Package 'MASS' required for PP-plots. Please install it.", call. = FALSE)
      }
      x <- sort(stats::residuals(model), na.last = NA)
      probs <- stats::ppoints(x)
      dparms <- MASS::fitdistr(x, densfun = "normal")
      y <- do.call(stats::pnorm, c(list(q = x), dparms$estimate))
      dat <- data.frame(x = probs, y = y)
      .plot_diag_pp(dat, point_size = point_size, line_size = size)
    }
  }
}

