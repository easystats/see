#' Plot method for homogeneity of variances checks
#'
#' The \code{plot()} method for the \code{performance::check_homogeneity()} function.
#'
#' @inheritParams data_plot
#'
#' @return A ggplot2-object.
#'
#' @examples
#' library(performance)
#' model <<- lm(len ~ supp + dose, data = ToothGrowth)
#' result <- check_homogeneity(model)
#' result
#' plot(result)
#' @importFrom insight get_data find_predictors get_response
#' @importFrom rlang .data
#' @export
plot.see_check_homogeneity <- function(x, data = NULL, ...) {
  if (is.null(data)) {
    model <- .retrieve_data(x)
  } else {
    model <- data
  }

  method <- paste0("Homogeneity of Variance (", attr(x, "method"), ")")

  dat <- insight::get_data(model)
  resp <- insight::get_response(model)
  pred <- insight::find_predictors(model, flatten = TRUE)

  if (length(pred) > 1) {
    l <- lapply(dat[, pred], as.character)
    for (i in pred[1:(length(pred) - 1)]) l[[i]] <- sprintf("%s*", l[[i]])
    x <- do.call(c, l)
    group_labels <- do.call(paste0, l)
    group <- rep(group_labels, each = length(pred))
    resp <- rep(resp, length(pred))
  } else {
    x <- as.character(as.vector(dat[, pred, drop = TRUE]))
    group <- 1
  }

  dat <- data.frame(
    x = x,
    y = resp,
    group = group,
    stringsAsFactors = FALSE
  )


  p <- if (length(pred) > 1)
    ggplot(data = dat, aes(x = .data$x, y = .data$y, fill = .data$group)) + geom_violin()
  else
    ggplot(data = dat, aes(x = .data$x, y = .data$y)) + geom_violin(fill = "#2980b9")

  p +
    scale_fill_flat_d() +
    scale_x_discrete(labels = NULL) +
    theme_modern() +
    labs(x = NULL, y = insight::find_response(model), fill = NULL, title = method)
}