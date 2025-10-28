#' Plot method for Bayes Factors for model comparison
#'
#' The `plot()` method for the `bayestestR::bayesfactor_models()` function.
#' These plots visualize the **posterior probabilities** of the compared models.
#'
#' @param n_pies Number of pies.
#' @param value What value to display.
#' @param sort The behavior of this argument depends on the plotting contexts.
#' - *Plotting model parameters*:
#'   If `NULL`, coefficients are plotted in the order as they appear in the
#'   summary. Setting `sort = "ascending"` or `sort = "descending"` sorts
#'   coefficients in ascending or descending order, respectively.
#'   Setting `sort = TRUE` is the same as `sort = "ascending"`.
#' - *Plotting Bayes factors*:
#'   Sort pie-slices by posterior probability (descending)?
#' @param log Logical that decides whether to display log-transformed Bayes
#'   factors.
#' @param prior_odds An optional vector of prior odds for the models. See
#'   `BayesFactor::priorOdds`. As the size of the pizza slices corresponds to
#'   posterior probability (which is a function of prior probability and the
#'   Bayes Factor), custom `prior_odds` will change the slices' size.
#' @inheritParams data_plot
#'
#' @return A ggplot2-object.
#'
#' @examples
#' library(bayestestR)
#' library(see)
#'
#' lm0 <- lm(qsec ~ 1, data = mtcars)
#' lm1 <- lm(qsec ~ drat, data = mtcars)
#' lm2 <- lm(qsec ~ wt, data = mtcars)
#' lm3 <- lm(qsec ~ drat + wt, data = mtcars)
#'
#' result <- bayesfactor_models(lm1, lm2, lm3, denominator = lm0)
#'
#' plot(result, n_pies = "one", value = "probability", sort = TRUE) +
#'   scale_fill_pizza(reverse = TRUE)
#'
#' plot(result, n_pies = "many", value = "BF", log = TRUE) +
#'   scale_fill_pizza(reverse = FALSE)
#' @export
plot.see_bayesfactor_models <- function(
  x,
  n_pies = c("one", "many"),
  value = c("none", "BF", "probability"),
  sort = FALSE,
  log = FALSE,
  prior_odds = NULL,
  ...
) {
  if ("log_BF" %in% names(x) && !"BF" %in% names(x)) {
    x$BF <- exp(x$log_BF)
  }

  n_pies <- match.arg(n_pies)
  value <- match.arg(value)

  denominator <- attr(x, "denominator")
  denominator_name <- x$Model[denominator]

  priorOdds <- rep(1, nrow(x))
  po_txt <- ""
  if (!is.null(prior_odds)) {
    priorOdds[-denominator] <- prior_odds
    po_txt <- "NOTE: Slice sizes based on custom prior odds"
  }

  ## Prep data
  # One pie data
  one_pie_data <- as.data.frame(x)
  one_pie_data$postOdds <- priorOdds * one_pie_data$BF
  one_pie_data$PostProb <- (one_pie_data$postOdds / sum(one_pie_data$postOdds))
  if (isTRUE(sort)) {
    one_pie_data <- one_pie_data[
      order(one_pie_data$PostProb, decreasing = TRUE),
    ]
  }
  one_pie_data$Model <- factor(one_pie_data$Model, levels = one_pie_data$Model)

  # Two pie data
  many_pies_data <- one_pie_data[one_pie_data$Model != denominator_name, ]
  many_pies_data <- split(many_pies_data, many_pies_data$Model)
  many_pies_data <- lapply(many_pies_data, function(m) {
    m <- rbind(one_pie_data[one_pie_data$Model == denominator_name, ], m)
    m$panel <- m$Model[2]
    m$bar_pos <- m$PostProb / sum(m$PostProb)
    m
  })
  many_pies_data <- many_pies_data[names(many_pies_data) != denominator_name]
  many_pies_data <- do.call(rbind, many_pies_data)
  many_pies_data$Model <- factor(
    many_pies_data$Model,
    levels = levels(one_pie_data$Model)
  )
  many_pies_data$panel <- factor(
    many_pies_data$panel,
    levels = levels(one_pie_data$Model)
  )
  many_pies_data$panel <- droplevels(many_pies_data$panel)

  ## Labels
  if (value == "BF") {
    if (log) {
      one_pie_data$label <- insight::format_value(log(one_pie_data$BF), 2)
      many_pies_data$label <- insight::format_value(log(many_pies_data$BF), 2)
      po_txt <- paste0(po_txt, "\nLabels are log(BF).")
    } else {
      one_pie_data$label <- insight::format_value(one_pie_data$BF, 2)
      many_pies_data$label <- insight::format_value(many_pies_data$BF, 2)
    }
  } else if (value == "probability") {
    one_pie_data$label <- insight::format_value(
      one_pie_data$PostProb,
      1,
      as_percent = TRUE
    )
    many_pies_data$label <- insight::format_value(
      many_pies_data$PostProb,
      1,
      as_percent = TRUE
    )
  } else {
    one_pie_data$label <- ""
    many_pies_data$label <- ""
  }

  ## Plot
  if (n_pies == "one") {
    p <- ggplot(
      one_pie_data,
      aes(x = "", y = .data$PostProb, fill = .data$Model)
    )
  } else {
    p <- ggplot(
      many_pies_data,
      aes(x = "", y = .data$bar_pos, fill = .data$Model)
    ) +
      facet_wrap(~ .data$panel)
  }

  p +
    geom_bar(width = 1, stat = "identity", color = "white", linewidth = 0.5) +
    geom_text(
      aes(label = .data$label),
      position = position_stack(vjust = 0.5)
    ) +
    coord_polar("y", start = 0) +
    scale_y_continuous(expand = c(0, 0)) +
    labs(x = "", y = "", fill = "Model") +
    theme_void() +
    labs(caption = po_txt)
}
