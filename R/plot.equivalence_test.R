#' @importFrom ggridges geom_density_ridges2
#' @export
plot.see_equivalence_test <- function(x, rope_color = "#0171D3", rope_alpha = .2, ...) {
  model_name <- attr(x, "object_name", exact = TRUE)

  if (is.null(model_name)) {
    warning("plot() only works for equivalence_test() with model-objects.", call. = FALSE)
    return(x)
  }


  # retrieve model
  model <- tryCatch({
    get(model_name, envir = parent.frame())
  },
  error = function(e) {
    NULL
  }
  )

  if (is.null(model)) {
    warning(sprintf("Can't find object '%s'.", model_name))
    return(x)
  }


  # if we have intercept-only models, keep at least the intercept
  intercepts <- which(x$Parameter %in% c("Intercept", "(Intercept)", "b_Intercept"))
  if (length(intercepts) && nrow(x) > length(intercepts)) {
    x <- x[-intercepts, ]
  }

  .rope <- c(x$ROPE_low[1], x$ROPE_high[1])

  # split for multiple CIs
  tests <- split(x, x$CI)

  result <- lapply(tests, function(i) {
    tmp <- as.data.frame(model, stringsAsFactors = FALSE)[, i$Parameter, drop = FALSE]

    tmp2 <- lapply(1:nrow(i), function(j) {
      p <- i$Parameter[j]
      tmp[[p]][tmp[[p]] < i$HDI_low[j]] <- NA
      tmp[[p]][tmp[[p]] > i$HDI_high[j]] <- NA
      tmp[[p]]
    })

    cnames <- colnames(tmp)
    tmp <- as.data.frame(tmp2)
    colnames(tmp) <- cnames

    tmp <- .to_long(tmp, names_to = "predictor", values_to = "estimate")
    # tmp$predictor <- as.factor(tmp$predictor)

    tmp$grp <- NA
    for (j in 1:nrow(i)) {
      tmp$grp[tmp$predictor == i$Parameter[j]] <- i$ROPE_Equivalence[j]
    }

    tmp$predictor <- factor(tmp$predictor)
    tmp$predictor <- factor(tmp$predictor, levels = rev(levels(tmp$predictor)))

    tmp$HDI <- sprintf("%i%% HDI", i$CI[1])

    tmp
  })

  tmp <- do.call(rbind, result)

  # check for user defined arguments

  fill.color <- c("#CD423F", "#018F77", "#FCDA3B")
  x.title <- sprintf("%i%% Highest Density Region of Posterior Samples", x$CI[1])
  legend.title <- "Decision on H0"
  labels <- levels(tmp$predictor)
  names(labels) <- labels

  fill.color <- fill.color[sort(unique(match(x$ROPE_Equivalence, c("accepted", "rejected", "undecided"))))]

  add.args <- lapply(match.call(expand.dots = F)$`...`, function(x) x)
  if ("colors" %in% names(add.args)) fill.color <- eval(add.args[["colors"]])
  if ("x.title" %in% names(add.args)) x.title <- eval(add.args[["x.title"]])
  if ("legend.title" %in% names(add.args)) legend.title <- eval(add.args[["legend.title"]])
  if ("labels" %in% names(add.args)) labels <- eval(add.args[["labels"]])

  rope.line.alpha <- 1.25 * rope_alpha
  if (rope.line.alpha > 1) rope.line.alpha <- 1


  p <- ggplot(tmp, aes_string(x = "estimate", y = "predictor", fill = "grp")) +
    annotate("rect", xmin = .rope[1], xmax = .rope[2], ymin = 0, ymax = Inf, fill = rope_color, alpha = rope_alpha) +
    geom_vline(xintercept = 0, colour = rope_color, size = .8, alpha = rope.line.alpha) +
    ggridges::geom_density_ridges2(rel_min_height = 0.01, scale = 2, alpha = .5) +
    scale_fill_manual(values = fill.color) +
    labs(x = x.title, y = NULL, fill = legend.title) +
    scale_y_discrete(labels = labels) +
    theme(legend.position = "bottom")

  if (length(unique(tmp$HDI)) > 1) {
    p <- p + facet_wrap(~HDI, scales = "free")
  }

  p
}



#' @importFrom stats reshape
#' @keywords internal
.to_long <- function(x, names_to = "key", values_to = "value", columns = colnames(x)) {
  if (is.numeric(columns)) columns <- colnames(x)[columns]
  dat <- stats::reshape(
    as.data.frame(x),
    idvar = "id",
    ids = row.names(x),
    times = columns,
    timevar = names_to,
    v.names = values_to,
    varying = list(columns),
    direction = "long"
  )

  if (is.factor(dat[[values_to]])) {
    dat[[values_to]] <- as.character(dat[[values_to]])
  }

  dat[, 1:(ncol(dat) - 1), drop = FALSE]
}
