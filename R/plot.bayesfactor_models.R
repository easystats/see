#' @export
plot.see_bayesfactor_models <- function(x, n_pies = c("one","many"), value = c("none","BF","probability"), log = FALSE) {
  n_pies <- match.arg(n_pies)
  value <- match.arg(value)

  plot_data <- as.data.frame(x) %>%
    within({
      Model <- factor(Model, levels = Model)
      PostProb <- BF / sum(BF)
      pos_txt <- sum(PostProb) + PostProb / 2 - cumsum(PostProb)
    })

  if (log) {
    plot_data$BF <- log(plot_data$BF)
  }

  if (n_pies == "one") {
    if (value == "BF") {
      plot_data$label <- round(plot_data$BF,2)
    } else if (value == "probability") {
      plot_data$label <- round(plot_data$PostProb*100,1)
    } else {
      plot_data$label <- ""
    }

    plot_data$Model <- factor(plot_data$Model, levels = unique(plot_data$Model))

    ggplot(plot_data, aes(x = "", y = .data$PostProb, fill = .data$Model)) +
      geom_bar(width = 1, stat = "identity", color = "black", size = 1) +
      geom_text(aes(y = .data$pos_txt, label = .data$label), position = position_nudge(.1)) +
      coord_polar("y", start = 0) +
      scale_y_continuous(expand = c(0, 0)) +
      labs(x = "", y = "", fill = "Model")
  } else {
    denominator <- attr(x,"denominator")
    denominator_name <- x$Model[denominator]

    plot_data2 <- plot_data %>%
      within({
        Type <- denominator_name
        BF <- BF[denominator]
        PostProb <- PostProb[denominator]
      }) %>%
      rbind(within(plot_data,Type <- Model)) %>%
      split(.$Model) %>%
      lapply(function(x) {
        x$pos_bar <- x$PostProb/sum(x$PostProb)
        x$pos_txt <- sum(x$pos_bar) + x$pos_bar / 2 - cumsum(x$pos_bar)
        x
      }) %>%
      do.call("rbind",.) %>%
      .[.$Model != denominator_name,]

    plot_data2$Type <- factor(plot_data2$Type, levels = unique(plot_data2$Type))

    if (value == "BF") {
      plot_data2$label <- round(plot_data2$BF,2)
    } else if (value == "probability") {
      plot_data2$label <- round(plot_data2$PostProb*100,1)
    } else {
      plot_data2$label <- ""
    }

    ggplot(plot_data2, aes(x = "", y = .data$pos_bar, fill = .data$Type)) +
      geom_bar(width = 1, stat = "identity", color = "black", size = 1) +
      geom_text(aes(y = .data$pos_txt, label = .data$label), position = position_nudge(.1)) +
      coord_polar("y", start = 0) +
      facet_wrap( ~ .data$Model) +
      labs(x = "", y = "", fill = "Model")
  }
}
