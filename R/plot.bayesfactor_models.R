#' @export
plot.see_bayesfactor_models <- function(x, n_pies = c("one","many"), value = c("BF","Probability"), log = FALSE) {
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
    ggplot(plot_data, aes(x = "", y = PostProb, fill = Model)) +
      geom_bar(width = 1, stat = "identity") +
      coord_polar("y", start = 0) +
      labs(x = "", y = "") +
      if (value == "BF") {
        geom_text(aes(y = pos_txt, label = round(BF,2)))
      } else {
        geom_text(aes(y = pos_txt, label = round(PostProb*100,1)))
      }

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


    ggplot(plot_data2, aes(x = "", y = pos_bar, fill = Type)) +
      geom_bar(width = 1, stat = "identity") +
      coord_polar("y", start = 0) +
      facet_wrap(~Model) +
      labs(x = "", y = "") +
      if (value == "BF") {
        geom_text(aes(y = pos_txt, label = round(BF,2)))
      } else {
        geom_text(aes(y = pos_txt, label = round(PostProb*100,1)))
      }
  }
}
