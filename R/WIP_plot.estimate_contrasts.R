# library(tidyverse)
# library(rstanarm)
# library(estimate)
# library(see)
#
# model <- stan_glm(Sepal.Width ~ Species, data=iris)
# contrasts <- estimate_contrasts(model)
# means <- estimate_means(model)
#
# means %>%
#   ggplot() +
#   geom_pointrange(data=means, aes(x=Species, y=Median, ymin=CI_low, ymax=CI_high))
#
#
# means
# contrasts
#
#
#
# contrasts$x <- NA
# for(level in unique(contrasts$Level1)){
#   contrasts[contrasts$Level1 == level, c("x")] <- means[means$Species == level, "Median"]
#
# }
#
#
# contrasts$y <- contrasts$Median + contrasts$x
# contrasts$ymin <- contrasts$CI_low + contrasts$x
# contrasts$ymax <- contrasts$CI_high + contrasts$x
#
#
# df <- data.frame(levels = c("A", "B", "C"),
#                  means = c(0, 1, 3))
# means %>%
#   ggplot() +
#   geom_ribbon(aes(x=Level1, ))
#   geom_pointrange(data=means, aes(x=Species, y=Median, ymin=CI_low, ymax=CI_high))
#
#
#
# data_triangle <- data.frame(group = c(1,1,1), polygon.x = c(2,4,4), polygon.y = c(1,1,3))
#
#   p <- ggplot()
#   p <- p + geom_polygon(
#     data = data_triangle
#     ,aes(
#       x=polygon.x
#       ,y=polygon.y
#       ,group=group
#     )
#   )
#   p
