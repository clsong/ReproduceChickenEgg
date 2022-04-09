#' Reproduce Figure 3 (note that the reproduced figure lacks annotations
#' in the main text; these annotations were added via the software Illustrator)
#'
#' @export
plot_figure_3 <- function() {
  df_plots <-
    df_binarized %>%
    group_by(web, generalization_measure, method_generalization, method_abundance, level) %>%
    mutate(n = n / sum(n)) %>%
    ungroup() %>%
    filter(generalization_measure == "Normalized degree") %>%
    filter(method_generalization == method_abundance) %>%
    mutate(level = ifelse(level == "animal", "Hummingbird", "Plant")) %>%
    mutate(level = as.factor(level)) %>%
    filter(class %in% c("RG", "AS")) %>%
    mutate(class = fct_recode(class,
      "Rare-Specialized" = "RS",
      "Rare-Generalized" = "RG",
      "Abundant-Specialized" = "AS",
      "Abundant-Generalized" = "AG"
    )) %>%
    nest(data = -c(method_generalization, level)) %>%
    mutate(plot = map2(data, level, function(data, level) {
      if (level == "Hummingbird") {
        plot <- ggstatsplot::ggwithinstats(
          data = data,
          x = class,
          y = n,
          type = "np",
          xlab = "",
          ylab = "Mean proportion\nin an empirical community",
          centrality.plotting = F,
          results.subtitle = F
        ) +
          scale_color_manual(values = rep("#FFBD20", 2))
      } else {
        plot <- ggstatsplot::ggwithinstats(
          data = data,
          x = class,
          y = n,
          type = "np",
          xlab = "",
          ylab = "Mean proportion\nin an empirical community",
          centrality.plotting = F,
          results.subtitle = F
        ) +
          scale_color_manual(values = rep("dodgerblue", 2))
      }

      plot
    })) %>%
    pull(plot)

  (df_plots[[1]] + df_plots[[2]]) / (ggplot() +
    theme_void()) / (df_plots[[3]] + df_plots[[4]]) +
    plot_layout(heights = c(5, .3, 5)) +
    jtools::theme_nice() &
    theme(
      legend.position = "none",
      text = element_text(size = 15)
    )
}
