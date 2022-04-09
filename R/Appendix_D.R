#' Reproduce Figure S8
#' @export
plot_figure_S8 <- function() {
  df_statistics %>%
    filter(generalization_measure == "Normalized degree") %>%
    mutate(level = ifelse(level == "animal", "Hummingbird", "Plant")) %>%
    mutate(class = fct_recode(class,
      "Rare-Specialized" = "RS",
      "Rare-Generalized" = "RG",
      "Abundant-Specialized" = "AS",
      "Abundant-Generalized" = "AG"
    )) %>%
    grouped_ggwithinstats(
      x = class,
      y = n,
      type = "np",
      grouping.var = level,
      xlab = "",
      ylab = "Mean proportion\nin an empirical community",
      centrality.plotting = F
    )
}

#' Reproduce Figure S9
#' @export
plot_figure_S9 <- function() {
  df_statistics %>%
    filter(generalization_measure == "Normalized degree") %>%
    mutate(level = ifelse(level == "animal", "Hummingbird", "Plant")) %>%
    mutate(level = as.factor(level)) %>%
    filter(class %in% c("RG", "AS")) %>%
    mutate(class = fct_recode(class,
      "Rare-Specialized" = "RS",
      "Rare-Generalized" = "RG",
      "Abundant-Specialized" = "AS",
      "Abundant-Generalized" = "AG"
    )) %>%
    ggstatsplot::grouped_ggwithinstats(
      x = class,
      y = n,
      type = "np",
      grouping.var = level,
      xlab = "",
      ylab = "Mean proportion\nin an empirical community",
      centrality.plotting = F,
      results.subtitle = T,
      ggplot.component = ggplot2::scale_color_grey()
    ) +
    jtools::theme_nice() +
    theme(
      legend.position = "none"
    )
}
