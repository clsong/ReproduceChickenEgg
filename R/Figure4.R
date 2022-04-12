#' Reproduce Figure 4 (note that the reproduced figure lacks annotations
#' in the main text; these annotations were added via the software Illustrator)
#'
#' @export
plot_figure_4 <- function() {
  p2 <-
    environment_variability %>%
    left_join(select(df_NODF, web, NODFc, AS, RG, level)) %>%
    mutate(diff = AS - RG) %>%
    mutate(level = ifelse(level == "animal", "Hummingbird", "Plant")) %>%
    drop_na() %>%
    group_by(web, NODFc, level, diff) %>%
    summarise(
      sd = sd(Temp_mean),
      Temp = mean(Temp_mean)
    ) %>%
    ggplot(aes(Temp, diff, color = level, fill = level)) +
    geom_abline(slope = 0, intercept = 0, color = "black", size = 1) +
    geom_point(size = 3) +
    scale_color_manual(values = c("#FFBD20", "dodgerblue")) +
    scale_fill_manual(values = c("#FFBD20", "dodgerblue")) +
    geom_errorbarh(aes(xmin = Temp - sd, xmax = Temp + sd)) +
    facet_wrap(level ~ ., ncol = 1, scales = "free") +
    stat_cor() +
    labs(
      x = "Mean temperature",
      y = "Difference between % of Abundant-Specialized \nand % of Rare-Generalized"
    ) +
    geom_smooth(method = "lm", size = 1.5) +
    jtools::theme_nice() +
    theme(
      text = element_text(size = 20),
      axis.title.y = element_text(size = 16),
      axis.title.x = element_text(size = 16),
      legend.position = "none"
    )

  p1 <-
    df_NODF %>%
    mutate(level = ifelse(level == "animal", "Hummingbird", "Plant")) %>%
    drop_na() %>%
    ggplot(aes(NODFc, AS - RG, color = level, fill = level)) +
    geom_abline(slope = 0, intercept = 0, color = "black", size = 1) +
    geom_point(size = 3) +
    scale_color_manual(values = c("#FFBD20", "dodgerblue")) +
    scale_fill_manual(values = c("#FFBD20", "dodgerblue")) +
    facet_wrap(level ~ ., ncol = 1, scales = "free") +
    stat_cor() +
    labs(
      x = "Combined nestedness (NODFc)",
      y = "Difference between % of Abundant-Specialized \nand % of Rare-Generalized"
    ) +
    geom_smooth(method = "lm", size = 1.5) +
    jtools::theme_nice() +
    theme(
      text = element_text(size = 20),
      axis.title.y = element_text(size = 16),
      axis.title.x = element_text(size = 16),
      legend.position = "none"
    )

  p1 + p2 & theme(aspect.ratio = 1)
}
