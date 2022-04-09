#' Reproduce Figure S1
#' @export
plot_figure_S1 <- function() {
  df_cor <- df %>%
    drop_na() %>%
    filter(!is.infinite(generalization)) %>%
    group_by(generalization_measure, level) %>%
    mutate(abundance_normalized = normalize(abundance, method = "normal")) %>%
    mutate(generalization_normalized = normalize(generalization, method = "normal")) %>%
    ungroup()

  df_cor %>%
    nest(data = c(-generalization_measure, -level)) %>%
    mutate(cor_normalize = map(data, function(x) {
      cor.test(x$abundance_normalized, x$generalization_normalized, method = "pearson", conf.level = 0.9)
    })) %>%
    mutate(cor_original = map(data, function(x) {
      cor.test(log(x$abundance + 1), x$generalization, method = "pearson", conf.level = 0.9)
    })) %>%
    select(-data) %>%
    gather(key, value, -level, -generalization_measure) %>%
    rowwise() %>%
    mutate(
      cor = value$estimate,
      cor.max = value$conf.int[2],
      cor.min = value$conf.int[1]
    ) %>%
    ungroup() %>%
    separate(key, c("dumb", "Date type"), sep = "_") %>%
    select(-dumb, -value) %>%
    mutate(
      level = ifelse(level == "animal", "Hummingbird", "Plant"),
      `Date type` = ifelse(`Date type` == "original", "Original data", "Normalized data")
    ) %>%
    ggplot(aes(x = cor, y = generalization_measure)) +
    geom_point(size = 5) +
    geom_vline(xintercept = 0, color = "red") +
    geom_errorbarh(aes(xmin = cor.min, xmax = cor.max), height = .2) +
    facet_grid(`Date type` ~ level) +
    labs(
      x = "Pearson correlation with abundance",
      y = ""
    ) +
    jtools::theme_nice() +
    theme(
      panel.spacing = unit(2, "lines"),
      text = element_text(size = 20)
    )
}
