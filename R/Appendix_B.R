#' Reproduce Figure S2
#' @export
plot_figure_S2 <- function() {
  Npoint <- 100000
  noise <- function(x) {
    1 / (1 + exp(-1)) - 1 / (1 + exp(-x))
  }
  data_uniform <-
    tibble(x = runif(n = Npoint, min = 0, max = 1)) %>%
    rowwise() %>%
    mutate(y = runif(1, min = x - noise(x), max = x + noise(x))) %>%
    ungroup() %>%
    mutate(label_sample = row_number()) %>%
    gather(variable, value, -label_sample) %>%
    group_by(variable) %>%
    mutate(
      value = normalize(value, method = "original")
    ) %>%
    ungroup() %>%
    rowwise() %>%
    mutate(value_class = rbinom(1, 1, value)) %>%
    ungroup() %>%
    mutate(value_class = if_else(value_class == 1, "Large", "Small")) %>%
    mutate(value_class = ordered(value_class, levels = c("Small", "Large")))

  cor_uniform <- data_uniform %>%
    pivot_wider(values_from = value, names_from = variable) %>%
    na.omit() %>%
    {
      cor(.$x, .$y)
    } %>%
    round(2)
  cor_uniform

  p1 <-
    data_uniform %>%
    pivot_wider(values_from = value, names_from = variable) %>%
    na.omit() %>%
    ggplot(aes(x, y)) +
    ggpointdensity::geom_pointdensity(show.legend = F) +
    # annotate("text", x = .9, y = .3, label = paste0('cor = ', cor_uniform), size = 3) +
    viridis::scale_color_viridis() +
    jtools::theme_nice() +
    ggtitle("Linear causal relationship") +
    theme(
      aspect.ratio = 1,
      legend.title = element_blank(),
      legend.position = c(.8, .3),
      text = element_text(size = 20),
    )
  p1

  p2 <-
    data_uniform %>%
    ungroup() %>%
    select(-value) %>%
    pivot_wider(values_from = value_class, names_from = variable) %>%
    group_by(x, y) %>%
    count() %>%
    ungroup() %>%
    mutate(n = n / sum(n)) %>%
    ggplot(aes(x, y, fill = n)) +
    geom_tile() +
    geom_text(aes(label = round(n, 2)), color = "white", size = 10) +
    viridis::scale_fill_viridis(option = "E") +
    jtools::theme_nice() +
    ggtitle("Linear causal relationship") +
    theme(
      text = element_text(size = 20),
      aspect.ratio = 1,
      legend.position = "none"
    )
  p2

  data_lognormal <-
    tibble(x = runif(n = Npoint, min = 0, max = 1)) %>%
    rowwise() %>%
    mutate(y = runif(1,
      min = x^2 - noise(x),
      max = x^2 + noise(x)
    )) %>%
    ungroup() %>%
    mutate(label_sample = row_number()) %>%
    gather(variable, value, -label_sample) %>%
    group_by(variable) %>%
    mutate(
      value = normalize(value, method = "original")
    ) %>%
    ungroup() %>%
    rowwise() %>%
    mutate(value_class = rbinom(1, 1, value)) %>%
    ungroup() %>%
    mutate(value_class = if_else(value_class == 1, "Large", "Small")) %>%
    mutate(value_class = ordered(value_class, levels = c("Small", "Large")))

  cor_lognormal <-
    data_lognormal %>%
    pivot_wider(values_from = value, names_from = variable) %>%
    na.omit() %>%
    {
      cor(.$x, .$y)
    } %>%
    round(2)
  cor_lognormal

  p3 <- data_lognormal %>%
    pivot_wider(values_from = value, names_from = variable) %>%
    na.omit() %>%
    ggplot(aes(x, y)) +
    ggpointdensity::geom_pointdensity(show.legend = F) +
    viridis::scale_color_viridis() +
    ggtitle("Non-linear causal relationship") +
    jtools::theme_nice() +
    theme(
      aspect.ratio = 1,
      legend.title = element_blank(),
      text = element_text(size = 20),
    )
  p3

  p4 <- data_lognormal %>%
    ungroup() %>%
    select(-value) %>%
    pivot_wider(values_from = value_class, names_from = variable) %>%
    group_by(x, y) %>%
    count() %>%
    ungroup() %>%
    mutate(n = n / sum(n)) %>%
    ggplot(aes(x, y, fill = n)) +
    geom_tile() +
    geom_text(aes(label = round(n, 2)), color = "white", size = 10) +
    ggtitle("Non-linear causal relationship") +
    viridis::scale_fill_viridis(option = "E") +
    jtools::theme_nice() +
    theme(
      text = element_text(size = 20),
      aspect.ratio = 1,
      legend.position = "none"
    )
  p4

  (p1 + p2) / (p3 + p4) + plot_annotation(tag_levels = "A")
}
