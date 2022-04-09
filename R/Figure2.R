#' Reproduce Figure 2 (note that the reproduced figure lacks annotations
#' in the main text; these annotations were added via the software Illustrator)
#'
#' @export
plot_figure_2 <- function() {
  nsample <- 5000

  set.seed(1010)
  df_x <- tibble(x = runif(nsample, 0, 1)) %>%
    mutate(x = normalize(x, method = "original"))

  set.seed(1010)
  df <-
    seq(from = 0.5, to = 1, by = .1) %>%
    map(function(coeff) {
      df_x %>%
        mutate(
          y = x^coeff + rnorm(nsample, 0, .5)
        )
    }) %>%
    bind_rows()

  set.seed(1010)
  df <- df %>%
    sample_n(500)

  # regression
  df_regression <- df %>%
    mutate(cause_x = gam(y ~ s(x), data = .)$residuals) %>%
    mutate(cause_y = gam(x ~ s(y), data = .)$residuals)

  p_original <- df_regression %>%
    ggplot(aes(x, y)) +
    geom_pointdensity() +
    scale_color_viridis(option = "magma") +
    labs(
      x = "*X*",
      y = "*Y*"
    ) +
    theme(
      aspect.ratio = 1,
      legend.title = element_blank(),
      legend.position = "none",
      text = element_text(size = 20),
      axis.title.y = element_markdown(),
      axis.title.x = element_markdown()
    )

  df_regression_summary <- bind_rows(
    df_regression %>%
      select(ends_with("x")) %>%
      rename(value = x, cause = cause_x) %>%
      mutate(variable = "x"),
    df_regression %>%
      select(ends_with("y")) %>%
      rename(value = y, cause = cause_y) %>%
      mutate(variable = "y"),
  ) %>%
    nest(data = -variable)

  # Time consuming
  # set.seed(1010)
  # p_value <- df_regression_summary %>%
  #   mutate(p = map_dbl(data, ~dhsic.test(.$value, .$cause)$p.value)) %>%
  #   mutate(p = paste0("p = ", round(p, 4))) %>%
  #   mutate(variable = ifelse(variable == 'x', '*X*', '*Y*'))

  p_regression <- df_regression_summary %>%
    unnest(data) %>%
    group_by(variable) %>%
    filter(value < quantile(value, 0.95) & value > quantile(value, 0.05)) %>%
    ungroup() %>%
    mutate(variable = ifelse(variable == "x", "*X*", "*Y*")) %>%
    ggplot(aes(value, cause)) +
    geom_pointdensity() +
    scale_color_viridis(option = "magma") +
    geom_label(
      data = p_value,
      aes(label = p),
      x = Inf, y = -Inf, hjust = 1, vjust = 0, size = 6,
      inherit.aes = FALSE
    ) +
    facet_wrap(. ~ variable, scales = "free", strip.position = "bottom") +
    labs(
      x = "",
      y = "Noise"
    ) +
    theme(
      legend.title = element_blank(),
      legend.position = "none",
      strip.placement = "outside",
      text = element_text(size = 20),
      strip.text = element_markdown()
    )

  # information theory
  entropy_empirical <- function(x) {
    dumb <- x[order(x)] %>%
      diff(lag = 1) %>%
      abs() %>%
      {
        .[. != 0]
      }
    digamma(length(x)) - digamma(1) + mean(log(dumb))
  }

  p_entropy <- df %>%
    mutate(
      x = normalize(x, method = "original"),
      y = normalize(y, method = "original")
    ) %>%
    ggplot(aes(x, y)) +
    geom_pointdensity() +
    scale_color_viridis(option = "magma") +
    labs(
      x = "*X* (scaled)",
      y = "*Y* (scaled)"
    ) +
    theme(
      aspect.ratio = 1,
      legend.title = element_blank(),
      legend.position = "none",
      text = element_text(size = 20),
      axis.title.y = element_markdown(),
      axis.title.x = element_markdown()
    )


  p_entropy <- p_entropy %>%
    ggMarginal(
      type = "densigram",
      xparams = list(fill = "dodgerblue"),
      yparams = list(fill = "#009051")
    )

  # formal logic
  df_formal <- df %>%
    mutate(
      x = normalize(x, method = "normal"),
      y = normalize(y, method = "normal")
    )

  p_formal <- df_formal %>%
    ggplot(aes(x, y)) +
    geom_pointdensity() +
    scale_color_viridis(option = "magma") +
    labs(
      x = "*X* (normalized)",
      y = "*Y* (normalized)"
    ) +
    geom_vline(xintercept = .5, color = "white", size = .5) +
    geom_hline(yintercept = .5, color = "white", size = .5) +
    theme(
      aspect.ratio = 1,
      legend.title = element_blank(),
      legend.position = "none",
      text = element_text(size = 20),
      axis.title.y = ggtext::element_markdown(),
      axis.title.x = ggtext::element_markdown()
    )

  # combine the figures
  p_original + p_formal + p_regression + p_entropy +
    plot_annotation(tag_levels = "A")
}
