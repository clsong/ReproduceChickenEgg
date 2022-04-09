#' Reproduce Figure S4
#' @export
plot_figure_S4 <- function() {
  plots_original <- df %>%
    group_split(generalization_measure, level) %>%
    map(function(df) {
      p <- df %>%
        ggplot(aes(generalization, abundance)) +
        ggpointdensity::geom_pointdensity(show.legend = F) +
        viridis::scale_color_viridis() +
        # geom_smooth(method="lm")+
        jtools::theme_nice() +
        labs(
          x = "Generalization",
          y = "Abundance",
          title = paste0(unique(df$level), ":", unique(df$generalization_measure))
        ) +
        theme(
          aspect.ratio = 1,
          text = element_text(size = 20),
          plot.title = element_text(size = 20)
        )
      ggMarginal(p)
    })

  wrap_plots(plots_original[c(1, 3, 2, 4)]) + plot_annotation(tag_levels = "A")
}

#' Reproduce Figure S7
#' @export
plot_figure_S7 <- function() {
  plots_normal <- df %>%
    group_split(generalization_measure, level) %>%
    map(function(df) {
      p <- df %>%
        filter(!is.infinite(generalization)) %>%
        mutate(abundance = normalize(abundance, method = "normal")) %>%
        mutate(generalization = normalize(generalization, method = "normal")) %>%
        ggplot(aes(generalization, abundance)) +
        ggpointdensity::geom_pointdensity(show.legend = F) +
        viridis::scale_color_viridis() +
        annotation_logticks() +
        labs(
          x = "Generalization",
          y = "Abundance",
          title = paste0(unique(df$level), ":", unique(df$generalization_measure))
        ) +
        jtools::theme_nice() +
        theme(
          aspect.ratio = 1,
          text = element_text(size = 20),
          plot.title = element_text(size = 20)
        )
      ggMarginal(p)
    })

  wrap_plots(plots_normal[c(1, 3, 2, 4)]) + plot_annotation(tag_levels = "A")
}

#' Reproduce Figure S6
#' @export
plot_figure_S6 <- function() {
  df %>%
    drop_na() %>%
    filter(!is.infinite(generalization)) %>%
    group_by(level, generalization_measure) %>%
    mutate(generalization = log(generalization)) %>%
    ungroup() %>%
    filter(!is.infinite(generalization)) %>%
    nest(data = c(abundance, generalization, web, species)) %>%
    mutate(
      p = map(data, ~ shapiro.test(.$generalization)$p.value)
    ) %>%
    unnest(p) %>%
    unnest(data) %>%
    ggqqplot(x = "generalization") %>%
    facet(facet.by = c("level", "generalization_measure"))
}

#' Reproduce Figure S5
#' @export
plot_figure_S5 <- function() {
  df %>%
    drop_na() %>%
    select(level, abundance, species, web) %>%
    distinct() %>%
    group_by(level) %>%
    mutate(abundance = log(abundance)) %>%
    ungroup() %>%
    filter(!is.infinite(abundance)) %>%
    ggqqplot(x = "abundance") %>%
    facet(facet.by = c("level"))
}
