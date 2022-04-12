#' Reproduce Figure S13 (note that the reproduced figure lacks annotations
#' in the main text; these annotations were added via the software Illustrator)
#'
#' @export
plot_figure_S13 <- function() {
  set.seed(1010)
  df_pca %>%
    rename(
      "Temperature" = Temp,
      "Connectance" = connectance,
      "Species\nrichness" = web_size,
      "Evidence" = diff
    ) %>%
    nest(data = -level) %>%
    mutate(plot = map2(data, level, function(data, level) {
      color_choice <- ifelse(level == "Hummingbird", "#FFBD20", "dodgerblue")
      data <- prcomp(data, scale = TRUE)
      fviz_pca_biplot(
        data,
        repel = TRUE,
        col.var = color_choice, # Variables color
        col.ind = "#696969", # Individuals color,
        geom = c("point")
      ) +
        labs(title = level) +
        theme(
          aspect.ratio = 1
        )
    })) %>%
    pull(plot) %>%
    patchwork::wrap_plots()
}
