#' Reproduce Figure S13
#' @export
plot_figure_S13 <- function() {
  df_causal <- df %>%
    na.omit() %>%
    filter(generalization_measure == "Normalized degree") %>%
    group_by(level) %>%
    mutate(
      generalization = normalize(generalization, method = "original"),
      abundance = normalize(abundance + 1, method = "log")
    ) %>%
    ungroup()

  additive_noise_model <- df_causal %>%
    nest(data = -level) %>%
    mutate(data = map(data, ~ bootstraps(., times = 100))) %>%
    unnest(data) %>%
    mutate(
      generalization_model = map(splits, function(splits) {
        df_causal_sub <- analysis(splits)
        X <- df_causal_sub$generalization
        Y <- df_causal_sub$abundance
        web <- df_causal_sub$web %>% factor()
        gam(Y ~ s(X) + s(web, bs = "re"))
      }),
      abundance_model = map(splits, function(splits) {
        df_causal_sub <- analysis(splits)
        X <- df_causal_sub$generalization
        Y <- df_causal_sub$abundance
        web <- df_causal_sub$web %>% factor()
        gam(X ~ s(Y) + s(web, bs = "re"))
      })
    )

  additive_noise_model %>%
    select(-splits) %>%
    gather(type, model, -level, -id) %>%
    mutate(level = ifelse(level == "animal", "Hummingbird", "Plant")) %>%
    mutate(type = ifelse(type == "abundance_model", "Abundance as the cause", "Generalism as the cause")) %>%
    rowwise() %>%
    mutate(r2 = summary(model)$r.sq) %>%
    mutate(r2 = round(r2, 2)) %>%
    select(-model) %>%
    grouped_ggbetweenstats(
      x = type,
      y = r2,
      grouping.var = level,
      xlab = "Category",
      ylab = "Adjusted R2"
    ) +
    theme(
      axis.title.y = element_markdown(),
      legend.position = "none"
    )
}

#' Reproduce Figure S14
#' @export
plot_figure_S14 <- function() {
  df_causal <- df %>%
    na.omit() %>%
    filter(generalization_measure == "Normalized degree") %>%
    group_by(level) %>%
    mutate(
      generalization = normalize(generalization, method = "original"),
      abundance = normalize(abundance + 1, method = "log")
    ) %>%
    ungroup()

  additive_noise_model <- df_causal %>%
    nest(data = -level) %>%
    mutate(data = map(data, ~ bootstraps(., times = 100))) %>%
    unnest(data) %>%
    mutate(
      generalization_model = map(splits, function(splits) {
        df_causal_sub <- analysis(splits)
        X <- df_causal_sub$generalization
        Y <- df_causal_sub$abundance
        web <- df_causal_sub$web %>% factor()
        gam(Y ~ s(X) + s(web, bs = "re"))
      }),
      abundance_model = map(splits, function(splits) {
        df_causal_sub <- analysis(splits)
        X <- df_causal_sub$generalization
        Y <- df_causal_sub$abundance
        web <- df_causal_sub$web %>% factor()
        gam(X ~ s(Y) + s(web, bs = "re"))
      })
    )

  additive_noise_model_pvalue <-
    additive_noise_model %>%
    mutate(data = map(splits, analysis)) %>%
    select(-splits) %>%
    gather(type, model, -level, -data, -id) %>%
    mutate(residuals = map(model, ~ .$residuals)) %>%
    mutate(value = map2(data, type, function(data, type) {
      if (str_detect(type, "generalization")) {
        value <- data$generalization
      }
      if (str_detect(type, "abundance")) {
        value <- data$abundance
      }
      value
    })) %>%
    mutate(
      residual_value = map2(residuals, value, ~ tibble(residual = .x, value = .y))
    ) %>%
    select(-data, -residuals, -value) %>%
    unnest(residual_value) %>%
    mutate(level = ifelse(level == "animal", "Hummingbird", "Plant")) %>%
    mutate(type = ifelse(type == "abundance_model", "Abundance as the cause", "Generalism as the cause")) %>%
    nest(data = -c(level, type, id)) %>%
    mutate(p_value = map(data, ~ dhsic.test(.$residual, .$value)$p.value))

  additive_noise_model_pvalue %>%
    unnest(p_value) %>%
    grouped_ggbetweenstats(
      x = type,
      y = p_value,
      grouping.var = level,
      xlab = "Category",
      ylab = "P value"
    ) +
    theme(
      axis.title.y = element_markdown(),
      legend.position = "none"
    )
}


#' Reproduce Figure S15
#' @export
plot_figure_S15 <- function() {
  entropy_empirical <- function(x) {
    dumb <- x[order(x)] %>%
      diff(lag = 1) %>%
      abs() %>%
      {
        .[. != 0]
      }
    digamma(length(x)) - digamma(1) + mean(log(dumb))
  }

  df %>%
    na.omit() %>%
    group_by(level, generalization_measure) %>%
    mutate(
      generalization = normalize(generalization, method = "original"),
      abundance = normalize(abundance + 1, method = "log")
    ) %>%
    ungroup() %>%
    nest(data = c(-level, -generalization_measure)) %>%
    mutate(data = map(data, ~ bootstraps(., times = 100))) %>%
    unnest(data) %>%
    mutate(
      entropy = map(splits, function(splits) {
        df_causal_sub <- analysis(splits)
        tibble(
          abundance = entropy_empirical(df_causal_sub$abundance),
          generalization = entropy_empirical(df_causal_sub$generalization)
        )
      })
    ) %>%
    unnest(entropy) %>%
    filter(generalization_measure != "Generalisation index g") %>%
    select(-splits) %>%
    rename(
      `Genneralization measure` = generalization_measure,
      Abundance = abundance,
      Generalization = generalization
    ) %>%
    mutate(level = ifelse(level == "animal", "Hummingbird", "Plant")) %>%
    gather(key, value, -level, -`Genneralization measure`, -id) %>%
    mutate(
      key = ifelse(key == "Abundance", key, paste0(key, "\n(", `Genneralization measure`, ")"))
    ) %>%
    select(-`Genneralization measure`) %>%
    # filter(key == 'Abundance') %>%
    group_by(level, id, key) %>%
    sample_n(1) %>%
    ungroup() %>%
    mutate(key = ordered(key, levels = c(
      "Abundance",
      "Generalization\n(Normalized degree)",
      "Generalization\n(Degree)"
    ))) %>%
    grouped_ggbetweenstats(
      x = key,
      y = value,
      grouping.var = level,
      xlab = "",
      ylab = "Entropy"
    )
}
