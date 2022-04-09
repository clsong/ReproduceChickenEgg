#' Reproduce Figure S16
#' @export
plot_figure_S16 <- function() {
  ggscatterstats(
    data = species_info,
    x = HGI,
    y = abundance,
    xlab = "Species generalization index",
    ylab = "Species abundance"
  )
}

#' Reproduce Table S1
#' @export
plot_table_S1 <- function() {
  X <- species_info %>%
    mutate(HGI = ifelse(is.na(HGI), 0, HGI)) %>%
    pull(HGI) %>%
    normalize(method = "original")

  Y <- species_info %>%
    pull(abundance) %>%
    normalize(method = "log")

  modelforw <- gam(Y ~ s(X))
  modelbackw <- gam(X ~ s(Y))

  tribble(
    ~model, ~p.value, ~r2,
    "Generalization", dhsic.test(modelforw$residuals, X)$p.value, summary(modelforw)$r.sq,
    "Abundance", dhsic.test(modelbackw$residuals, Y)$p.value, summary(modelbackw)$r.sq
  ) %>%
    mutate(
      p.value = round(p.value, 2),
      r2 = round(r2, 2)
    ) %>%
    gt(rowname_col = c("model")) %>%
    tab_header(
      title = md("Adjusted R<sup>2</sup> with additive additive model")
    ) %>%
    as_latex() %>%
    as.character() %>%
    cat()
}

#' Reproduce Table S2
#' @export
plot_table_S1 <- function() {
  entropy_empirical <- function(x) {
    dumb <- x[order(x)] %>%
      diff(lag = 1) %>%
      abs() %>%
      {
        .[. != 0]
      }
    digamma(length(x)) - digamma(1) + mean(log(dumb))
  }
  entropy_empirical(X)
  entropy_empirical(Y)

  tribble(
    ~model, ~entropy,
    "Generalization", entropy_empirical(X),
    "Abundance", entropy_empirical(Y)
  ) %>%
    mutate(entropy = round(entropy, 2)) %>%
    gt(rowname_col = c("model")) %>%
    tab_header(
      title = md("Empirical entropy of marginal distribution")
    ) %>%
    as_latex() %>%
    as.character() %>%
    cat()
}
