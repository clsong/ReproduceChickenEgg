#' Process to get the key tibbles used in the analysis. These steps
#' are time-consuming, thus these tibbles are already saved as .rda
#' files.


#' the key tibble with the network information
#' @export
get_df <- function() {
  read_network <- function(web_name) {
    web_name %>%
      {
        here("data/networks", .)
      } %>%
      read.table(.) %>%
      as.matrix(.) %>%
      reshape2::melt(.) %>%
      mutate(web = str_sub(web_name, 1, -5))
  }

  df_messy <-
    list.files(path = here("data/networks")) %>%
    map_dfr(~ read_network(.)) %>%
    as_tibble() %>%
    rename(
      animal = Var2,
      plant = Var1
    ) %>%
    group_by(animal, web) %>%
    mutate(n_animals = n()) %>%
    group_by(plant, web) %>%
    mutate(n_plants = n()) %>%
    ungroup() %>%
    nest(data = -web) %>%
    mutate(g_animal = map2(data, web, function(data, web_name) {
      plant_abundance <- data %>%
        left_join(
          abundance %>% filter(level == "plant") %>%
            select(-measure) %>%
            filter(web == web_name),
          by = c("plant" = "species")
        ) %>%
        select(plant, abundance) %>%
        unique() %>%
        arrange(plant)

      data_web <- here("data/networks", paste0(web_name, ".txt")) %>%
        read.table() %>%
        as.matrix()

      specialization <- dfun(t(data_web))
      generalization <- 1 - specialization$d / specialization$dmax
      generalization %>%
        enframe(name = "animal", value = "g")
    })) %>%
    mutate(g_plant = map2(data, web, function(data, web_name) {
      animal_abundance <- data %>%
        left_join(
          abundance %>% filter(level == "animal") %>% select(-measure) %>% filter(web == web_name),
          by = c("animal" = "species")
        ) %>%
        select(animal, abundance) %>%
        unique() %>%
        arrange(animal)

      data_web <- here("data/networks", paste0(web_name, ".txt")) %>%
        read.table() %>%
        as.matrix()

      specialization <- dfun(data_web)
      generalization <- 1 - specialization$d / specialization$dmax
      generalization %>%
        enframe(name = "plant", value = "g")
      # right_join(data, by = "plant")
    })) %>%
    mutate(data = map2(data, g_animal, ~ left_join(.x, .y, by = "animal"))) %>%
    mutate(data = map2(data, g_plant, ~ left_join(.x, .y, by = "plant"))) %>%
    select(-g_animal, -g_plant) %>%
    unnest(data) %>%
    rename(
      g_animal = g.x,
      g_plants = g.y
    ) %>%
    filter(value > 0) %>%
    select(-value) %>%
    group_by(animal, web) %>%
    mutate(degree_animal = n()) %>%
    group_by(plant, web) %>%
    mutate(degree_plant = n()) %>%
    ungroup()

  df <-
    bind_rows(
      networks %>%
        select(web, contains("plant")) %>%
        rename(
          species = plant,
          n = n_plants,
          g = g_plants,
          degree = degree_plant
        ) %>%
        distinct() %>%
        mutate(level = "plant"),
      networks %>%
        select(web, contains("animal")) %>%
        rename(
          species = animal,
          n = n_animals,
          g = g_animal,
          degree = degree_animal
        ) %>%
        distinct() %>%
        mutate(level = "animal"),
    ) %>%
    mutate(level = as.character(level)) %>%
    left_join(abundance, by = c("species", "web")) %>%
    rename(level = level.x) %>%
    select(species, web, abundance, n, g, degree, level) %>%
    mutate(degree_normalized = degree / n) %>%
    select(-n) %>%
    gather(key = "generalization_measure", value = "generalization", -species, -web, -abundance, -level) %>%
    mutate(
      generalization_measure = case_when(
        generalization_measure == "degree" ~ "Degree",
        generalization_measure == "degree_normalized" ~ "Normalized degree",
        generalization_measure == "g" ~ "Generalisation index g"
      )
    ) %>%
    mutate(
      generalization_measure = ordered(generalization_measure,
        levels = c("Degree", "Normalized degree", "Generalisation index g")
      )
    )
  df
}

#' Normalize generalism and abundance
#' @export
get_df_normalized <- function(df) {
  nsample <- 100
  df_normalized <-
    df %>%
    expand_grid(
      method_generalization = c("original", "normal"),
      method_abundance = c("original", "normal")
    ) %>%
    nest(data = c(species, generalization, abundance, web)) %>%
    mutate(
      data = pmap(
        list(data, method_generalization, method_abundance),
        function(data, method_generalization, method_abundance) {
          data %>%
            filter(!is.infinite(generalization)) %>%
            mutate(
              generalization = normalize(generalization, method_generalization),
              abundance = normalize(abundance, method_abundance)
            )
        }
      )
    ) %>%
    unnest(data) %>%
    group_by(generalization_measure, method_generalization, method_abundance, web, level) %>%
    mutate(
      generalization = normalize(generalization, method = "original"),
      abundance = normalize(abundance, method = "original")
    ) %>%
    ungroup()

  df_normalized
}

#' Binarize generalization and abundance
#' @export
get_df_binarized <- function(df_normalized) {
  df_binarized <-
    df_normalized %>%
    na.omit() %>%
    gather(key, value = "probability", generalization:abundance) %>%
    mutate(
      binary = map(probability, ~ rbinom(nsample, 1, .))
    ) %>%
    unnest(binary) %>%
    select(-probability) %>%
    pivot_wider(names_from = key, values_from = binary) %>%
    unnest(generalization) %>%
    unnest(abundance) %>%
    mutate(class = case_when(
      abundance == 0 & generalization == 0 ~ "RS",
      abundance == 0 & generalization == 1 ~ "RG",
      abundance == 1 & generalization == 0 ~ "AS",
      abundance == 1 & generalization == 1 ~ "AG"
    )) %>%
    mutate(class = ordered(class, levels = c("RS", "RG", "AS", "AG"))) %>%
    group_by(web, generalization_measure, method_generalization, method_abundance, class, level) %>%
    count() %>%
    ungroup()

  df_binarized
}

#' get key statistics
#' @export
get_df_statistics <- function(df_binarized) {
  df_statistics <- df_binarized %>%
    group_by(web, generalization_measure, method_generalization, method_abundance, level) %>%
    mutate(n = n / sum(n)) %>%
    ungroup() %>%
    filter(method_generalization == "normal" & method_abundance == "normal") %>%
    select(-method_generalization, -method_abundance)

  df_statistics
}

#' get NODF_c in the networks
#' @export
get_df_NODF <- function() {
  df_NODF <- df_statistics %>%
    pivot_wider(names_from = class, values_from = n) %>%
    filter(generalization_measure == "Normalized degree") %>%
    mutate(hypothesis = if_else(RG > AS, "abundant", "generalist")) %>%
    arrange(hypothesis) %>%
    left_join(
      list.files(path = here("data/networks")) %>%
        enframe(value = "web_name") %>%
        mutate(NODFc = map(web_name, function(x) {
          matrix <- x %>%
            {
              here("data/networks", .)
            } %>%
            read.table() %>%
            as.matrix()

          matrix <- matrix[which(rowSums(matrix) > 0), which(colSums(matrix) > 0)]

          try(maxnodf::NODFc(matrix, quality = 2))
        })) %>%
        rowwise() %>%
        filter(is.numeric(NODFc)) %>%
        mutate(web_name = str_sub(web_name, 1, -5)) %>%
        unnest(NODFc) %>%
        rename(web = web_name) %>%
        select(-name),
      by = "web"
    )
}

#' get environmental information
#' @export
get_environment_variability <- function() {
  Tmax <- paste0(paste0("http://thredds.northwestknowledge.net:8080/thredds/dodsC/agg_terraclimate_", "tmax"), "_1958_CurrentYear_GLOBE.nc")
  Tmin <- paste0(paste0("http://thredds.northwestknowledge.net:8080/thredds/dodsC/agg_terraclimate_", "tmin"), "_1958_CurrentYear_GLOBE.nc")

  environment_all <-
    df_NODF %>%
    left_join(
      location,
      by = "web"
    ) %>%
    expand_grid(
      var = c("tmax", "tmin")
    ) %>%
    drop_na() %>%
    mutate(
      temp = pmap(list(Longitude, Latitude, var), function(Longitude, Latitude, var) {
        x <- c(Longitude, Latitude)
        if (var == "tmax") {
          nc <- nc_open(Tmax)
        }
        if (var == "tmin") {
          nc <- nc_open(Tmin)
        }

        lon <- ncvar_get(nc, "lon")
        lat <- ncvar_get(nc, "lat")
        flat <- match(abs(lat - x[2]) < 1 / 48, 1)
        latindex <- which(flat %in% 1)
        flon <- match(abs(lon - x[1]) < 1 / 48, 1)
        lonindex <- which(flon %in% 1)
        start <- c(lonindex, latindex, 1)
        count <- c(1, 1, -1)

        data <- try(as.numeric(ncvar_get(nc, varid = var, start = start, count)))
      })
    )

  environment_variability <-
    environment_all %>%
    rowwise() %>%
    mutate(length = length(temp)) %>%
    ungroup() %>%
    filter(length == 756) %>%
    mutate(temp = map(temp, function(temp) {
      enframe(temp) %>%
        mutate(month = name %% 12) %>%
        group_by(month) %>%
        mutate(year = 1958:2020) %>%
        ungroup() %>%
        mutate(month = if_else(month == 0, 12, month))
    })) %>%
    unnest(temp) %>%
    select(-name) %>%
    pivot_wider(names_from = var, values_from = value) %>%
    group_by(year, web) %>%
    summarise(
      Temp_var = mean((tmax - tmin)^2 / 12),
      Temp_mean = mean((tmax + tmin) / 2),
      .groups = "drop"
    )
}


#' To do PCA analysis in Appendix E
#' @export
get_df_pca <- function(environment_variability) {
  df_pca <-
    environment_variability %>%
    left_join(select(df_NODF, web, NODFc, AS, RG, level)) %>%
    mutate(diff = AS - RG) %>%
    mutate(level = ifelse(level == "animal", "Hummingbird", "Plant")) %>%
    group_by(web, NODFc, level, diff) %>%
    summarise(
      sd = sd(Temp_mean),
      Temp = mean(Temp_mean)
    ) %>%
    ungroup() %>%
    drop_na() %>%
    select(web, NODFc, Temp, diff, level) %>%
    mutate(web_structure = map(web, function(web) {
      web %>%
        paste0(".txt") %>%
        {
          here("data/networks", .)
        } %>%
        read.table() %>%
        as.matrix()
    })) %>%
    mutate(
      web_size = map(web_structure, ~ sqrt(ncol(.) * nrow(.))),
      connectance = map(web_structure, function(x) {
        x[x > 0] <- 1
        sum(x) / (ncol(x) * nrow(x))
      })
    ) %>%
    select(-web_structure) %>%
    unnest(web_size) %>%
    unnest(connectance) %>%
    select(-web) %>%
    drop_na()

  df_pca
}
