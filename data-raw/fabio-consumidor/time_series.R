# dados -------------------------------------------------------------------

set.seed(18)
da_ts <- tibble::tibble(
  id = 1:3000,
  a_2020 = sample(c("Procon / Consumidor.gov", "SAC", "Judiciário", "Ouvidoria"), 3000, replace=TRUE),
  a_2021 =  sample(c("Procon / Consumidor.gov", "SAC", "Judiciário", "Ouvidoria"), 3000, replace=TRUE),
  a_2022 =  sample(c("Procon / Consumidor.gov", "SAC", "Judiciário", "Ouvidoria"), 3000, replace=TRUE),
) |>
  tidyr::pivot_longer(cols = dplyr::contains("a_"), names_to = "ano") |>
  dplyr::mutate(
    ano = stringr::str_extract(ano, "[0-9]+")
  ) |>
  dplyr::count(ano, value)

cores_abj <-  viridis::viridis(4, 1, .2, .8)


# time series completo ----------------------------------------------------
da_ts |>
  ggplot2::ggplot() +
  ggplot2::aes(x = ano, y = n, color = value, group = value) +
  ggplot2::geom_point(size = 4) +
  ggplot2::geom_line(size = 1) +
  ggplot2::scale_color_manual(values = cores_abj, name = "Via") +
  ggplot2::labs(
    y = "Quantidade de reclamações consumeristas",
    x = "Ano"
  ) +
  ggplot2::theme_bw(16)


# time series parcial ----------------------------------------------------
da_ts |>
  dplyr::filter(value %in% c("Judiciário", "Procon / Consumidor.gov")) |>
  ggplot2::ggplot() +
  ggplot2::aes(x = ano, y = n, color = value, group = value) +
  ggplot2::geom_point(size = 4) +
  ggplot2::geom_line(size = 1) +
  ggplot2::scale_color_manual(values = cores_abj, name = "Via") +
  ggplot2::labs(
    y = "Quantidade de reclamações consumeristas",
    x = "Ano"
  ) +
  ggplot2::theme_bw(16)

