# base de dados ---------------------------------------------------------

set.seed(1)
da <- tibble::tibble(
  id = 1:1000,
  etapa1 = sample(c("Procon / Consumidor.gov", "SAC", "Judiciário"), 1000, replace=TRUE, prob=c(0.3, 0.65, 0.05)),
  etapa2 = NA_character_,
  etapa3 = NA_character_,
  etapa4 = NA_character_
)

for(i in da$id) {
  # etapa 2
  etapa <- da |>
    dplyr::filter(id == i) |>
    dplyr::pull(etapa1)

  if(etapa == "SAC") etapa2 <- sample(c("Procon / Consumidor.gov", "Ouvidoria", "Judiciário"), 1)
  if(etapa == "Judiciário") etapa2 <- NA_character_
  if(etapa == "Procon / Consumidor.gov") etapa2 <- sample(c("Judiciário", NA_character_), 1)

  da$etapa2[da$id == i] = etapa2

  # etapa 3
  etapa <- da |>
    dplyr::filter(id == i) |>
    dplyr::pull(etapa2)

  if(is.na(etapa)) {
    etapa3 <- NA_character_
  } else {
    if(etapa == "Ouvidoria") etapa3 <- sample(c("Procon / Consumidor.gov", NA_character_), 1)
    if(etapa == "Judiciário") etapa3 <- NA_character_
    if(etapa == "Procon / Consumidor.gov") etapa3 <- sample(c("Judiciário", NA_character_), 1)
  }

  da$etapa3[da$id == i] = etapa3

  # etapa 4
  etapa <- da |>
    dplyr::filter(id == i) |>
    dplyr::pull(etapa3)

  if(is.na(etapa)) {
    etapa4 <- NA_character_
  } else {
    if(etapa == "Judiciário") etapa4 <- NA_character_
    if(etapa == "Procon / Consumidor.gov") etapa4 <- sample(c("Judiciário", NA_character_), 1)
  }

  da$etapa4[da$id == i] = etapa4
}

readr::write_rds(da, "data-raw/fabio-consumidor/da_sankey.rds")

# sankey completo ---------------------------------------------------------
da <- readr::read_rds("data-raw/fabio-consumidor/da_sankey.rds")
cores_abj <-  viridis::viridis(4, 1, .2, .8)

da_lodes <- da |>
  tidyr::pivot_longer(cols = dplyr::contains("etapa"), names_to = "etapa") |>
  dplyr::filter(!is.na(value))

ggplot2::ggplot(da_lodes,
                ggplot2::aes(
                  x = etapa,
                  stratum = value,
                  alluvium = id,
                  fill = value,
                  label = value
                )) +
  ggalluvial::geom_flow() +
  ggalluvial::geom_stratum() +
  ggplot2::scale_fill_manual(values = cores_abj, name = "Via") +
  ggplot2::theme_minimal(16) +
  ggplot2::theme(
    axis.title.x = ggplot2::element_blank(),
    axis.text.x = ggplot2::element_blank(),
    legend.position = c(.8, .5)
  )

# sankey parcial ---------------------------------------------------------
da <- readr::read_rds("data-raw/fabio-consumidor/da_sankey.rds")
cores_abj <-  viridis::viridis(4, 1, .2, .8)

da_lodes <- da |>
  tidyr::pivot_longer(cols = dplyr::contains("etapa"), names_to = "etapa") |>
  dplyr::filter(
    !is.na(value),
    etapa %in% c("etapa3", "etapa4")
  )

ggplot2::ggplot(da_lodes,
                ggplot2::aes(
                  x = etapa,
                  stratum = value,
                  alluvium = id,
                  fill = value,
                  label = value
                )) +
  ggalluvial::geom_flow() +
  ggalluvial::geom_stratum() +
  ggplot2::scale_fill_manual(values = c(cores_abj[1], cores_abj[2], cores_abj[3]), name = "Via") +
  ggplot2::theme_minimal(16) +
  ggplot2::theme(
    axis.title.x = ggplot2::element_blank(),
    axis.text.x = ggplot2::element_blank(),
    legend.position = c(.8, .5)
  )
