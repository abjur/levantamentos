# sankey completo ---------------------------------------------------------

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

