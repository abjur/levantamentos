# Dados do Querido Diário

cidades <- httr::GET(
  "https://queridodiario.ok.org.br/api/cities",
  httr::accept_json()
)

cidades <- cidades |>
  httr::text_content() |>
  jsonlite::fromJSON() |>
  purrr::pluck(1)

diarios_disponiveis <- dplyr::filter(cidades, level == 3)


# Municípios solicitados

xlsx <- readxl::read_xlsx("data-raw/ana_spinelli/lista_municipios.xlsx")
xlsx <- xlsx |>
  dplyr::inner_join(
    dplyr::distinct(abjData::muni, uf_nm, uf_sigla, muni_nm, muni_id),
    c("Estado" = "uf_nm", "Município" = "muni_nm")
  )
dplyr::glimpse(diarios_disponiveis)


# Municípios solicitados com diários disponíveis

xlsx |>
  dplyr::left_join(
    diarios_disponiveis, c("muni_id" = "territory_id")
  ) |>
  dplyr::filter(!is.na(level)) |>
  dplyr::pull(Município) |>
  clipr::write_clip()

xlsx |>
  dplyr::left_join(
    diarios_disponiveis, c("muni_id" = "territory_id")
  ) |>
  dplyr::filter(!is.na(level)) |>
  dplyr::glimpse()


# Baixar dados

get_gazettes <- function(id) {
  url <- "https://queridodiario.ok.org.br/api/gazettes"
  params <- list(
    territory_ids = id,
    published_since = "2010-01-01",
    published_until = "2022-12-31",
    querystring = "(\"encomenda tecnológica\") | (\"encomenda tecnologica\")",
    excerpt_size = 500,
    number_of_excerpts = 1,
    pre_tags = "",
    post_tags = "",
    size = 50,
    sort_by = "relevance"
  )
  diarios <- httr::GET(
    url, query = params, httr::accept_json()
  )
  if (httr::content(diarios)$total_gazettes > 0) {
    httr::content(diarios) |>
      purrr::pluck("gazettes") |>
      purrr::map_vec(tibble::as_tibble)
  }
}

diarios_termo <- purrr::map(diarios_disponiveis$territory_id, get_gazettes) |>
  dplyr::bind_rows()

readr::write_rds(diarios_termo, "data-raw/ana_spinelli/diarios_termo.rds")
