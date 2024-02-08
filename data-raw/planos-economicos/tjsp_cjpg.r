tipo <- "assunto"
tbl <- stringr::str_c(
  "https://esaj.tjsp.jus.br/cjpg/", tipo, "TreeSelect.do?campoId=",
  tipo
) |>
  httr::GET(
    httr::config(ssl_verifypeer = FALSE),
    lex:::esaj_ua()
  ) |>
  httr::content("text") |>
  xml2::read_html() |>
  xml2::xml_find_all("//div[@class='treeView']") |>
  xml2::as_list() |>
  dplyr::first() |>
  dplyr::nth(2) |>
  purrr::keep(~ is.list(.x)) |>
  lex:::tree_to_tibble() |>
  dplyr::as_tibble() |>
  dplyr::mutate(name0 = ifelse(is.na(name0),
    name5, name0
  ), id0 = ifelse(is.na(id0), id5, id0)) |>
  dplyr::select(
    dplyr::ends_with("0"), dplyr::ends_with("1"),
    dplyr::ends_with("2"), dplyr::ends_with("3"), dplyr::ends_with("4"),
    dplyr::ends_with("5")
  )

id <- tbl |>
  dplyr::filter(stringr::str_detect(name5, "Planos Econ")) |>
  dplyr::pull(id5)

decisoes <- lex::tjsp_cjpg_download(
  busca = "", dir = "data-raw/planos-economicos/tjsp_cjpg",
  assunto = id, data_ini = "2023-01-01", data_fim = "2023-12-31"
)
