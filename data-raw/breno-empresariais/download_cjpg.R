prest_contas <- lex::tjsp_cjpg_table(tipo = "assunto") |>
  dplyr::filter(dplyr::if_any(
    dplyr::contains("name"),
    ~stringr::str_detect(.x, stringr::regex("prestação de contas", TRUE))
  )) |>
  dplyr::pull(id5)

varas <- lex::tjsp_cjpg_table(tipo = "vara") |>
  dplyr::filter(stringr::str_detect(
    court, stringr::regex("vara empresarial", TRUE)
  )) |>
  dplyr::pull(id) |>
  unique()


download_cjpg <- function(ano) {
  cjpg <- lex::tjsp_cjpg_download(
    busca = '"perícia contábil"',
    dir = "data-raw/breno-empresariais/cjpg",
    comarca = varas,
    data_ini = paste0(ano,"-01-01"), data_fim = paste0(ano, "-12-31")
  )
  purrr::map_dfr(cjpg, lex::tjsp_cjpg_parse)
}

poss <- purrr::possibly(download_cjpg, NULL)
base <- purrr::map_dfr(2017:2020, poss)

base |>
  dplyr::distinct() |>
  writexl::write_xlsx("data-raw/breno-empresariais/pericia_contabil.xlsx")
