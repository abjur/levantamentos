sp <- readr::read_rds("data-raw/fernando_viana_empresariais/sp.rds")

processo <- sp |>
  dplyr::filter(stringr::str_detect(processo, abjutils::pattern_cnj())) |>
  dplyr::pull(processo) |>
  unique()

download_tjsp <- function(id, ...) {
  lex::tjsp_cpopg_download(id, ...)
  Sys.sleep(1)
}

download <- purrr::map(
  processo, download_tjsp,
  dir = "data-raw/fernando_viana_empresariais/tjsp_cpopg",
  .progress = TRUE
)

download <- fs::dir_ls("data-raw/fernando_viana_empresariais/tjsp_cpopg")


tbl_cpopg <- purrr::map_vec(download, lex::tjsp_cpopg_parse)

tbl_cpopg |>
  dplyr::filter(is.na(erro)) |>
  dplyr::distinct(id_processo, .keep_all = TRUE) |>
  readr::write_rds("data-raw/fernando_viana_empresariais/tjsp_cpopg.rds")

termos_busca <- c(
  "(LUCROS CESSANTES)",
  "(MARCA)", "(PATENTE)",
  "(PROPRIEDADE INTELECTUAL)", "(PROPRIEDADE INDUSTRIAL)",
  "(CONCORRÊNCIA DESLEAL)",
  "(APURAÇÃO DE HAVERES)",
  "(DISSOLUÇÃO)",
  "(EXCLUSÃO)",
  "(ANULAÇÃO DE SENTENÇA ARBITRAL)",
  "(CONFLITO DE INTERESSES)",
  "(ROUBO DE INFORMAÇÃO CONFIDENCIAL)",
  "(PRODUÇÃO ANTECIPADA DE PROVAS)"
) |>
  paste(collapse = "|") |>
  stringr::regex(ignore_case = TRUE)

sp_final <- tbl_cpopg |>
  dplyr::distinct(id_processo, .keep_all = TRUE) |>
  dplyr::filter(
    is.na(erro),
    stringr::str_detect(assunto, termos_busca)
  ) |>
  dplyr::mutate(termo = stringr::str_extract(assunto, termos_busca)) |>
  dplyr::select(id_processo:digital, outros_numeros:local_fisico)

writexl::write_xlsx(sp_final, "data-raw/fernando_viana_empresariais/base_termos_sp.xlsx")


