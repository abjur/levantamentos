
orgaos <- lex::tjsp_cjsg_table("vara")
orgaos_id <- orgaos |>
  dplyr::filter(stringr::str_detect(court, "Câmara Reservada de Direito Empresarial")) |>
  dplyr::pull(id)

download_camara <- function(camara) {
  cjsg <- lex::tjsp_cjsg_download(
    busca = '"recuperação judicial" E "plano" E "homologação"',
    dir = "data-raw/luiza-giannotti/cjsg",
    orgao_julgador = camara,
    julgamento_ini = "01/01/2019",
    julgamento_fim = "31/12/2022"
  )
  cjsg_parsed <- purrr::map_dfr(
    cjsg, lex::tjsp_cjsg_parse,
    .progress = TRUE
  ) |>
  dplyr::mutate(camara = camara)
}

cjsg_parsed <- purrr::map_dfr(
  orgaos_id, download_camara,
  .progress = TRUE
)
cjsg_parsed <- cjsg_parsed |>
  dplyr::left_join(orgaos, dplyr::join_by(camara == id)) |>
  dplyr::mutate(camara = court) |>
  dplyr::select(-court, -branch)

writexl::write_xlsx(cjsg_parsed, "data-raw/luiza-giannotti/levantamento.xlsx")
