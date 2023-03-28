
# cjpg --------------------------------------------------------------------

assuntos <- "4942,9558,3661,5980"

classes <- "8562,8575,8576"

lex::tjsp_api_login()
lex::tjsp_cjpg_download(
  busca = "",
  dir = "data-raw/fernanda-portella/cjpg",
  assunto = assuntos,
  classe = classes)
)

da_cjpg <- fs::dir_ls("data-raw/fernanda-portella/cjpg/") |>
  purrr::map_dfr(lex::tjsp_cjpg_parse) |>
  dplyr::select(n_processo, data_de_disponibilizacao)

readr::write_rds(da_cjpg, "data-raw/fernanda-portella/da_cjpg.rds")

# cpopg -------------------------------------------------------------------
processos <- da_cjpg |>
  # dplyr::mutate(
  #   ano = lubridate::dmy(data_de_disponibilizacao),
  #   ano = lubridate::year(ano)
  # ) |>
  # dplyr::filter(ano >= 2018) |>
  dplyr::pull(n_processo)

for(processo in processos) {
  Sys.sleep(1)
  lex::tjsp_cpopg_download(
    id = processo,
    dir = "data-raw/fernanda-portella/cpopg/",
    login = "27022971889",
    senha = "pesquisa")
}

da_cpopg <- fs::dir_ls("data-raw/fernanda-portella/cpopg/") |>
  purrr::map_dfr(lex::tjsp_cpopg_parse)

readr::write_rds(da_cpopg, "data-raw/fernanda-portella/da_cpopg.rds")


# filter ------------------------------------------------------------------

processos_sa <- da_cpopg |>
  tidyr::unnest(partes) |>
  dplyr::mutate(
    sa = stringr::str_detect(nome, stringr::regex("( SA| Sa| sa)$")),
    id_processo = abjutils::clean_id(id_processo)
  ) |>
  dplyr::filter(sa) |>
  dplyr::pull(id_processo) |>
  unique()

da_fernanda <- da_cpopg |>
  dplyr::mutate(
    id_processo = abjutils::clean_id(id_processo)
  ) |>
  dplyr::filter(id_processo %in% processos_sa) |>
  dplyr::transmute(
    id_processo,
    status,
    assunto,
    classe,
    foro,
    juiz,
    vara,
    distribuicao = stringr::str_extract(distribuicao, "^[0-9]{2}\\/[0-9]{2}\\/[0-9]{4}"),
    distribuicao = lubridate::dmy(distribuicao),
    ano = lubridate::year(distribuicao),
    area,
    valor_da_acao,
    outros_numeros,
    digital,
    apensado_ao
  )

writexl::write_xlsx(da_fernanda, "data-raw/fernanda-portella/da_fernanda.xlsx")
