library(magrittr)

# SP ----------------------------------------------------------------------

obsFase2::da_relatorio %>%
  dplyr::filter(data_dist >= "2015-01-01") %>%
  dplyr::transmute(
    id_processo = abjutils::build_id(n_processo), litisconsorcio, cnpj, comarca,
    data_distribuicao = data_dist, deferido, data_decisao_deferimento,
    data_primeira_agc = data_agc1, data_ultima_agc = data_agcn, data_aprovacao,
    resultado_final, acabou, desfecho_final, data_fim
  ) %>%
  writexl::write_xlsx("data-raw/insolvencia-marcus/obsfase2.xlsx")


# RJ ----------------------------------------------------------------------

build_cnpj <- function(id)
{
  stringr::str_c(
    substr(id, 1, 2), ".", substr(id, 3, 5), ".", substr(id, 6, 8), "/",
    substr(id, 9, 12), "-", substr(id, 13, 14)
  )
}

rj_cnpjs <- obsRJRJ::da_parte_tidy %>%
  dplyr::mutate(ncnpj = stringr::str_length(cnpj)) %>%
  dplyr::filter(!is.na(ncnpj)) %>%
  dplyr::mutate(cnpj = ifelse(
    ncnpj == 14,
    build_cnpj(cnpj),
    cnpj
  )) %>%
  dplyr::group_by(id_processo) %>%
  dplyr::summarise(cnpj = stringr::str_c(cnpj, collapse = ", ")) %>%
  dplyr::filter(stringr::str_detect(cnpj, ","))

obsRJRJ::da_processo_tidy %>%
  dplyr::filter(dplyr::between(ano_dist, 2015, 2017)) %>%
  dplyr::transmute(
    id_processo, litisconsorcio, grupo_nome, comarca,
    data_distribuicao = data_dist, deferido, data_decisao_deferimento,
    data_primeira_agc = data_agc1, data_ultima_agc = data_agcn, data_aprovacao,
    resultado_agc = agc_res, plano_desfecho, desfecho_final, data_fim
  ) %>%
  dplyr::left_join(rj_cnpjs, "id_processo") %>%
  dplyr::relocate(cnpj, .after = grupo_nome) %>%
  writexl::write_xlsx("data-raw/insolvencia-marcus/obsrjrj.xlsx")







