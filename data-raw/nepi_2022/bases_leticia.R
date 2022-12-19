# Letícia Fornel

rfb_natureza <- obsFase3::aux_rfb |>
  dplyr::select(cnpj, natureza = nm_subclass_natureza_juridica) |>
  dplyr::filter(cnpj %in% cnpjs)

tidy_planilha <- function(df) {
  dplyr::mutate(
    df,
    dplyr::across(.fns = as.character)
  )
}

processos_com_individual <- obsFase3::da_processo_tidy |>
  dplyr::select(id_processo, planilha_partes) |>
  dplyr::mutate(planilha_partes = purrr::map(planilha_partes, tidy_planilha)) |>
  tidyr::unnest(planilha_partes) |>
  dplyr::filter(!is.na(cnpj)) |>
  dplyr::select(id_processo, cnpj) |>
  dplyr::mutate(
    cnpj = abjutils::clean_cnj(cnpj)
  ) |>
  dplyr::left_join(rfb_natureza, by = "cnpj") |>
  dplyr::filter(!is.na(natureza)) |>
  dplyr::group_by(id_processo) |>
  dplyr::summarise(
    natureza = paste0(natureza, collapse = ", ")
  ) |>
  dplyr::ungroup()

da_processos_leticia <- obsFase3::da_processo_tidy |>
  dplyr::filter(id_processo %in% processos_com_individual$id_processo) |>
  dplyr::left_join(processos_com_individual, by = "id_processo") |>
  dplyr::mutate(
    individual = stringr::str_detect(natureza, "ndividual")
  ) |>
  dplyr::select(
    id_processo,
    natureza,
    individual,
    ano_dist,
    info_classe,
    info_assunto,
    info_autofal,
    dt_decisao,
    info_fal_dec,
    dt_listcred_aj,
    info_leilao,
    info_ativo_val,
    info_fal_acabou,
    dt_fal_fim,
    dt_dist,
    info_origem,
    dt_extincao,
    info_obrig_extin,
    dt_arrecadacao,
    info_fal_extin_caucao,
    dplyr::contains("pgto")
  )

writexl::write_xlsx(da_processos_leticia, "data-raw/nepi_2022/xlsx/da_processos_leticia.xlsx")


# avaliacao -----------------------------------------------------------------

da_leticia_avaliacoes <- obsFase3::da_avaliacao_tidy |>
  dplyr::filter(id_processo %in% processos_com_individual$id_processo) |>
  dplyr::select(
    id_processo,
    tipo,
    qtde,
    valor,
    descricao
  )

writexl::write_xlsx(da_leticia_avaliacoes, "data-raw/nepi_2022/xlsx/da_leticia_avaliacoes.xlsx")

