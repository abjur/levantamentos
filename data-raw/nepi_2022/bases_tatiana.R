# Bases - Tatiana Adoglio

avaliacao <- obsFase3::da_avaliacao_tidy |>
  dplyr::distinct(id_processo, data_laudo) |>
  dplyr::group_by(id_processo) |>
  dplyr::arrange(data_laudo) |>
  dplyr::slice_min(data_laudo,na_rm = TRUE)

processos_com_avaliacao <- avaliacao |>
  dplyr::pull(id_processo)

# filtro = processos com avaliação
# select = data da primeira avaliação e data da decretação

da <- obsFase3::da_processo_tidy |>
  dplyr::filter(id_processo %in% processos_com_avaliacao) |>
  dplyr::select(
    id_processo,
    dt_decisao,
    dt_arrecadacao
  ) |>
  dplyr::left_join(avaliacao) |>
  dplyr::mutate(
    tempo = data_laudo - dt_decisao
  ) |>
  dplyr::filter(tempo > 0) |>
  dplyr::mutate(
    cat_tempo = dplyr::case_when(
      tempo < quantile(tempo, 0.25) ~ "rápido",
      tempo > quantile(tempo, 0.25) & tempo < quantile(tempo, 0.50) ~ "médio-rápido",
      tempo > quantile(tempo, 0.50) & tempo < quantile(tempo, 0.75) ~ "médio-lento",
      tempo > quantile(tempo, 0.75) ~ "lento"
    ),
    cat_tempo = forcats::fct_relevel(cat_tempo, c("rápido", "médio-rápido", "médio-lento", "lento"))
  ) |>
  dplyr::arrange(tempo)

writexl::write_xlsx(da, "data-raw/nepi_2022/xlsx/da_tempos_tatiana.xlsx")


# comparação com o artigo do Marcelo --------------------------------------

obsoc::da_processo_tidy |>
  dplyr::mutate(
    apur_haver = decisao_geral == "Apuração de haveres"
  ) |>
  dplyr::count(apur_haver) |>
  dplyr::mutate(
    prop = n/sum(n)
  )

obsoc::da_processo_tidy |>
  dplyr::filter(stringr::str_detect(assunto_contrapedido, stringr::regex("apuração de haveres", TRUE))) |>
  dplyr::mutate(
    decisao_apur_haver = stringr::str_detect(decisao_sentenca, stringr::regex("apuração de haveres", TRUE))
  ) |>
  dplyr::count(decisao_apur_haver) |>
  dplyr::mutate(
    prop = n/sum(n)
  )

# novas bases sobre arrecadação ---------------------------------------------------------------

da_avaliacao_tatiana <- obsFase3::da_avaliacao_tidy |>
  dplyr::select(
    id_processo,
    tipo,
    descricao,
    valor,
    tipo_valor
  )

processos_com_avaliacao2 <- da_avaliacao_tatiana |>
  dplyr::distinct(id_processo) |>
  dplyr::pull(id_processo)

da_processos_tatiana <- obsFase3::da_processo_tidy |>
  dplyr::filter(id_processo %in% processos_com_avaliacao2) |>
  dplyr::select(
    id_processo,
    dt_decisao,
    dt_arrecadacao,
    info_digital,
    info_foro,
    aj_pfpj,
    info_ativo_val,
    aj_tipo_remu,
    aj_caucao,
    info_fal_extin_caucao
  )

writexl::write_xlsx(da_avaliacao_tatiana, "data-raw/nepi_2022/xlsx/da_avaliacao_tatiana.xlsx")
writexl::write_xlsx(da_processos_tatiana, "data-raw/nepi_2022/xlsx/da_processos_tatiana.xlsx")
