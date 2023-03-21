# Pedro Ivo


# base processos ----------------------------------------------------------

da_processos_pedro <- obsFase3::da_processo_tidy |>
  dplyr::filter(info_digital == "Sim") |>
  dplyr::mutate(
    ano_dist = dplyr::case_when(
      ano_dist == "2018, 2018" ~ "2018",
      ano_dist == "2019, 2019" ~ "2019",
      ano_dist == "2011, 2011" ~ "2011",
      ano_dist == "2015, 2015" ~ "2015",
      TRUE ~ ano_dist
    ),
    ano_dist = as.numeric(ano_dist),
    info_fal_acabou = dplyr::coalesce(info_fal_acabou, info_fal_acabou2),
    dt_arrecadacao = dplyr::coalesce(dt_arrecadacao, dt_arrecadacao2)
    # tempo_dec_assinatura_tc = dt_assinatura_tc - dt_decisao,
    # tempo_assinatura_tc_listcred_devedor = dt_listcred_devedor - dt_assinatura_tc,
    # tempo_listcred_devedor_listcred_aj = dt_listcred_aj - dt_listcred_devedor,
    # tempo_listcred_aj_relatorio = dt_relatorio
  ) |>
  dplyr::select(
    id_processo,
    ano_dist,
    info_digital,
    info_foro,
    dt_decisao,
    listcred_devedor_teve,
    dt_listcred_devedor,
    listcred_aj_teve,
    dt_listcred_aj,
    aj_pfpj,
    info_leilao,
    info_ativo_val,
    info_fal_acabou,
    dt_fal_fim,
    dt_extincao,
    dt_assinatura_tc,
    info_aj_relatorio,
    dt_relatorio,
    dt_arrecadacao,
    dplyr::contains("pgto")
  )

writexl::write_xlsx(da_processos_pedro, "data-raw/nepi_2022/xlsx/da_pedro_processos.xlsx")
# base leilao -------------------------------------------------------------

processos <- da_processos_pedro |>
  dplyr::pull(id_processo)

da_leilao_pedro <- obsFase3::da_leilao_tidy |>
  dplyr::filter(id_processo %in% processos) |>
  dplyr::select(
    id_processo,
    descricao,
    modalidade,
    data_edital,
    lances_a_partir_de_qual_valor,
    valor_avaliacao_inicial,
    vendeu,
    valor_total_arrematado
  )

writexl::write_xlsx(da_leilao_pedro, "data-raw/nepi_2022/xlsx/da_pedro_leilao.xlsx")
