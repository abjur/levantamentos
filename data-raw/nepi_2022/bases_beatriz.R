
leilao <- obsFase3::da_leilao_tidy |>
  dplyr::select(
    id_processo,
    tipo, data_edital, lance_inicial, valor_avaliacao_inicial,
    o_juiz_determinou_lance_inicial, valor_lance_inicial_juiz, vendeu,
    valor_total_arrematado
  ) |>
  dplyr::rename_at(.vars = 2:9, .funs = ~paste0("leilao_", .x))


dados <- obsFase3::da_avaliacao_tidy |>
  dplyr::distinct(id_processo, .keep_all = TRUE) |>
  dplyr::inner_join(obsFase3::da_processo_tidy, "id_processo") |>
  dplyr::transmute(
    id_processo, ano_dist, info_leilao,
    info_leilao_justif = info_leilao_justif_min,
    info_ativo_val, info_fal_acabou, dt_fal_fim, dt_extincao, dt_assinatura_tc,
    avaliacao_data_laudo = data_laudo,
    avaliacao_valor = valor, avaliacao_tipo = tipo, avaliacao_qtde = qtde,
    avaliacao_descricao = descricao, avaliacao_tipo_valor = tipo_valor,
  ) |>
  dplyr::inner_join(leilao, "id_processo")

writexl::write_xlsx(dados, "data-raw/nepi_2022/xlsx/da_beatriz.xlsx")

