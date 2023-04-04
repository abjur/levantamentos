# Gabriel Hess

# nome da empresa,
# CNPJ,
# processo de RJ,
# data do pedido,
# AJ,
# faturamento,
# volume do QGC, deságio, prazo e forma de pagamento, taxa de juros e índice de correção.


# São Paulo ---------------------------------------------------------------

da_rjsp <- obsFase2::da_relatorio |>
  dplyr::transmute(
    id_processo = n_processo,
    origem = "SP",
    dt_dist = dplyr::coalesce(data_dist, data_dist2),
    cnpj = stringr::str_remove_all(cnpj, "\\.|\\/|-"),
    aj_nome = NA_character_,
    faturamento_total,
    divida,
    pgto_prazo_classe1 = classe1_prazo,
    pgto_periodicidade_classe1 = classe1_periodicidade,
    desagio_valor_classe1 = classe1_desagio,
    indice_classe1 = classe1_indice_correcao,
    juros_valor_classe1 = classe1_juros,
    pgto_prazo_classe2 = classe2_prazo,
    pgto_periodicidade_classe2 = classe2_periodicidade,
    desagio_valor_classe2 = classe2_desagio,
    indice_classe2 = classe2_indice_correcao,
    juros_valor_classe2 = classe2_juros,
    pgto_prazo_classe3 = classe3_prazo,
    pgto_periodicidade_classe3 = classe3_periodicidade,
    desagio_valor_classe3 = classe3_desagio,
    indice_classe3 = classe3_indice_correcao,
    juros_valor_classe3 = classe3_juros,
    pgto_prazo_classe4 = NA_real_,
    pgto_periodicidade_classe4 =  NA_character_,
    desagio_valor_classe4 =  NA_real_,
    indice_classe4 =  NA_character_,
    juros_valor_classe4 =  NA_real_
  )

# Rio de Janeiro ----------------------------------------------------------

partes_rjrj <- obsRJRJ::da_parte_tidy |>
  dplyr::mutate(
    faturamento = ifelse(
      stringr::str_detect(faturamento, "[:digits]"), NA_real_, as.numeric(faturamento))
  ) |>
  dplyr::group_by(id_processo) |>
  dplyr::summarise(
    cnpj = paste0(cnpj, collapse = ", ")
  ) |>
  dplyr::ungroup()

da_rjrj <- obsRJRJ::da_processo_tidy |>
  dplyr::left_join(partes_rjrj) |>
  dplyr::transmute(
    id_processo,
    origem = "RJ",
    dt_dist = data_dist,
    cnpj,
    aj_nome,
    faturamento_total = faturamento,
    divida = passivos,
    pgto_prazo_classe1,
    pgto_periodicidade_classe1,
    desagio_valor_classe1,
    indice_classe1,
    juros_valor_classe1,
    pgto_prazo_classe2,
    pgto_periodicidade_classe2,
    desagio_valor_classe2,
    indice_classe2,
    juros_valor_classe2,
    pgto_prazo_classe3,
    pgto_periodicidade_classe3,
    desagio_valor_classe3,
    indice_classe3,
    juros_valor_classe3,
    pgto_prazo_classe4,
    pgto_periodicidade_classe4,
    desagio_valor_classe4,
    indice_classe4,
    juros_valor_classe4
  )


# Rio Grande do Sul -------------------------------------------------------

partes_rjrs <- obsRJRS::da_parte_tidy |>
  dplyr::group_by(id_processo) |>
  dplyr::summarise(
    cnpj = paste0(cnpj, collapse = ", "),
    faturamento_total = sum(as.numeric(faturamento), na.rm = TRUE)
  ) |>
  dplyr::ungroup()

da_rjrs <- obsRJRS::da_processo_tidy |>
  dplyr::left_join(partes_rjrs) |>
  dplyr::transmute(
    id_processo,
    origem = "RS",
    dt_dist = data_dist,
    cnpj,
    aj_nome,
    faturamento_total = faturamento,
    divida = passivos,
    pgto_prazo_classe1,
    pgto_periodicidade_classe1,
    desagio_valor_classe1,
    indice_classe1,
    juros_valor_classe1,
    pgto_prazo_classe2,
    pgto_periodicidade_classe2,
    desagio_valor_classe2,
    indice_classe2,
    juros_valor_classe2,
    pgto_prazo_classe3,
    pgto_periodicidade_classe3,
    desagio_valor_classe3,
    indice_classe3,
    juros_valor_classe3,
    pgto_prazo_classe4,
    pgto_periodicidade_classe4,
    desagio_valor_classe4,
    indice_classe4,
    juros_valor_classe4
  )


# join --------------------------------------------------------------------

da <- da_rjrj |>
  dplyr::bind_rows(da_rjrs) |>
  dplyr::bind_rows(da_rjsp)

writexl::write_xlsx(da, "data-raw/gabriel_hess/da.xlsx")
