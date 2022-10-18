# Base André

da_rjsp <- obsFase2::da_relatorio |>
  dplyr::filter(epp_ou_me == "Empresa de Pequeno Porte (EPP)" | epp_ou_me == "Micro Empresa (ME)") |>
  dplyr::mutate(
    plano_desfecho = dplyr::case_when(
      resultado_final == "Aprovação do plano" ~ "O plano foi aprovado",
      resultado_final == "Ainda em negociação" ~ "Ainda não foi aprovado nem reprovado",
      cram_down == "Sim" ~ "Cram Down",
      is.na(resultado_final) ~ "(Vazio)",
      TRUE ~ "O plano foi reprovado (a empresa/grupo faliu)"
    ),
    desfecho_final = dplyr::case_when(
      is.na(desfecho_final) ~ "(Vazio)",
      desfecho_final == "Ainda em curso" ~ "Ainda não encerrou o cumprimento do plano",
      desfecho_final == "Encerramento sem falência" ~ "Cumprimento do plano encerrado",
      desfecho_final == "Falência" ~ "Faliu cumprindo o plano"
    ),
    plano_uno = dplyr::case_when(
      is.na(houve_decisao_plano_uno) ~ "(Vazio)",
    )
  ) |>
  dplyr::transmute(
    id_processo = n_processo,
    ano_dist = lubridate::year(data_dist),
    grupo_nome = NA_character_,
    comarca,
    capital,
    emenda,
    emenda_pedido_teve = "(Vazio)",
    plano_uno = houve_decisao_plano_uno,
    aj_listcred_valor = NA,
    requerente_listcred_valor = NA,
    faliu_antes_agc,
    plano_desfecho,
    resultado_final,
    data_falencia,
    tipo_societario = "(Vazio)",
    balanco_unificado = "(Vazio)",
    patrimonio_liquido = NA_real_,
    faturamento = faturamento_total,
    passivos = total_passivo,
    ativos = total_ativo,
    faixa_faturamento,
    faixa_passivo,
    faixa_ativo,
    aj_tipo = "(Vazio)",
    aj_nome = "(Vazio)",
    faliu_antes_agc,
    plano_desfecho,
    comite_credores = "(Vazio)",
    cram_down,
    legalidade_plano = "(Vazio)",
    legalidade_plano_momento = "(Vazio)",
    n_agc,
    pgto_prazo_classe1 = classe1_prazo,
    pgto_prazo_classe2 = classe2_prazo,
    pgto_prazo_classe3 = classe3_prazo,
    pgto_prazo_classe4 = NA_real_,
    desagio_valor_classe1 = classe1_desagio,
    desagio_valor_classe2 = classe2_desagio,
    desagio_valor_classe3 = classe3_desagio,
    desagio_valor_classe4 = NA_real_
  )

writexl::write_xlsx(da_rjsp, path = "data-raw/nepi_2022/xlsx/da_rjsp.xlsx")

falencia_empresas <- obsFase3::aux_rfb |>
  dplyr::select(cnpj, porte_empresa)

processos_f_epp_me <- obsFase3::aux_polo_ativo_cnpj |>
  dplyr::left_join(falencia_empresas) |>
  dplyr::filter(
    stringr::str_detect(nome, "EPP") |
      porte_empresa == "ME") |>
  dplyr::pull(id_processo)

da_fsp <- obsFase3::da_processo_tidy |>
  dplyr::filter(id_processo %in% processos_f_epp_me) |>
  dplyr::select(
    id_processo,
    ano_dist,
    info_foro,
    info_conv,
    listcred_aj_teve,
    listcred_devedor_val,
    listcred_aj_val,
    aj_pfpj,
    info_ativo_val,
    dt_dist,
    info_origem,
    info_aj_crime
  )

writexl::write_xlsx(da_fsp, path = "data-raw/nepi_2022/xlsx/da_fsp.xlsx")
