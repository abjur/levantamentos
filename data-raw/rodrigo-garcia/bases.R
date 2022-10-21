# Rodrigo Garcia


# f_sp --------------------------------------------------------------------
f_sp_processos <- obsFase3::da_processo_tidy |>
  # dplyr::glimpse()
  dplyr::select(
    id_processo,
    dt_dist,
    info_foro,
    dt_decisao,
    dt_fal_fim,
    listcred_aj_val,
    info_leilao,
    info_leilao_justif,
    info_ativo_val,
    info_obrig_extin,
    aj_tipo_remu,
    dplyr::contains("pgto")
  )

f_sp_avaliacao <- obsFase3::da_avaliacao_tidy

f_sp_leilao <- obsFase3::da_leilao_tidy

link_f <- "https://docs.google.com/spreadsheets/d/1nHI8EA_7l0_lD85V_BD8tczv847XHi8qSyz_yUc5RDU/edit#gid=0"

# googlesheets4::gs4_auth("rfeliz@abj.org")

googlesheets4::write_sheet(
  f_sp_avaliacao,
  link_f,
  "avaliacao"
)

googlesheets4::write_sheet(
  f_sp_leilao,
  link_f,
  "leilao"
)

googlesheets4::write_sheet(
  f_sp_processos,
  link_f,
  "processos"
)
# rj -------------------------------------------------------------------
# sp
rj_sp_processos <- obsFase2::da_relatorio |>
  # dplyr::glimpse()
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
    )
  ) |>
  dplyr::transmute(
    id_processo = n_processo,
    data_dist,
    comarca,
    capital,
    requerente_listcred_valor = NA,
    aj_listcred_valor = NA,
    aj_remu_valor = NA,
    remu_divida = NA,
    plano_desfecho,
    data_aprovacao,
    data_concessao,
    leilao,
    upi,
    desfecho_final,
    data_fim,
    data_falencia,
    balanco_unificado = NA,
    patrimonio_liquido = NA,
    faturamento = faturamento_total,
    faixa_faturamento,
    passivos = total_passivo,
    faixa_passivo,
    ativos = total_ativo,
    faixa_ativo
  )


# rj
rj_rj_processos <- obsRJRJ::da_processo_tidy |>
  # dplyr::glimpse()
  dplyr::transmute(
    id_processo,
    data_dist,
    comarca,
    capital,
    requerente_listcred_valor = NA,
    aj_listcred_valor,
    aj_remu_valor,
    remu_divida,
    plano_desfecho,
    data_aprovacao,
    data_concessao,
    leilao,
    upi,
    desfecho_final,
    data_fim,
    data_falencia,
    balanco_unificado,
    patrimonio_liquido,
    faturamento,
    faixa_faturamento,
    passivos,
    faixa_passivo,
    ativos,
    faixa_ativo
  )

# rs
rj_rs_processos <- obsRJRS::da_processo_tidy |>
  # dplyr::glimpse()
  dplyr::select(
    id_processo,
    data_dist,
    comarca,
    capital,
    requerente_listcred_valor,
    aj_listcred_valor,
    aj_remu_valor,
    remu_divida,
    plano_desfecho,
    data_aprovacao,
    data_concessao,
    leilao,
    upi,
    desfecho_final,
    data_fim,
    data_falencia,
    balanco_unificado,
    patrimonio_liquido,
    faturamento,
    faixa_faturamento,
    passivos,
    faixa_passivo,
    ativos,
    faixa_ativo
  )

link_rj <- "https://docs.google.com/spreadsheets/d/1vKBO50eQzb6QH36-g0y4iyYhWk0Fp8EfQyxQo2UemvQ/edit#gid=0"


googlesheets4::write_sheet(
  rj_sp_processos,
  link_rj,
  "sp"
)

googlesheets4::write_sheet(
  rj_rj_processos,
  link_rj,
  "rj"
)

googlesheets4::write_sheet(
  rj_rs_processos,
  link_rj,
  "rs"
)
