# NEPI 2022 - Thais Hanessaka


# bases -------------------------------------------------------------------
da_processos <- obsFase3::da_processo_tidy |>
  dplyr::filter(info_conv != "(Vazio)" & info_conv != "Outros") |>
  dplyr::select(
    id_processo,
    ano_dist,
    info_classe,
    info_assunto,
    info_digital,
    info_foro,
    info_conv,
    info_autofal,
    info_fal_dec,
    info_fal_dec_fund,
    dt_dist,
    dt_decisao,
    listcred_devedor_teve,
    listcred_devedor_val,
    dt_listcred_devedor,
    listcred_devedor_aj,
    listcred_aj_teve,
    listcred_aj_val,
    dt_listcred_aj,
    aj_pfpj,
    info_leilao,
    info_leilao_justif,
    info_ativo_val,
    info_104,
    info_fal_acabou,
    dt_fal_fim,
    info_origem,
    dt_extincao,
    info_aj_crime,
    dt_assinatura_tc,
    info_aj_relatorio,
    info_aj_relatorio_mp,
    dt_relatorio,
    info_obrig_extin,
    dt_arrecadacao,
    aj_tipo_remu,
    aj_caucao,
    info_fal_extin_caucao,
    dplyr::contains("pgto")
  )

processos <- da_processos |>
  dplyr::pull(id_processo)

da_avaliacao <- obsFase3::da_avaliacao_tidy |>
  dplyr::filter(id_processo %in% processos)

da_leilao <- obsFase3::da_leilao_tidy |>
  dplyr::filter(id_processo %in% processos)


# n casos -----------------------------------------------------------------

da_avaliacao |>
  dplyr::distinct(id_processo) |>
  dplyr::count()

da_leilao |>
  dplyr::distinct(id_processo) |>
  dplyr::count()

# drive -------------------------------------------------------------------

googlesheets4::gs4_auth("rfeliz@abj.org.br")

link <- "https://docs.google.com/spreadsheets/d/1sB67CtDx7hZqk9-aksCo9enh2mB8z8Fj8-7nJjw9G_M/edit#gid=0"

googlesheets4::write_sheet(
  da_processos,
  link,
  "processos"
)

googlesheets4::write_sheet(
  da_avaliacao,
  link,
  "avaliacao"
)

googlesheets4::write_sheet(
  da_leilao,
  link,
  "leilao"
)
