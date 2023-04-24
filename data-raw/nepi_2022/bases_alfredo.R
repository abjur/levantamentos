vars <- c(
  "id_processo",
  "ano_dist",
  "info_classe",
  "info_assunto",
  "info_digital",
  "info_foro",
  "info_conv",
  "info_autofal",
  "dt_decisao",
  "info_fal_dec",
  "info_fal_dec_fund",
  "listcred_devedor_teve",
  "listcred_devedor_val",
  "dt_listcred_devedor",
  "listcred_devedor_aj",
  "listcred_aj_teve",
  "listcred_aj_val",
  "dt_listcred_aj",
  "aj_pfpj",
  "info_leilao",
  "info_leilao_justif",
  "info_ativo_val",
  "info_104",
  "info_fal_acabou",
  "dt_fal_fim",
  "dt_dist",
  "info_origem",
  "dt_extincao",
  "info_aj_crime",
  "dt_assinatura_tc",
  "info_aj_relatorio",
  "info_aj_relatorio_mp",
  "dt_relatorio",
  "info_obrig_extin",
  "dt_arrecadacao",
  "aj_tipo_remu",
  "aj_caucao",
  "info_fal_extin_caucao"
)

obsFase3::da_processo_tidy |>
  dplyr::filter(listcred_quadro == "Sim") |>
  dplyr::select(all_of(vars)) |>
  writexl::write_xlsx("data-raw/nepi_2022/xlsx/da_alfredo_processos_listcred_quadro.xlsx")


obsFase3::da_processo_tidy |>
  dplyr::filter(info_fal_acabou == "Sim" | info_fal_acabou2 == "Sim") |>
  dplyr::select(all_of(vars), listcred_quadro) |>
  writexl::write_xlsx("data-raw/nepi_2022/xlsx/da_alfredo_processos_encerrados.xlsx")

# pagamento
classes <- paste(
  "pgto",
  c("trib", "trab", "especial", "geral", "quiro", "subquiro", "prio", "extra"),
  sep = "_"
)
obsFase3::da_processo_tidy |>
  dplyr::filter(!dplyr::if_all(dplyr::contains(classes), is.na)) |>
  dplyr::select(all_of(vars)) |>
  writexl::write_xlsx("data-raw/nepi_2022/xlsx/da_alfredo_pagamento.xlsx")
