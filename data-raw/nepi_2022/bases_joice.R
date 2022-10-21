# Bases Joice

da_avaliacao_joice <- obsFase3::da_avaliacao_tidy

da_leilao_joice <- obsFase3::da_leilao_tidy

da_processos_joice <- obsFase3::da_processo_tidy |>
  dplyr::select(
    id_processo,
    info_leilao,
    info_leilao_justif,
    info_ativo_val,
    dt_dist,
    dt_arrecadacao,
    aj_tipo_remu,
    aj_caucao,
    info_fal_extin_caucao,
    dplyr::contains("pgto")
  )

writexl::write_xlsx(da_avaliacao_joice, "data-raw/nepi_2022/xlsx/da_avaliacao_joice.xlsx")
writexl::write_xlsx(da_leilao_joice, "data-raw/nepi_2022/xlsx/da_leilao_joice.xlsx")
writexl::write_xlsx(da_processos_joice, "data-raw/nepi_2022/xlsx/da_processos_joice.xlsx")
