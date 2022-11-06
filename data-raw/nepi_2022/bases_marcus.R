# bases marcus

da <- obsFase3::da_processo_tidy |>
  dplyr::select(
    id_processo,
    ano_dist,
    info_digital,
    info_foro,
    info_fal_dec,
    listcred_aj_val,
    aj_pfpj,
    info_ativo_val,
    info_fal_acabou,
    dt_dist,
    info_aj_crime,
    dt_arrecadacao,
    aj_tipo_remu,
    info_fal_extin_caucao
  ) |>
  dplyr::filter(
    info_fal_dec == "Sim" & info_fal_extin_caucao == "Não"
  )

writexl::write_xlsx(da, "data-raw/nepi_2022/xlsx/da_marcus.xlsx")
