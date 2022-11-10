da <- obsFase3::da_processo_tidy |>
  dplyr::filter(
    ano_dist >= 2015 & ano_dist <= 2020,
    info_fal_dec_nao == "Acordo extrajudicial"
  ) |>
  dplyr::select(
    id_processo,
    dt_extincao,
    info_fal_dec_nao
  )

writexl::write_xlsx(da, "data-raw/nepi_2022/xlsx/da_maria.xlsx")
