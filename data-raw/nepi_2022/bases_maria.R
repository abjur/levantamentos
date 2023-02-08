da <- obsFase3::da_processo_tidy |>
  dplyr::filter(
    # ano_dist >= 2015 & ano_dist <= 2020,
    info_fal_dec_nao %in% c(
      "Acordo extrajudicial",
      "Abandono/Desistência/Falta de interesse da requerente"
    )
  ) |>
  dplyr::mutate(
    ano_dist = ifelse(ano_dist == "2015, 2015", "2015", ano_dist)
  ) |>
  dplyr::select(
    id_processo,
    ano_dist,
    info_classe,
    info_assunto,
    info_foro,
    info_fal_dec_fund,
    dt_fal_fim,
    dt_dist,
    info_origem,
    info_obrig_extin
  )

writexl::write_xlsx(da, "data-raw/nepi_2022/xlsx/da_maria.xlsx")
