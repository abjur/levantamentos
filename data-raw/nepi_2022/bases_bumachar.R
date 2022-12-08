# Bases Juliana Bumachar

da <- obsFase3::da_processo_tidy |>
  dplyr::mutate(info_autofal = stringr::str_to_sentence(info_autofal)) |>
  dplyr::filter(
    info_autofal == "Sim" |
    info_fal_dec_fund == "Artigo 97-I ou 105 da Lei 11.101/2005 (autofalencia)" |
    info_origem == "Falência requerida pela própria empresa (autofalência)"
  ) |>
  dplyr::mutate(
    info_conv2 = dplyr::case_when(
      info_conv2 == "artigo 99, da Lei nº 11.101/2005" ~ "Sim, com fundmento no artigo 99, da Lei nº 11.101/2005",
      TRUE ~ info_conv2
    )
  ) |>
  dplyr::select(
    id_processo,
    info_conv = info_conv2,
    info_autofal,
    info_origem,
    dt_decisao,
    info_fal_dec,
    info_fal_dec_fund,
    dt_listcred_devedor,
    dt_listcred_aj,
    info_ativo_val,
    info_fal_acabou,
    dt_fal_fim,
    dt_dist,
    dt_extincao,
    info_aj_crime,
    info_obrig_extin,
    dt_arrecadacao,
    dplyr::contains("pgto")
  )

writexl::write_xlsx(da, "data-raw/nepi_2022/xlsx/da_bumachar.xlsx")
