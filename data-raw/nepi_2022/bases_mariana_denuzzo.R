# Bases Mariana Denuzzo e José Afonso Leirião Filho

unique(obsFase3::da_processo_tidy$info_fal_dec)

da <- obsFase3::da_processo_tidy |>
  dplyr::filter(
    info_fal_dec == "Sim" &
    (info_fal_acabou == "Sim" | info_fal_acabou2 == "Sim")
  ) |>
  dplyr::mutate(info_fal_acabou = "Sim") |>
  dplyr::select(
    id_processo,
    ano_dist,
    info_digital,
    info_foro,
    info_conv,
    info_autofal,
    dt_decisao,
    info_fal_dec,
    listcred_devedor_teve,
    listcred_devedor_val,
    info_leilao,
    info_leilao_justif,
    info_ativo_val,
    info_fal_acabou,
    dt_fal_fim,
    dt_dist,
    dt_extincao,
    info_aj_crime,
    dt_assinatura_tc,
    info_aj_relatorio,
    info_aj_relatorio_mp,
    info_obrig_extin,
    dt_arrecadacao,
    info_fal_extin_caucao,
    dplyr::contains("pgto")
  )

writexl::write_xlsx(da, "data-raw/nepi_2022/xlsx/da_mariana_denuzzo.xlsx")
