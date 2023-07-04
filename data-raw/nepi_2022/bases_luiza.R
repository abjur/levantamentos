# Luiza Gianotti

# Diante disso, poderia, por gentileza, disponibilizar a base dos processos de
# falência apenas da Capital (Foro Central Cível), no período de 2010 a 2020?
# São quantos processos, neste caso?

da_luiza <- obsFase3::da_processo_tidy |>
  dplyr::filter(info_foro == "Capital") |>
  dplyr::select(
    id_processo,
    ano_dist,
    info_fal_dec,
    info_fal_dec_fund,
    listcred_devedor_teve,
    listcred_devedor_val,
    dt_listcred_devedor,
    listcred_devedor_aj,
    listcred_aj_teve,
    listcred_aj_val,
    dt_listcred_aj,
    info_ativo_val,
    dt_extincao,
    info_fal_extin_caucao,
    dt_decisao
  )

writexl::write_xlsx(da_luiza, "data-raw/nepi_2022/xlsx/da_luiza.xlsx")


