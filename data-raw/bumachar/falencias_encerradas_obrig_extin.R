obrig_extin <- obsFase3::da_processo_tidy |>
  dplyr::filter(stringr::str_detect(info_obrig_extin, "Sim")) |>
  dplyr::select(id_processo, info_obrig_extin, info_fal_acabou)

writexl::write_xlsx(obrig_extin, "~/Documents/abj/levantamentos/data-raw/bumachar/obrig_extin.xlsx")

encerrados <- obsFase3::da_processo_tidy |>
  dplyr::filter(info_fal_acabou == "Sim") |>
  dplyr::select(id_processo, info_fal_acabou, info_obrig_extin)

writexl::write_xlsx(encerrados, "~/Documents/abj/levantamentos/data-raw/bumachar/falencias_encerradas.xlsx")
