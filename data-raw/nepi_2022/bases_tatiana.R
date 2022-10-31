# Bases - Tatiana Adoglio

da <- obsFase3::da_processo_tidy |>
  dplyr::filter(info_fal_acabou == "Sim" | info_fal_acabou2 == "Sim") |>
  dplyr::select(
    id_processo,
    dt_dist
  )
