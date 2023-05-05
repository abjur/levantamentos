stay_rj <- obsRJRJ::da_processo_tidy |>
  # dplyr::filter(stay_prorrogado == "Sim") |>
  dplyr::transmute(
    id_processo,
    origem = "RJ",
    stay_prorrogado
  )

stay_sp <- obsFase2::da_relatorio |>
  # dplyr::filter(stay_prorrogado == "Sim") |>
  dplyr::transmute(
    id_processo = n_processo,
    origem = "SP",
    stay_prorrogado
  )

stay_rs <- obsRJRS::da_processo_tidy |>
  # dplyr::filter(stay_prorrogado == "Sim") |>
  dplyr::transmute(
    id_processo,
    origem = "RS",
    stay_prorrogado
  )

stay <- stay_sp |>
  dplyr::bind_rows(stay_rj) |>
  dplyr::bind_rows(stay_rs) |>
  dplyr::mutate(stay_prorrogado = tidyr::replace_na(stay_prorrogado, "(Vazio)"))

stay |>
  dplyr::count(origem, stay_prorrogado) |>
  dplyr::mutate(
    taxa_stay_period = n/sum(n),
    taxa_stay_period = formattable::percent(taxa_stay_period)
  ) |>
  dplyr::filter(stay_prorrogado == "Sim") |>
  janitor::adorn_totals()
