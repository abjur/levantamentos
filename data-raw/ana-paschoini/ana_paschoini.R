# Ana Paschoini

sp <- obsFase2::da_relatorio |>
  dplyr::filter(leilao == "Sim" | upi == "Sim") |>
  dplyr::transmute(
    id_processo = n_processo,
    dt_dist = dplyr::coalesce(data_dist, data_dist2),
    origem = "SP",
    upi_vendeu
  )

rj <- obsRJRJ::da_processo_tidy |>
  dplyr::filter(leilao == "Sim" | upi == "Sim") |>
  dplyr::transmute(
    id_processo,
    dt_dist = data_dist,
    origem = "RJ"
  )

rs <- obsRJRS::da_processo_tidy |>
  dplyr::filter(leilao == "Sim" | upi == "Sim") |>
  dplyr::transmute(
    id_processo,
    dt_dist = data_dist,
    origem = "RS"
  )

da_ana_paschoini <- dplyr::bind_rows(sp, rj, rs)

writexl::write_xlsx(da_ana_paschoini, "data-raw/ana-paschoini/da_ana_paschoini.xlsx")
