daRJRJ <- obsRJRJ::da_parte_tidy |>
  dplyr::distinct(id_processo, cnpj) |>
  dplyr::left_join(
    x = obsRJRJ::da_rjrj |>
      dplyr::select(id_processo, digital, dplyr::contains("leilao"), upi_teve) |>
      dplyr::mutate(
        upi_vendeu = NA_character_
      ),
    by = "id_processo"
  ) |>
  dplyr::mutate(
    tj = "RJ"
  ) |>
  dplyr::relocate(cnpj, .after = "id_processo") |>
  dplyr::relocate(tj, .after = "cnpj")

daRJSP <- obsFase2::da_relatorio |>
  dplyr::transmute(
    id_processo = n_processo,
    cnpj,
    digital = NA_character_,
    leilao_teve = leilao,
    leilao_data = as.Date(NA),
    leilao_vendeu = NA_character_,
    upi_teve = upi,
    upi_vendeu
  ) |>
  tidyr::separate_rows(cnpj, sep = ", ") |>
  dplyr::mutate(
    cnpj = cnpj |>
      abjutils::clean_cnj() |>
      stringr::str_squish(),
    tj = "SP"
  ) |>
  dplyr::relocate(tj, .after = "cnpj")

daRJRS <- obsRJRS::da_parte_tidy |>
  dplyr::distinct(id_processo, cnpj) |>
  dplyr::left_join(
    x = obsRJRS::da_rjrs |>
      dplyr::select(id_processo, digital, leilao_teve, leilao_data, leilao_vendeu, upi_teve) |>
      dplyr::mutate(
        upi_vendeu = NA_character_
      ),
    by = "id_processo"
  ) |>
  dplyr::mutate(
    tj = "RS"
  ) |>
  dplyr::relocate(cnpj, .after = "id_processo") |>
  dplyr::relocate(tj, .after = "cnpj")

cnpjs <- dplyr::bind_rows(daRJRJ, daRJSP, daRJRS)

readr::write_csv(cnpjs, "data-raw/davi-bernardo-paris/cnpjs.csv")
