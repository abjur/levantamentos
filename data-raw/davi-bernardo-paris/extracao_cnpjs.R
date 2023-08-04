cnpjRJRJ <- obsRJRJ::aux_rfb |>
  dplyr::distinct(cnpj) |>
  dplyr::transmute(
    tj = "RJ",
    cnpj
  )

cnpjRJSP <- obsFase2::da_relatorio |>
  dplyr::distinct(cnpj) |>
  dplyr::transmute(
    tj = "SP",
    cnpj
  )

cnpjRJRS <- obsRJRS::aux_rfb |>
  dplyr::distinct(cnpj) |>
  dplyr::transmute(
    tj = "RS",
    cnpj
  )

cnpjs <- dplyr::bind_rows(cnpjRJRJ, cnpjRJSP, cnpjRJRS)

readr::write_csv(cnpjs, "data-raw/davi-bernardo-paris/cnpjs.csv")
