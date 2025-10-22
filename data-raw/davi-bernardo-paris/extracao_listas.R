daRJRJ <- obsRJRJ::da_rjrj |>
  dplyr::transmute(
    id_processo,
    tj = "RJ",
    link_edital_recuperanda = pdf_listcred_req,
    link_edital_aj = pdf_listcred_aj,
    link_edital_qgc = pdf_qgc
  )

daRJSP <- obsFase2::da_relatorio |>
  dplyr::transmute(
    id_processo = n_processo,
    tj = "SP",
    link_edital_recuperanda = NA_character_,
    link_edital_aj = NA_character_,
    link_edital_qgc = NA_character_
  )

daRJRS <- obsRJRS::da_rjrs |>
  dplyr::transmute(
    id_processo,
    tj = "RS",
    link_edital_recuperanda = pdf_listcred_req,
    link_edital_aj = pdf_listcred_aj,
    link_edital_qgc = pdf_qgc
  )

editais <- dplyr::bind_rows(daRJRJ, daRJSP, daRJRS)

readr::write_csv(editais, "data-raw/davi-bernardo-paris/editais.csv")
