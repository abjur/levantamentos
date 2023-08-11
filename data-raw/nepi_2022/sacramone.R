rjsp_maiores_dividas <- obsFase2::da_relatorio |>
  dplyr::arrange(desc(divida)) |>
  head(20) |>
  dplyr::select(n_processo, divida, remuneracao, tipo_remuneracao_arrumado)

writexl::write_xlsx(rjsp_maiores_dividas, "data-raw/nepi_2022/xlsx/rjsp_maiores_dividas.xlsx")
