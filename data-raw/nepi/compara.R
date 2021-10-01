library(magrittr)

nepi <- readxl::read_excel("data-raw/nepi/Cópia de base - débito fiscal estadual - v01.xlsx") %>%
  janitor::clean_names() %>%
  dplyr::mutate(
    n_processo = abjutils::clean_cnj(n_processo),
    cnpj_empresa = abjutils::clean_cnj(cnpj_empresa),
    valor_passivo_tributario_estadual = as.numeric(valor_passivo_tributario_estadual)
  )
dplyr::glimpse(nepi)


obstrib <- obsTrib::icms_divida %>%
  dplyr::select(cnpj, ano, valor_acumulado_divida) %>%
  dplyr::distinct()

# dados pge ---------------------------------------------------------------

da <- nepi %>%
  dplyr::mutate(ano_do_balanco = as.character(ano_do_balanco)) %>%
  dplyr::left_join(obstrib, c("cnpj_empresa" = "cnpj", "ano_do_balanco" = "ano"))

da %>%
  dplyr::mutate(dif = valor_acumulado_divida - valor_passivo_tributario_estadual) %>%
  dplyr::mutate(
    dif = abs(dif),
    dif_cat = dplyr::case_when(
      dif < 100000 ~ "até 100k",
      dif < 1000000 ~ "até 1M",
      dif < 10000000 ~ "até 10M",
      dif < 100000000 ~ "até 100M",
      dif < 1000000000 ~ "até 1Bi",
      TRUE ~ "mais de 1Bi"
  )) %>%
  dplyr::count(dif_cat)

da %>%
  ggplot2::ggplot(ggplot2::aes(
    x = valor_passivo_tributario_estadual, y = valor_acumulado_divida
  )) +
  ggplot2::geom_point() +
  ggplot2::geom_abline(slope = 1, intercept = 0)

# dados pgfn --------------------------------------------------------------

da <- nepi %>%
  dplyr::left_join(bidTrib::da_rfb_pgfn, c("cnpj_empresa" = "cnpj")) %>%
  dplyr::mutate(total_dividas_tributarias_no_balanco = as.numeric(total_dividas_tributarias_no_balanco))
da %>%
  dplyr::mutate(dif = valor_consolidado - total_dividas_tributarias_no_balanco) %>%
  dplyr::mutate(
    dif = abs(dif),
    dif_cat = dplyr::case_when(
      dif < 100000 ~ "até 100k",
      dif < 1000000 ~ "até 1M",
      dif < 10000000 ~ "até 10M",
      dif < 100000000 ~ "até 100M",
      dif < 1000000000 ~ "até 1Bi",
      TRUE ~ "mais de 1Bi"
    )) %>%
  dplyr::count(dif_cat)

writexl::write_xlsx(da, "data-raw/nepi/dados_pgfn.xlsx")
