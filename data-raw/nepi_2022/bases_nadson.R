# Bases Nadson Costa Cerqueira

# base I ------------------------------------------------------------------

# I) Vamos analisar a mediação nos processos de falência e buscar entender sobre o êxito deste instrumento nos processos de falência. Por isso, precisamos dos seguintes dados:

# a) Foro de distribuição: Capital e RAJ
# b) Data da distribuição da ação: Aqui vamos fazer um recorte temporal a partir de 18/03/2016.
# c) Decisão e Motivo de não decretação de falência: Acreditamos que com estas informações podemos entender se houve mediação nos processos de falência.
# d) Valor da causa: Aqui vamos relacionar o valor da causa à ocorrência de mediação, para entender se a mediação acaba sendo mais efetiva em falências fundamentadas em dívidas maiores ou menores.

da_nadson1 <- obsFase3::da_processo_tidy |>
  dplyr::mutate(
    info_fal_dec = dplyr::case_when(
      !is.na(listcred_devedor_val) ~ "Sim",
      !is.na(listcred_aj_val) ~ "Sim"
    )
  ) |>
  dplyr::filter(dt_dist > as.Date("2016-03-18") & dt_dist < as.Date("2020-12-12")) |>
  dplyr::select(
    id_processo,
    info_foro,
    dt_dist,
    info_fal_dec,
    info_fal_dec_fund,
    listcred_devedor_val,
    listcred_aj_val,
    info_ativo_val
  )

writexl::write_xlsx(da_nadson1, "data-raw/nepi_2022/xlsx/da_nadson1.xlsx")

# base II -----------------------------------------------------------------
da_nadson2 <- obsFase3::da_processo_tidy |>
  dplyr::mutate(
    info_fal_dec = dplyr::case_when(
      !is.na(listcred_devedor_val) ~ "Sim",
      !is.na(listcred_aj_val) ~ "Sim"
    )
  ) |>
  dplyr::filter(info_fal_dec == "Sim") |>
  dplyr::select(
    id_processo,
    info_foro,
    dt_dist,
    info_fal_dec,
    info_fal_dec_fund,
    listcred_devedor_val,
    listcred_aj_val,
    info_ativo_val
  )

writexl::write_xlsx(da_nadson2, "data-raw/nepi_2022/xlsx/da_nadson2.xlsx")
