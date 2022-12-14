da_remu_aj <- obsFase3::da_processo_tidy |>
  dplyr::filter(info_fal_acabou == "Sim") |>
  dplyr::mutate(
    info_fal_acabou == "Sim",
  ) |>
  dplyr::mutate(
    aj_tipo_remu = ifelse(aj_tipo_remu == "(Vazio)", NA_character_, aj_tipo_remu),
    aj_tipo_remu = dplyr::coalesce(aj_tipo_remu, aj_tipo_remu2),
    aj_val = dplyr::coalesce(aj_val, aj_val2),
    aj_val = as.numeric(aj_val),
    aj_val = ifelse(aj_val > 0.05 & aj_tipo_remu == "Porcentual", aj_val / 100, aj_val),
    aj_tipo_remu = ifelse(aj_val < 1 & aj_tipo_remu == "Fixa", "Porcentual", aj_tipo_remu),
    list_cred = dplyr::coalesce(listcred_aj_val, listcred_devedor_val),
    aj_remu = dplyr::case_when(
      aj_tipo_remu == "Porcentual" ~ list_cred * aj_val,
      aj_tipo_remu == "Fixa" ~ aj_val
    )
  ) |>
  dplyr::select(
    id_processo,
    info_leilao_justif,
    aj_remu
  )

processos <- da_remu_aj |> dplyr::pull(id_processo)

da_remu_leiloeiro <- obsFase3::da_leilao_tidy |>
  dplyr::filter(id_processo %in% processos) |>
  dplyr::mutate(remuneracao = ifelse(remuneracao > 0.05, 0.05, remuneracao)) |>
  dplyr::distinct(id_processo, tipo_remuneracao, remuneracao) |>
  dplyr::mutate(
    remuneracao = as.numeric(remuneracao)
  )

da_custos <- da_remu_aj |>
  dplyr::left_join(da_remu_leiloeiro) |>
  dplyr::transmute(
    id_processo,
    aj_remu,
    info_leilao_justif,
    leiloeiro_remu = formattable::percent(remuneracao),
    leiloeiro_remu = ifelse(!is.na(leiloeiro_remu), paste0(leiloeiro_remu, " do valor arrematado"), leiloeiro_remu)
  )

writexl::write_xlsx(da_custos, "data-raw/nepi_2022/xlsx/da_custos.xlsx")
