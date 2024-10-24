# Bases Juliana Bumachar


# primeiro pedido ---------------------------------------------------------

da <- obsFase3::da_processo_tidy |>
  dplyr::mutate(info_autofal = stringr::str_to_sentence(info_autofal)) |>
  dplyr::filter(
    info_autofal == "Sim" |
    info_fal_dec_fund == "Artigo 97-I ou 105 da Lei 11.101/2005 (autofalencia)" |
    info_origem == "Falência requerida pela própria empresa (autofalência)"
  ) |>
  dplyr::mutate(
    info_conv2 = dplyr::case_when(
      info_conv2 == "artigo 99, da Lei nº 11.101/2005" ~ "Sim, com fundmento no artigo 99, da Lei nº 11.101/2005",
      TRUE ~ info_conv2
    )
  ) |>
  dplyr::select(
    id_processo,
    info_conv = info_conv2,
    info_autofal,
    info_origem,
    dt_decisao,
    info_fal_dec,
    info_fal_dec_fund,
    dt_listcred_devedor,
    dt_listcred_aj,
    info_ativo_val,
    info_fal_acabou,
    dt_fal_fim,
    dt_dist,
    dt_extincao,
    info_aj_crime,
    info_obrig_extin,
    dt_arrecadacao,
    dplyr::contains("pgto"),
    listcred_devedor_val,
    listcred_aj_val,
    info_ativo_val
  )

partes <- obsFase3::da_processo_tidy |>
  dplyr::filter(id_processo %in% da$id_processo) |>
  dplyr::select(id_processo, planilha_partes) |>
  tidyr::unnest(planilha_partes) |>
  dplyr::filter(!is.na(nome)) |>
  dplyr::transmute(
    id_processo, polo, cnpj = stringr::str_remove_all(cnpj, "[^0-9]")
  ) |>
  dplyr::inner_join(obsFase3::aux_rfb, "cnpj") |>
  dplyr::select(
    id_processo, polo, nm_natureza_juridica, nm_subclass_natureza_juridica,
    secao, divisao, grupo, classe, subclasse
  )

arrematados <- obsFase3::da_leilao_tidy |>
  dplyr::filter(
    id_processo %in% da$id_processo,
    !is.na(valor_total_arrematado)
  ) |>
  dplyr::group_by(id_processo) |>
  dplyr::summarise(
    valor_total_arrematado_processo = sum(valor_total_arrematado),
    .groups = "drop"
  )

da <- da |>
  dplyr::left_join(partes, "id_processo") |>
  dplyr::group_by(id_processo) |>
  dplyr::slice_max(polo) |>
  dplyr::ungroup() |>
  dplyr::left_join(arrematados, "id_processo")


writexl::write_xlsx(da, "data-raw/nepi_2022/xlsx/da_bumachar.xlsx")


# segundo pedido (feito pelo Bruno) ----------------------------------------------------------


processos_bruno_nao_autofal <- obsFase3::da_processo_tidy |>
  dplyr::filter(info_origem_min != "Autofalência")

datas_leilao <- obsFase3::da_leilao_tidy |>
  dplyr::select(
    id_processo,
    data_edital
  ) |>
  dplyr::filter(id_processo %in% dplyr::pull(da_bruno_nao_autofal, id_processo)) |>
  dplyr::group_by(id_processo) |>
  dplyr::filter(data_edital == min(data_edital)) |>
  dplyr::distinct() |>
  dplyr::ungroup()


da_bruno_nao_autofal <- obsFase3::da_processo_tidy |>
  dplyr::filter(info_origem_min != "Autofalência") |>
  dplyr::left_join(datas_leilao) |>
  dplyr::transmute(
    id_processo,
    ano_dist,
    dt_decisao,
    dt_arrecadacao = dplyr::coalesce(dt_arrecadacao, dt_arrecadacao2),
    teve_arrecadacao = !is.na(dt_arrecadacao),
    dt_leilao = dplyr::coalesce(data_edital, dt_edital_leilao, dt_edital_leilao_last),
    teve_leilao = !is.na(dt_leilao),
    dt_extincao,
    teve_extincao = !is.na(dt_extincao)
  )

writexl::write_xlsx(da_bruno_nao_autofal, "data-raw/nepi_2022/xlsx/da_bruno_nao_autofal.xlsx")

