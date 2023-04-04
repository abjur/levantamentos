# Danielly Araujo

# produtores_rurais <- obsFase3::da_processo_tidy |>
#   dplyr::select(id_processo, parte_reqdo_cnpj) |>
#   dplyr::mutate(
#     cnpj = stringr::str_squish(parte_reqdo_cnpj),
#     cnpj =  stringr::str_remove_all(cnpj, "\\.|\\/|-| |[a-z]|[A-Z]"),
#     cnpj = stringr::str_split(cnpj, pattern = stringr::regex(",|;"))
#   ) |>
#   tidyr::unnest(cnpj) |>
#   dplyr::mutate(
#     cnpj = dplyr::case_when(
#       cnpj == "" ~ NA_character_,
#       TRUE ~ cnpj
#     ),
#     cpf = dplyr::case_when(
#       stringr::str_length(cnpj) == 11 ~ TRUE,
#       # is.na(parte_reqdo_cnpj) ~ TRUE,
#       TRUE ~ FALSE
#     )
#   ) |>
#   dplyr::filter(cpf) |>
#   dplyr::pull(id_processo)

partes <- obsFase3::da_processo_tidy |>
  dplyr::filter(purrr::map_int(planilha_partes, nrow) > 0) |>
  dplyr::mutate(planilha_partes = purrr::map(
    planilha_partes, dplyr::mutate_all, as.character
  )) |>
  tidyr::unnest(planilha_partes)

pessoas_naturais <- partes |>
  dplyr::filter(info_fal == "Sim", info_fal_dec_min == "Sim") |>
  dplyr::select(
    id_processo,
    info_digital,
    nome,
    polo,
    cnpj
  ) |>
  dplyr::mutate(
    polo = dplyr::case_when(
      polo == "Devedor" ~ "passivo",
      polo == "Credor" ~ "ativo",
      TRUE ~ polo
    ),
    polo = stringr::str_to_lower(polo)
  ) |>
  dplyr::filter(polo %in% c("passivo", "autofalência")) |>
  dplyr::mutate(
    cnpj = stringr::str_squish(cnpj),
    cnpj =  stringr::str_remove_all(cnpj, "\\.|\\/|-| |[a-z]|[A-Z]"),
    # cnpj = stringr::str_split(cnpj, pattern = stringr::regex(",|;"))
    cpf = stringr::str_length(cnpj) == 11
  ) |>
  dplyr::left_join(obsFase3::aux_rfb) |>
  dplyr::mutate(
    me_epp = dplyr::case_when(
      porte_empresa == "ME" ~ TRUE,
      stringr::str_detect(nome, stringr::regex("epp$", TRUE)) ~ TRUE,
      TRUE ~ FALSE
    )
  ) |>
  dplyr::select(
    id_processo,
    info_digital,
    nome,
    polo,
    cnpj,
    cpf,
    porte_empresa,
    me_epp
  ) |>
  dplyr::mutate(
    manter = dplyr::case_when(
      # cpf ~ TRUE,
      me_epp ~ TRUE,
      # info_digital == "Sim" & is.na(nome) ~ TRUE,
      TRUE ~ FALSE
    )
  ) |>
  dplyr::filter(manter) |>
  dplyr::distinct(id_processo) |>  dplyr::pull()

da_danielly <- obsFase3::da_processo_tidy |>
  dplyr::filter(id_processo %in% pessoas_naturais) |>
  dplyr::select(
    id_processo,
    ano_dist,
    info_classe,
    info_assunto,
    info_digital,
    info_foro,
    info_conv,
    info_autofal,
    dt_decisao,
    info_fal_dec,
    info_fal_dec_fund,
    listcred_devedor_teve,
    listcred_devedor_val,
    dt_listcred_devedor,
    listcred_aj_teve,
    listcred_aj_val,
    dt_listcred_aj,
    aj_pfpj,
    info_ativo_val,
    info_104,
    info_fal_acabou,
    dt_fal_fim,
    dt_dist,
    info_origem,
    dt_extincao,
    info_aj_crime,
    dt_assinatura_tc,
    info_aj_relatorio,
    info_aj_relatorio_mp,
    info_obrig_extin,
    info_fal_extin_caucao,
    dplyr::contains("pgto")
  )

writexl::write_xlsx(da_danielly, "data-raw/nepi_2022/xlsx/da_danielly.xlsx")
