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
  dplyr::filter(
    info_fal == "Sim"
    # info_fal_dec_min == "Sim" # esperar resposta do email
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
    cpf = stringr::str_length(cnpj) == 11
  ) |>
  dplyr::left_join(obsFase3::aux_rfb) |>
  dplyr::mutate(
    me_epp = dplyr::case_when(
      porte_empresa == "ME" ~ TRUE,
      stringr::str_detect(nome, stringr::regex("epp$", TRUE)) ~ TRUE,
      TRUE ~ FALSE
    ),
    manter = dplyr::case_when(
      cpf ~ TRUE,
      me_epp ~ TRUE,
      # info_digital == "Sim" & is.na(nome) ~ TRUE,
      TRUE ~ FALSE
    )
  ) |>
  dplyr::filter(manter) |>
  dplyr::distinct(id_processo) |>
  dplyr::pull()

litis <- partes |>
  dplyr::filter(!is.na(forma_participacao)) |>
  dplyr::group_by(id_processo) |>
  dplyr::count(forma_participacao) |>
  dplyr::ungroup() |>
  dplyr::filter(
    forma_participacao %in% c("Devedor", "Autofalência"),
    n > 1
  ) |>
  dplyr::pull(id_processo)

da_danielly <- obsFase3::da_processo_tidy |>
  dplyr::filter(id_processo %in% pessoas_naturais) |>
  dplyr::mutate(
    litis = ifelse(id_processo %in% litis, "Sim", "Não")
  ) |>
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
    dplyr::contains("pgto"),
    litis
  )

writexl::write_xlsx(da_danielly, "data-raw/nepi_2022/xlsx/da_danielly.xlsx")

# pedido novo -------------------------------------------------------------


da_partes <- obsFase3::da_processo_tidy |>
  dplyr::select(id_processo, planilha_partes) |>
  dplyr::filter(purrr::map_int(planilha_partes, nrow) > 0) |>
  dplyr::mutate(planilha_partes = purrr::map(
    planilha_partes, dplyr::mutate_all, as.character
  )) |>
  tidyr::unnest(planilha_partes) |>
  dplyr::filter(!is.na(nome)) |>
  dplyr::transmute(
    id_processo,
    nome,
    forma_participacao = dplyr::coalesce(forma_participacao, ...5),
    polo,
    cnpj,
    convolou = no_caso_de_litisconsorcio_convolou_em_falencia,
    consolidacao_substancial = `houve_consolidacao_ substancial`
  )

cnpjs_individuais <- obsFase3::aux_rfb |>
  dplyr::filter(nm_subclass_natureza_juridica == "Empresa Individual de Responsabilidade Limitada (de Natureza Simples)") |>
  dplyr::pull(cnpj)

processos_empresarios_individuais <- da_partes |>
  dplyr::mutate(
    cnpj = stringr::str_remove_all(cnpj, "[:punct:]")
  ) |>
  dplyr::filter(cnpj %in% cnpjs_individuais) |>
  dplyr::distinct(id_processo) |>
  dplyr::pull(id_processo)

da_individual09 <- obsFase3::da_processo_tidy |>
  dplyr::filter(id_processo %in% processos_empresarios_individuais) |>
  dplyr::mutate(
    litis = ifelse(id_processo %in% litis, "Sim", "Não")
  ) |>
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
    dplyr::contains("pgto"),
    litis
  )

writexl::write_xlsx(da_individual09, "data-raw/nepi_2022/xlsx/da_individual09.xlsx")
