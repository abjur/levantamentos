tempos_negativos <- da |>
  dplyr::mutate(
    tempos_plenario_fato = as.numeric(dt_plenario - dt_fato),
    tempos_ip_fato = as.numeric(dt_ip - dt_fato),
    tempos_dist_ip = as.numeric(dt_dist - dt_ip),
    tempos_denuncia_dist = as.numeric(dt_denuncia - dt_dist),
    tempos_recebimento_denuncia = as.numeric(dt_recebimento_denuncia - dt_denuncia),
    tempos_pronuncia_recebimento = as.numeric(dt_recebimento_denuncia - dt_pronuncia)
  ) |>
  dplyr::select(id_processo, dplyr::contains("tempos")) |>
  dplyr::filter(
    tempos_plenario_fato < 0 |
      tempos_ip_fato < 0 |
      tempos_dist_ip < 0 |
      tempos_denuncia_dist < 0 |
      tempos_recebimento_denuncia < 0 |
      tempos_pronuncia_recebimento < 0
  ) |>
  dplyr::mutate(
    incos1 = dplyr::case_when(
      tempos_plenario_fato < 0 ~ "o fato aconteceu depois do plenário"
    ),
    incos2 = dplyr::case_when(
      tempos_ip_fato < 0 ~ "o fato aconteceu depois da instauração do inquérito policial"
    ),
    incos3 = dplyr::case_when(
      tempos_dist_ip < 0  ~ "a instauração do inquérito policial aconteceu depois da distribuição do processo"
    ),
    incos4 = dplyr::case_when(
      tempos_denuncia_dist < 0 ~ "a distribuição do processo aconteceu depois da denúncia"
    ),
    incos5 = dplyr::case_when(
      tempos_recebimento_denuncia < 0 ~ "o recebimento da denúncia aconteceu depois da denúncia"
    ),
    incos6 = dplyr::case_when(
      tempos_pronuncia_recebimento < 0 ~ "o recebimento da pronúncia aconteceu depois da sentença de impronúncia, , desclassificação na primeira fase, absolvição sumária ou extinção da punibilidade"
    )
  ) |>
  dplyr::select(id_processo, dplyr::contains("incos")) |>
  tidyr::unite(
    incos, dplyr::contains("incos"), na.rm = TRUE, sep = "; "
  ) |>
  dplyr::mutate(
    n_incos = dplyr::case_when(
      stringr::str_detect(incos, ";") ~ 2,
      TRUE ~ 1
    )
  ) |>
  dplyr::arrange(desc(n_incos)) |>
  dplyr::rename(
    `número do processo` = id_processo,
    `descrição das inconsistências` = incos,
    `quantidade de inconsistências` = n_incos
  )

fs::dir_create("data-raw/bruno-nassar-puc/data-raw/xlsx")
writexl::write_xlsx(tempos_negativos, "data-raw/bruno-nassar-puc/data-raw/xlsx/tempos_negativos.xlsx")
