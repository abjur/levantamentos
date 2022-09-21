library(magrittr)

cjpg_parsed_clean <- readr::read_rds("cjpg_parsed_clean.rds")

termos_raw <- c(
  "PRESCRIÇÃO DA PRETENSÃO PUNITIVA EM ABSTRATO",
  "PRESCRIÇÃO INTERCORRENTE",
  "PRESCRIÇÃO RETROATIVA",
  "PRESCRIÇÃO DA PRETENSÃO PUNITIVA ANTECIPADA",
  "PRESCRIÇÃO DA PRETENSÃO PUNITIVA PROJETADA",
  "PRESCRIÇÃO DA PRETENSÃO PUNITIVA VIRTUAL",
  "PRESCRIÇÃO DA PRETENSÃO PUNITIVA RETROATIVA EM PERSPECTIVA",
  "DECLARO EXTINTA A PUNIBILIDADE PELA PRESCRIÇÃO DA PRETENSÃO PUNITIVA NA MODALIDADE VIRTUAL"
)

rx_prescricao <- stringr::regex("prescr", TRUE)

rx_tipo <- c(
  "pretens[aã]o punitiva",
  "retroativ",
  "intercorrente"
) %>%
  stringr::str_c(collapse = "|") %>%
  stringr::regex(TRUE)

rx_subtipo <- c(
  "antecipada",
  "projetada",
  "virtual",
  "perspectiva"
) %>%
  stringr::str_c(collapse = "|") %>%
  stringr::regex(TRUE)

aux_prescricao <- cjpg_parsed_clean %>%
  dplyr::filter(stringr::str_detect(resumo, rx_prescricao))

aux_classificado <- aux_prescricao %>%
  dplyr::select(-n_cases) %>%
  dplyr::mutate(
    tipo = stringr::str_extract(resumo, rx_tipo),
    tipo = tolower(abjutils::rm_accent(tipo)),
    subtipo = dplyr::case_when(
      tipo == "pretensao punitiva" ~ stringr::str_extract(resumo, rx_subtipo),
      TRUE ~ NA_character_
    ),
    subtipo = tolower(subtipo)
  )

aux_classificado %>%
  dplyr::count(tipo, subtipo)

da_final <- aux_classificado %>%
  dplyr::filter(!is.na(tipo)) %>%
  dplyr::filter(tipo != "pretensao punitiva" | !is.na(subtipo)) %>%
  dplyr::relocate(tipo, subtipo, .after = codigo) %>%
  dplyr::mutate(resumo = stringr::str_trunc(resumo, 32000))

writexl::write_xlsx(
  da_final,
  "/mnt/dados/abj/levantamentos/prescricao_penal/20210901_prescricao_penal.xlsx"
)





