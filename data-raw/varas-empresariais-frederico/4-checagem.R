# checagem do relatório que o Fred mandou
library(magrittr)

processos <- "data-raw/varas-empresariais-frederico/da_completa.rds" %>%
  readr::read_rds() %>%
  dplyr::mutate(
    id_processo = abjutils::clean_cnj(id_processo),
    distribuicao = dplyr::coalesce(distribuicao, recebido_em),
    distribuicao = stringr::str_extract(distribuicao, "[0-9]{2}/[0-9]{2}/[0-9]{4}"),
    distribuicao = lubridate::dmy(distribuicao),
    ano_dist = dplyr::case_when(
      !is.na(distribuicao) ~ lubridate::year(distribuicao),
      is.na(distribuicao) ~ as.numeric(stringr::str_extract(
        id_processo, "(?<=[0-9]{9})[0-9]{4}"
      ))
    )
  )

processos_filtrados <- processos %>%
  dplyr::filter(distribuicao < "2020-03-01" | ano_dist < 2020)

nrow(processos_filtrados)
# 4239 casos ao todo


# criando tabela de classe processual -------------------------------------
classe <- processos_filtrados |>
  dplyr::count(classe) |>
  dplyr::arrange(desc(n))

writexl::write_xlsx(classe, "data-raw/varas-empresariais-frederico/xlsx/classes.xlsx")

# filtrando cumprimento de sentença ---------------------------------------
# no relatório 334
processos_filtrados |>
  dplyr::filter(stringr::str_detect(classe, stringr::regex("Cumprimento|Liquida", TRUE))) |>
  dplyr::count()
# 334 também

# filtrando processos apensados -------------------------------------------
# no relatório estão 4036 processos principais
# o que leva a 203 processos apensados
processos_filtrados |>
  dplyr::filter(!is.na(processo_principal)) |>
  dplyr::count()

# filtrando cumprimento de sentença e apensados ---------------------------
processos_filtrados |>
  dplyr::filter(
    !stringr::str_detect(classe, stringr::regex("cumprimento|liquida", TRUE)),
    is.na(processo_principal)
  ) |>
  dplyr::count()
# 3890

