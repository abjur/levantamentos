library(magrittr)
# Importação ------------------------------------------------------------------------------------------------------------------------------------

franquias_bases_raw <- readxl::read_excel("~/Documents/levantamentos/data-raw/luiz-estevam/bases/IC Franquias - 2021 - Luiz (120 casos) - com correção.xlsx")


# Manipulação -----------------------------------------------------------------------------------------------------------------------------------

franquias_base <- franquias_bases_raw %>%
  janitor::clean_names() %>%
  dplyr::filter(segmento_da_empresa != "fora do escopo", segmento_da_empresa != "sigilo" ) %>%
  dplyr::mutate(dplyr::across(c(data_do_recurso_peticao, data_da_distribuicao), lubridate::as_date)) %>%
  dplyr::mutate(dplyr::across(c(data_julgamento, data_publicacao), lubridate::dmy)) %>%
  dplyr::glimpse()

readr::write_rds(franquias_base, "~/Documents/levantamentos/data-raw/luiz-estevam/bases/franquias_base.rds")
