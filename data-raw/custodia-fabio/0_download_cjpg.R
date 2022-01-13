library(magrittr)

esaj <- esaj::cjpg_table("subjects")

download_cjpg <- function(ano, id_ass) {
  path <- paste0(path, ano)
  lex::tjsp_cjpg_download(
    busca = "",
    dir = path,
    assunto = id_ass, 
    data_ini = paste0(ano,"-01-01"), data_fim = paste0(ano, "-12-31")
  )
}
# a) Crimes contra o patrimônio -------------------------------------------

assuntos <- "  Furto
  Furto qualificado (Art. 155, § 4° e 5°)
  Roubo
  Roubo qualificado (Art. 157, § 2°)
  Latrocínio (Art. 157, § 3°)" %>%
  stringr::str_split("\n") %>%
  unlist() %>%
  stringr::str_squish() %>%
  stringr::str_remove(stringr::regex(" \\(.+\\)")) %>%
  stringr::str_c(collapse = "|") %>%
  stringr::regex(ignore_case = TRUE)

busca_termos <- '"roubo OU furto OU latrocinio"'

id_assuntos <- esaj %>% dplyr::filter(stringr::str_detect(name5, assuntos)) %>% with(id5)
path <- "/mnt/dados/abj/levantamentos/aud_custodia_fabiotoledo/cjpg_patrimonio/"

purrr::map(seq(2016, 2019), download_cjpg, id_ass = id_assuntos)


# b) Drogas ---------------------------------------------------------------

id_assuntos <- esaj %>%
  dplyr::filter(stringr::str_detect(name5, stringr::regex("tr[aá]fico de drogas", ignore_case = TRUE))) %>%
  with(id5)
path <- "/mnt/dados/abj/levantamentos/aud_custodia_fabiotoledo/cjpg_drogas/"
purrr::map(seq(2016, 2019), download_cjpg, id_ass = id_assuntos)


# c) Crimes contra a pessoa -----------------------------------------------

id_assuntos <- esaj %>%
  dplyr::filter(stringr::str_detect(name5, stringr::regex("homic", ignore_case = TRUE))) %>%
  with(id5)

id_assuntos <- esaj %>%
  dplyr::filter(stringr::str_detect(name1, stringr::regex("corporal", ignore_case = TRUE))) %>%
  dplyr::select(id1, id2, id5) %>%
  purrr::flatten_chr() %>%
  purrr::discard(is.na) %>%
  c(id_assuntos, .)

id_assuntos <- esaj %>%
  dplyr::filter_at(
    .vars = dplyr::vars(dplyr::starts_with("name")),
    .vars_predicate = dplyr::any_vars(stringr::str_detect(
      ., stringr::regex("viol[eê]ncia dom", ignore_case = TRUE)
    ))
  ) %>%
  dplyr::filter(id0 == "287") %>%
  dplyr::select(dplyr::starts_with("id")) %>%
  purrr::flatten_chr() %>% purrr::discard(is.na) %>%
  c(id_assuntos, .)

path <- "/mnt/dados/abj/levantamentos/aud_custodia_fabiotoledo/cjpg_pessoa/"
# purrr::map(seq(2016, 2019), download_cjpg, id_ass = id_assuntos)
download_cjpg(2016, id_assuntos)


# d) Crimes contra a dignidade sexual -------------------------------------

id_assuntos <- esaj %>%
  dplyr::filter_at(.vars = dplyr::vars(dplyr::starts_with("name")),
                   .vars_predicate = dplyr::any_vars(stringr::str_detect(
                     ., stringr::regex("estupro", ignore_case = TRUE)))) %>%
  dplyr::select(id2, id5) %>% purrr::flatten_chr() %>% purrr::discard(is.na) %>% c("3463", .)

id_assuntos <- esaj %>%
  dplyr::filter_at(.vars = dplyr::vars(dplyr::starts_with("name")),
                   .vars_predicate = dplyr::any_vars(stringr::str_detect(
                     ., stringr::regex("pudor", ignore_case = TRUE)))) %>%
  dplyr::select(id2, id5) %>% purrr::flatten_chr() %>% purrr::discard(is.na) %>% c(id_assuntos, .)

id_assuntos <- esaj %>%
  dplyr::filter_at(.vars = dplyr::vars(dplyr::starts_with("name")),
                   .vars_predicate = dplyr::any_vars(stringr::str_detect(
                     ., stringr::regex("corrup[cç][aã]o de menores", ignore_case = TRUE)))) %>%
  dplyr::select(id2, id5) %>% purrr::flatten_chr() %>% purrr::discard(is.na) %>% c(id_assuntos, .)

id_assuntos <- unique(id_assuntos)
path <- "/mnt/dados/abj/levantamentos/aud_custodia_fabiotoledo/cjpg_dignidade/"
purrr::map(seq(2016, 2019), download_cjpg, id_ass = id_assuntos)


# Outros ------------------------------------------------------------------

id_assuntos <- esaj %>%
  dplyr::filter_at(
    .vars = dplyr::vars(dplyr::starts_with("name")),
    .vars_predicate = dplyr::any_vars(stringr::str_detect(
      .,
      stringr::regex("((violação|assédio) sexual|lascívia mediante|(favorecimento|casa) de prost|rufian)", 
                     ignore_case = TRUE)
    ))
  ) %>% with(id5)

path <- "/mnt/dados/abj/levantamentos/aud_custodia_fabiotoledo/cjpg_outros/"
purrr::map(seq(2016, 2019), download_cjpg, id_ass = id_assuntos)

