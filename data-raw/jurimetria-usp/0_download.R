library(magrittr)

assuntos_esaj <- esaj::cjpg_table("subjects")
assuntos <- assuntos_esaj %>%
  dplyr::filter(stringr::str_detect(name5, stringr::regex("contra a mulher", ignore_case = TRUE))) %>%
  dplyr::pull(id5)

# cjpg --------------------------------------------------------------------

download_cjpg <- function(assunto) {
  path <- paste0("/mnt/dados/abj/levantamentos/jurimetria_usp/cjpg_novo/id", assunto)
  fs::dir_create(path)
  lex::tjsp_cjpg_download(
    busca = "",
    dir = path,
    data_ini = "01/01/2011", data_fim = "31/12/2015", pagina_fim = Inf,
    assunto = assunto)
}

poss_download_cjpg <- purrr::possibly(download_cjpg, otherwise = NULL)
purrr::map(assuntos, poss_download_cjpg)

# download_cjpg_key <- function() {
#   path <- paste0("/mnt/dados/abj/levantamentos/jurimetria_usp/cjpg/por_palavra_chave")
#   fs::dir_create(path)
#   lex::tjsp_cjpg_download(
#     busca = '"Lei Maria da Penha" OU "Lei 11.340" OU "Violência Doméstica" OU "Lesão corporal"' ,
#     dir = path,
#     data_ini = "01/01/2011", data_fim = "31/12/2015",
#   )
# }
# 
# 
# poss_download_cjpg_key <- purrr::possibly(download_cjpg_key, otherwise = "erro")
# future::plan("multiprocess")
# furrr::future_map(assuntos, poss_download_cjpg_key)
# 
# 
# # cpopg -------------------------------------------------------------------
# 
# id_processos <- "/mnt/dados/abj/levantamentos/jurimetria_usp/cjpg.rds" %>%
#   readr::read_rds() %>%
#   dplyr::pull(n_processo) %>%
#   abjutils::clean_cnj() %>%
#   stringr::str_pad(20, "left", 0) %>%
#   unique()
# 
# download_cpopg <- function(id) {
#   lex::tjsp_cpopg_download(
#     id, "/mnt/dados/abj/levantamentos/jurimetria_usp/cpopg",
#     login = "270.229.718-89", senha = "pesquisa")
# }
# 
# fs::dir_create("/mnt/dados/abj/levantamentos/jurimetria_usp/cpopg")
# poss_download_cpopg <- purrr::possibly(download_cpopg, otherwise = "erro")
# 
# purrr::map(id_processos, poss_download_cpopg)
# 


# cjsg --------------------------------------------------------------------

# reg_assunto <- stringr::str_c(
#   "viol[eê]ncia dom[eé]stica",
#   "les[aã]o corporal",
#   "amea[cç]a",
#   "contra a mulher",
#   "homic[ií]dio qualificado",
#   "inj[uú]ria",
#   sep = "|"
# ) %>%
#   stringr::regex(ignore_case = TRUE)
# 
# assuntos_esaj <- esaj::cjsg_table("subjects")
# assuntos <- assuntos_esaj %>%
#   dplyr::filter(stringr::str_detect(name5, reg_assunto)) %>%
#   dplyr::pull(id5) %>%
#   unique()
# 
# path <- "/mnt/dados/abj/levantamentos/jurimetria_usp/cjsg/por_assunto"
# fs::dir_create(path)
# 
# download_cjsg <- function(assunto) {
#   lex::tjsp_cjsg_download(
#     busca = "",
#     dir = paste0("/mnt/dados/abj/levantamentos/jurimetria_usp/cjsg/por_assunto/id", assunto),
#     registro_ini = "01/01/2011", registro_fim = "31/12/2015",
#     assunto = assunto,
#     # pagina_ini = 60,
#     pagina_fim = Inf)
# }
# poss_download_cjsg <- purrr::possibly(download_cjsg, otherwise = NULL)
# purrr::map(assuntos, poss_download_cjsg)
# 


# download_cjsg_key <- function(ano) {
#   path <- paste0("/mnt/dados/abj/levantamentos/jurimetria_usp/cjsg/por_palavra_chave/", ano)
#   fs::dir_create(path)
#   lex::tjsp_cjsg_download(
#     '"Lei Maria da Penha" OU "Lei 11.340" OU "Violência Doméstica" OU "Lesão corporal"',
#     julgamento_ini = paste0("01/01/", ano), julgamento_fim = paste0("31/12/", ano),
#     dir = path,
#     pagina_fim = Inf)
# }

# purrr::map(seq(2011, 2015), download_cjsg_key)

# cposg -------------------------------------------------------------------
# 
# fs::dir_create("/mnt/dados/abj/levantamentos/jurimetria_usp/cposg")
# 
# download_cposg <- function(id) {
#   lex::tjsp_cposg_download(
#     id,
#     "/mnt/dados/abj/levantamentos/jurimetria_usp/cposg",
#     login = "270.229.718-89",
#     senha = "pesquisa"
#   )
# }
# 
# poss_download_cposg <- purrr::possibly(download_cposg, NULL)
# 
# "/mnt/dados/abj/levantamentos/jurimetria_usp/cjsg_assunto.rds" %>%
#   readr::read_rds() %>%
#   dplyr::pull(n_processo) %>%
#   abjutils::clean_cnj() %>%
#   stringr::str_pad(20, "left", 0) %>%
#   unique() %>%
#   purrr::map(poss_download_cposg)
