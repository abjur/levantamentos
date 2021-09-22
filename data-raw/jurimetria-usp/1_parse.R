library(magrittr)


# cjpg --------------------------------------------------------------------

# assunto -----------------------------------------------------------------

poss_parse <- purrr::possibly(lex::tjsp_cjpg_parse, tibble::tibble())

fs::dir_ls("/mnt/dados/abj/levantamentos/jurimetria_usp/cjpg_novo", recurse = TRUE) %>%
  purrr::map_df(poss_parse) %>%
  dplyr::distinct() %>%
  readr::write_rds("/mnt/dados/abj/levantamentos/jurimetria_usp/cjpg_assunto.rds")

# # palavra-chave -----------------------------------------------------------------
# 
# fs::dir_ls("/mnt/dados/abj/levantamentos/jurimetria_usp/cjpg/por_palavra_chave/") %>%
#   lex::tjsp_cjpg_parse() %>%
#   dplyr::distinct() %>%
#   readr::write_rds("/mnt/dados/abj/levantamentos/jurimetria_usp/cjpg_palavra_chave.rds")
# 
# 
# readr::read_rds("/mnt/dados/abj/levantamentos/jurimetria_usp/cjpg_assunto.rds") %>%
#   dplyr::bind_rows(
#     readr::read_rds("/mnt/dados/abj/levantamentos/jurimetria_usp/cjpg_palavra_chave.rds")
#   ) %>%
#   dplyr::distinct() %>%
#   readr::write_rds("/mnt/dados/abj/levantamentos/jurimetria_usp/cjpg.rds")
# 
# 
# # cpopg -------------------------------------------------------------------
# 
# fs::dir_ls("/mnt/dados/abj/levantamentos/jurimetria_usp/cpopg/") %>%
#   lex::tjsp_cpopg_parse() %>%
#   dplyr::distinct() %>%
#   readr::write_rds("/mnt/dados/abj/levantamentos/jurimetria_usp/cpopg.rds")
# 
# 
# 
# # cpopg -------------------------------------------------------------------
# future::plan("multiprocess")
# fs::dir_ls("/mnt/dados/abj/levantamentos/jurimetria_usp/cpopg/") %>%
#   lex::tjsp_cpopg_parse() %>%
#   readr::write_rds("/mnt/dados/abj/levantamentos/jurimetria_usp/cpopg.rds")


# cjsg --------------------------------------------------------------------

# assunto -----------------------------------------------------------------

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
# parse_cjsg <- function(assunto) {
#   path <- paste0("/mnt/dados/abj/levantamentos/jurimetria_usp/cjsg/por_assunto/id", assunto)
#   proc <- tibble::tibble(
#     path = fs::dir_ls(path)
#   )
#   proc %>%
#     dplyr::mutate(
#       size = fs::file_size(path)
#     ) %>%
#     dplyr::filter(size > 0) %>%
#     with(path) %>%
#     purrr::map_df(lex::tjsp_cjsg_parse) %>%
#     readr::write_rds(paste0("/mnt/dados/abj/levantamentos/jurimetria_usp/cjsg_assunto_", assunto, ".rds"))
# }
# 
# purrr::map(assuntos, parse_cjsg)
# 
# 
# "/mnt/dados/abj/levantamentos/jurimetria_usp/cjsg_assunto_" %>%
#   paste0(seq(2011, 2015), ".rds") %>%
#   purrr::map(readr::read_rds) %>%
#   dplyr::bind_rows() %>%
#   dplyr::distinct() %>%
#   readr::write_rds("/mnt/dados/abj/levantamentos/jurimetria_usp/cjsg_assunto.rds")

# palavra-chave -----------------------------------------------------------

# parse_cjsg_key <- function(ano) {
#   path <- paste0("/mnt/dados/abj/levantamentos/jurimetria_usp/cjsg/por_palavra_chave/", ano)
#   proc <- tibble::tibble(
#     path = fs::dir_ls(path)
#   )
#   proc %>%
#     dplyr::mutate(
#       size = fs::file_size(path)
#     ) %>%
#     dplyr::filter(size > 0) %>%
#     with(path) %>%
#     purrr::map_df(lex::tjsp_cjsg_parse) %>%
#     readr::write_rds(paste0("/mnt/dados/abj/levantamentos/jurimetria_usp/cjsg_key_"), ano, ".rds")
# }
# 
# parse_cjsg_key("2011")
# parse_cjsg_key("2012")
# parse_cjsg_key("2013")
# parse_cjsg_key("2014")
# parse_cjsg_key("2015")
# 
# 
# "/mnt/dados/abj/levantamentos/jurimetria_usp/cjsg_key_" %>%
#   paste0(seq(2011, 2015), ".rds") %>%
#   purrr::map(readr::read_rds) %>%
#   dplyr::bind_rows() %>%
#   dplyr::distinct() %>%
#   readr::write_rds("/mnt/dados/abj/levantamentos/jurimetria_usp/cjsg_key.rds")
# 
# 
# # cposg -------------------------------------------------------------------

# 
# poss_cposg_parse <- purrr::possibly(lex::tjsp_cposg_parse, tibble::tibble())
# 
# fs::dir_ls("/mnt/dados/abj/levantamentos/jurimetria_usp/cposg/") %>%
#   purrr::map_df(poss_cposg_parse) %>%
#   dplyr::distinct() %>%
#   readr::write_rds("/mnt/dados/abj/levantamentos/jurimetria_usp/cposg.rds")