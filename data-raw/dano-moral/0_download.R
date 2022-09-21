library(magrittr)

assuntos <- c(
  "Contratos Bancários",
  "Indenização por Dano Moral",
  "Cláusulas Abusivas",
  "Rescisão do contrato e devolução do dinheiro",
  "Interpretação / Revisão de Contrato",
  "Inclusão Indevida em Cadastro de Inadimplentes",
  "Protesto Indevido de Título",
  "Indenização por Dano Material",
  "Vendas casadas",
  "Dever de Informação",
  "Práticas Abusivas",
  "Empréstimo consignado",
  "Tarifas",
  "Expurgos Inflacionários / Planos Econômicos",
  "Cobrança de tarifas administrativas para concessão e cobrança de créditos",
  "Cartão de Crédito",
  "Financiamento de Produto",
  "Capitalização e Previdência Privada",
  "Consórcio",
  "Seguro",
  "Reajuste contratual"
)

# esaj <- esaj::cjpg_table("subjects")
# id_assunto <- esaj %>%
#   dplyr::filter(name5 %in% assuntos) %>%
#   dplyr::pull(id5)

# "Indenização por Dano Moral"
# id_assunto <- esaj %>%
#   dplyr::filter_at(
#     .vars = dplyr::vars(dplyr::starts_with("name")),
#     .vars_predicate = dplyr::any_vars(stringr::str_detect(
#       .,
#       stringr::regex("Indenização por Dano Moral",
#                      ignore_case = TRUE)
#     ))
#   ) %>% dplyr::transmute(id = dplyr::coalesce(id2, id5)) %>% with(id)

# cjpg --------------------------------------------------------------------

# path <- "/mnt/dados/abj/levantamentos/reis/cjpg_dano_moral/"
# download_cjpg <- function(ano) {
#   path <- paste0(path, ano)
#   lex::tjsp_cjpg_download(
#     busca = "",
#     dir = path,
#     data_ini = paste0(ano, "-01-01"), data_fim = paste0(ano, "-12-31"),
#     assunto = id_assunto)
# }
# 
# poss_download_cjpg <- purrr::possibly(download_cjpg, NULL)
# purrr::map(seq(2018, 2020), poss_download_cjpg)


# cjsg --------------------------------------------------------------------

assuntos <- assuntos %>% 
  paste0(collapse = "|") %>% 
  stringr::regex(ignore_case = TRUE)

esaj <- esaj::cjsg_table("subjects")

id_assunto <- esaj %>%
  dplyr::filter_at(
    .vars = dplyr::vars(dplyr::starts_with("name")),
    .vars_predicate = dplyr::any_vars(stringr::str_detect(., assuntos))
  ) %>%
  dplyr::select(id2, id3, id5) %>% 
  purrr::flatten() %>% 
  unlist() %>% purrr::discard(is.na) %>% purrr::discard(.=="9674")

path <- "/mnt/dados/abj/levantamentos/reis/cjsg/"
download_cjsg <- function(ano) {
  path <- paste0(path, ano)
  lex::tjsp_cjsg_download(
    busca = "",
    dir = path,
    julgamento_ini = paste0(ano, "-01-01"), julgamento_fim = paste0(ano, "-12-31"),
    assunto = id_assunto)
}

poss_download_cjsg <- purrr::possibly(download_cjsg, NULL)
purrr::map(seq(2018, 2020), poss_download_cjsg)



# cpopg -------------------------------------------------------------------

# id_processos <- "/mnt/dados/abj/levantamentos/reis/cjpg_dano_moral.rds" %>%
#   readr::read_rds() %>%
#   dplyr::pull(n_processo) %>%
#   abjutils::clean_cnj() %>%
#   stringr::str_pad(20, "left", 0) %>%
#   unique()
# 
# fs::dir_create("/mnt/dados/abj/levantamentos/reis/cpopg_dano_moral")
# lex::tjsp_cpopg_download(id_processos, "/mnt/dados/abj/levantamentos/reis/cpopg_dano_moral",
#                          login = "270.229.718-89", senha = "pesquisa")

# # cposg -------------------------------------------------------------------
# 
# id_processos <- "/mnt/dados/abj/levantamentos/reis/processos_cjpg.rds" %>% 
#   readr::read_rds() %>%
#   dplyr::pull(n_processo) %>% 
#   abjutils::clean_cnj() %>% 
#   stringr::str_pad(20, "left", 0) %>% 
#   unique()
# 
# fs::dir_create("/mnt/dados/abj/levantamentos/reis/cposg")
# lex::tjsp_cposg_download(id_processos, "/mnt/dados/abj/levantamentos/reis/cposg",
#                          login = "270.229.718-89", senha = "pesquisa")
