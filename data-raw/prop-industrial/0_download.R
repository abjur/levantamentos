library(magrittr)

assuntos <- c(
  "Propriedade Intelectual / Industrial",
  "Direito Autoral",
  "Patente",
  "Marca",
  "Desenho Industrial",
  "Programa de Computador"
)

assuntos_esaj <- esaj::cjpg_table("subjects")
assuntos <- assuntos_esaj %>%
  dplyr::filter(name3 == "Propriedade Intelectual / Industrial") %>%
  dplyr::pull(id3) %>% unique()

# cjpg --------------------------------------------------------------------

fs::dir_create("/mnt/dados/abj/levantamentos/prop_industrial/cjpg2")

download_cjpg <- function(assunto) {
  lex::tjsp_cjpg_download(
    busca = "",
    dir = "/mnt/dados/abj/levantamentos/prop_industrial/cjpg2",
    data_ini = "01/01/2015", data_fim = "16/09/2020",
    assunto = assunto,
    pagina_fim = Inf)
}

purrr::map(assuntos, download_cjpg)

# cpopg -------------------------------------------------------------------

fs::dir_create("/mnt/dados/abj/levantamentos/prop_industrial/cpopg2")

download_cpopg <- function(id) {
  lex::tjsp_cpopg_download(
    id, "/mnt/dados/abj/levantamentos/prop_industrial/cpopg2",
    login = "270.229.718-89", senha = "pesquisa")
}
poss_download_cpopg <- purrr::possibly(download_cpopg, otherwise = "erro")

"/mnt/dados/abj/levantamentos/prop_industrial/cjpg2.rds" %>%
  readr::read_rds() %>%
  dplyr::pull(n_processo) %>%
  abjutils::clean_cnj() %>%
  stringr::str_pad(20, "left", 0) %>%
  unique() %>%
  purrr::map(poss_download_cpopg)

