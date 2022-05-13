library(magrittr)


# cjpg --------------------------------------------------------------------

assuntos <- c("4656", "4660", "4670", "4680", "10499") %>%
  paste0("id5", .)

parse_cjpg <- function(assunto) {
  path <-
    paste0("/mnt/dados/abj/levantamentos/prop_industrial/cjpg/",
           assunto)
  fs::dir_ls(path) %>%
    lex::tjsp_cjpg_parse() %>%
    dplyr::distinct() %>%
    readr::write_rds(paste0(
      "/mnt/dados/abj/levantamentos/prop_industrial/cjpg/",
      assunto,
      ".rds"
    ))
}

poss_parse <- purrr::possibly(lex::tjsp_cjpg_parse, "erro")
fs::dir_ls("/mnt/dados/abj/levantamentos/prop_industrial/cjpg2/") %>% 
  lex::tjsp_cjpg_parse() %>% 
  readr::write_rds("/mnt/dados/abj/levantamentos/prop_industrial/cjpg2.rds")

purrr::map(assuntos, parse_cjpg)

fs::dir_ls("/mnt/dados/abj/levantamentos/prop_industrial/cjpg/", glob = "*.rds") %>%
  purrr::map_df(readr::read_rds) %>%
  dplyr::distinct() %>%
  readr::write_rds("/mnt/dados/abj/levantamentos/prop_industrial/cjpg.rds")

# cpopg -------------------------------------------------------------------

future::plan("multiprocess")
"/mnt/dados/abj/levantamentos/prop_industrial/cpopg2" %>% 
  fs::dir_ls() %>% 
  lex::tjsp_cpopg_parse() %>% 
  readr::write_rds("/mnt/dados/abj/levantamentos/prop_industrial/cpopg2.rds")
