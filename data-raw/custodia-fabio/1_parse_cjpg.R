library(magrittr)

parse_cjpg <- function(path) {
  fs::dir_ls(path, recurse = TRUE, glob = "*.html") %>%
    lex::tjsp_cjpg_parse() %>%
    readr::write_rds(paste0(path, ".rds"))
}

# parse_cjpg("/mnt/dados/abj/levantamentos/aud_custodia_fabiotoledo/cjpg_outros")
parse_cjpg("/mnt/dados/abj/levantamentos/aud_custodia_fabiotoledo/cjpg_dignidade")
parse_cjpg("/mnt/dados/abj/levantamentos/aud_custodia_fabiotoledo/cjpg_drogas")
parse_cjpg("/mnt/dados/abj/levantamentos/aud_custodia_fabiotoledo/cjpg_pessoa")
parse_cjpg("/mnt/dados/abj/levantamentos/aud_custodia_fabiotoledo/cjpg_patrimonio")


fs::dir_ls("/mnt/dados/abj/levantamentos/aud_custodia_fabiotoledo/", regexp = "cjpg_.+\\.rds") %>% 
  purrr::map_df(readr::read_rds) %>% 
  dplyr::distinct() %>% 
  readr::write_rds("cjpg.rds")
