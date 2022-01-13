library(magrittr)

# id_processo <- "/mnt/dados/abj/levantamentos/aud_custodia_fabiotoledo/cjpg.rds" %>% 
#   readr::read_rds() %>% 
#   with(n_processo) %>%
#   unique()
# 
# poss_download_cpopg <- purrr::possibly(lex::tjsp_cpopg_download, NULL)
# 
# id_processo %>%
#   purrr::map(poss_download_cpopg,
#              dir = "/mnt/dados/abj/levantamentos/aud_custodia_fabiotoledo/cpopg",
#              login = "270.229.718-89", senha = "pesquisa")


future::plan("multiprocess")
fs::dir_ls("/mnt/dados/abj/levantamentos/aud_custodia_fabiotoledo/cpopg/") %>% 
  lex::tjsp_cpopg_parse() %>% 
  readr::write_rds("/mnt/dados/abj/levantamentos/aud_custodia_fabiotoledo/cpopg.rds")
