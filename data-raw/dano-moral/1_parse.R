library(magrittr)

# cjpg --------------------------------------------------------------------

# path <- "/mnt/dados/abj/levantamentos/reis/cjpg_dano_moral/"
# poss_parse <- purrr::possibly(lex::tjsp_cjpg_parse, otherwise = tibble::tibble(), quiet = FALSE)
# fs::dir_ls(path, recurse = TRUE, glob = "*.html") %>%
#   purrr::map_df(poss_parse) %>%
#   readr::write_rds("/mnt/dados/abj/levantamentos/reis/cjpg_dano_moral.rds")

# path %>% paste0(seq(2018, 2020), ".rds") %>% 
#   purrr::map_df(readr::read_rds) %>% 
#   readr::write_rds("/mnt/dados/abj/levantamentos/reis/cjpg.rds")

# 
# # cpopg -------------------------------------------------------------------
future::plan("multiprocess")
fs::dir_ls("/mnt/dados/abj/levantamentos/reis/cpopg_dano_moral/") %>%
  lex::tjsp_cpopg_parse() %>%
  dplyr::distinct() %>%
  readr::write_rds("/mnt/dados/abj/levantamentos/reis/processos_cpopg_dano_moral.rds")
# 
# # cposg -------------------------------------------------------------------
# 
# cposg <- fs::dir_ls("/mnt/dados/abj/levantamentos/reis/cposg/") %>% 
#   purrr::map_df(lex::tjsp_cposg_parse)
# 
# 
# cposg %>%   
#   dplyr::distinct() %>% 
#   readr::write_rds("/mnt/dados/abj/levantamentos/reis/processos_cposg.rds")
