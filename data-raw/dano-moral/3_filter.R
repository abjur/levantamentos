library(magrittr)

cjpg <- readr::read_rds("/mnt/dados/abj/levantamentos/reis/cjpg_novo.rds")
id_baixados <- cjpg %>% 
  with(n_processo) %>%
  abjutils::clean_cnj() %>%
  stringr::str_pad(20, "left", 0) %>%
  unique()


  