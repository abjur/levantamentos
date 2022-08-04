library(magrittr)

readr::read_rds("/mnt/dados/abj/levantamentos/reis/processos_cjpg.rds") %>% 
  dplyr::select(n_processo, comarca, assunto, vara, magistrado, resumo) %>% 
  dplyr::mutate(resumo = stringr::str_trunc(resumo, 32767)) %>% 
  writexl::write_xlsx("/mnt/dados/abj/levantamentos/reis/dados_basicos.xlsx")

readr::read_rds("/mnt/dados/abj/levantamentos/reis/processos_cpopg.rds") %>% 
  dplyr::mutate(n_processo = abjutils::clean_cnj(id_processo)) %>% 
  dplyr::select(n_processo, partes) %>% 
  tidyr::unnest(partes) %>% 
  writexl::write_xlsx("/mnt/dados/abj/levantamentos/reis/partes.xlsx")

readr::read_rds("/mnt/dados/abj/levantamentos/reis/processos_cpopg.rds") %>% 
  dplyr::mutate(n_processo = abjutils::clean_cnj(id_processo)) %>% 
  dplyr::select(n_processo, movimentacoes) %>% 
  tidyr::unnest(movimentacoes) %>% 
  dplyr::mutate(descricao = stringr::str_trunc(descricao, 32756)) %>%
  writexl::write_xlsx("/mnt/dados/abj/levantamentos/reis/movimentacoes.xlsx")

cposg <- readr::read_rds("/mnt/dados/abj/levantamentos/reis/processos_cposg.rds")
cposg %>% 
  dplyr::mutate(n_processo = abjutils::clean_cnj(id_processo)) %>% 
  dplyr::select(n_processo, movimentacoes) %>% 
  tidyr::unnest(movimentacoes) %>% 
  writexl::write_xlsx("/mnt/dados/abj/levantamentos/reis/movimentacoes_2.xlsx")
