library(magrittr)

# da <- read_rds("/mnt/dados/abj/levantamentos/aud_custodia_fabiotoledo/amostra.rds")
# 
# da %>%
#   select(id_processo, movimentacoes) %>%
#   unnest(movimentacoes) %>%
#   filter(str_detect(movimento, regex("audi[eê]ncia.+cust[oó]dia", ignore_case = TRUE))) %>%
#   select(id_processo, descricao)


cpopg <- readr::read_rds("/mnt/dados/abj/levantamentos/aud_custodia_fabiotoledo/cpopg.rds")
  
ids_custodia <- cpopg %>% 
  tidyr::unnest(movimentacoes) %>%
  dplyr::filter(stringr::str_detect(
    movimento,
    stringr::regex("audi[eê]ncia.+cust[oó]dia", ignore_case = TRUE)
  )) %>% 
  with(id_processo) %>% 
  unique()

cpopg %>% 
  dplyr::filter(id_processo %in% ids_custodia) %>% 
  dplyr::mutate(n_processo = abjutils::clean_cnj(id_processo)) %>% 
  dplyr::inner_join(readr::read_rds("/mnt/dados/abj/levantamentos/aud_custodia_fabiotoledo/cjpg.rds"),
                    "n_processo") %>%
  readr::write_rds("/mnt/dados/abj/levantamentos/aud_custodia_fabiotoledo/aud_custodia.rds")


aud_custodia <- readr::read_rds("/mnt/dados/abj/levantamentos/aud_custodia_fabiotoledo/aud_custodia.rds")
dplyr::glimpse(aud_custodia)

aud_custodia %>% 
  dplyr::select_if(purrr::negate(is.list)) %>%
  dplyr::select(-c(arq, n_processo, assunto.y, classe.y)) %>% 
  dplyr::rename(assunto = assunto.x, classe = classe.x) %>% 
  dplyr::mutate(resumo = stringr::str_trunc(resumo, 32767)) %>% 
  writexl::write_xlsx("levantamentos/aud_custodia_fabiotoledo/dados_basicos.xlsx")

aud_custodia %>% 
  dplyr::select(id_processo, movimentacoes) %>%
  tidyr::unnest(movimentacoes) %>% 
  dplyr::filter(stringr::str_detect(movimento, stringr::regex("(deci|procedente)", ignore_case = TRUE))) %>% 
  dplyr::mutate(descricao = stringr::str_trunc(descricao, 32767)) %>% 
  writexl::write_xlsx("levantamentos/aud_custodia_fabiotoledo/movimentacoes.xlsx")
