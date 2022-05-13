library(magrittr)

cjpg <- readr::read_rds("/mnt/dados/abj/levantamentos/prop_industrial/cjpg.rds") %>% 
  dplyr::bind_rows(readr::read_rds("/mnt/dados/abj/levantamentos/prop_industrial/cjpg2.rds"))
cpopg <- readr::read_rds("/mnt/dados/abj/levantamentos/prop_industrial/cpopg.rds") %>% 
  dplyr::bind_rows(readr::read_rds("/mnt/dados/abj/levantamentos/prop_industrial/cpopg2.rds"))


cjpg %>% dplyr::count(assunto, sort = TRUE)
cpopg %>% dplyr::count(assunto, sort = TRUE)

# n_processo, com comarca, classe, assunto, juiz, valor, polo ativo, polo passivo, 
# data de distribuição, data da decisão

cpopg <- cpopg %>%
  dplyr::filter(!is.na(distribuicao)) %>% 
  dplyr::transmute(
    n_processo = abjutils::clean_cnj(id_processo) %>% stringr::str_pad(20, "left", 0),
    status,
    area,
    data_distribuicao = stringr::str_extract(distribuicao, "[0-9]{2}/[0-9]{2}/[0-9]{4}") %>%
      lubridate::dmy(),
    distribuicao,
    juiz,
    outros_assuntos,
    valor_da_acao,
    partes
  ) 

# última decisão
cjpg <- cjpg %>% 
  dplyr::mutate(data_de_disponibilizacao = lubridate::dmy(data_de_disponibilizacao)) %>% 
  dplyr::group_by(n_processo) %>% 
  dplyr::filter(data_de_disponibilizacao == max(data_de_disponibilizacao)) %>% 
  dplyr::distinct(n_processo, .keep_all = TRUE) %>% 
  dplyr::transmute(
    n_processo, assunto, classe, comarca, data_sentenca = data_de_disponibilizacao, 
    foro, magistrado, vara
  )

cpopg %>% 
  dplyr::inner_join(cjpg, "n_processo") %>% 
  readr::write_rds("/mnt/dados/abj/levantamentos/prop_industrial/dados_pg.rds")
  


# Separa em polo ativo e polo passivo -------------------------------------

dados_pg <- readr::read_rds("/mnt/dados/abj/levantamentos/prop_industrial/dados_pg.rds")
partes <- dados_pg %>% dplyr::select(n_processo, partes) %>% 
  tidyr::unnest(partes)

r_auxiliar <- stringr::str_c("Ter", "Perito", "Assistente", "Interes", sep = "|")
r_ativo <- stringr::str_c("qte", "autor", "tte", "reclamante", sep = "|") %>% stringr::str_c("$") %>% 
  stringr::regex(ignore_case = TRUE)
r_passivo <- stringr::str_c(
  "(",
  stringr::str_c("qd[oa]", "ctd[oa]", "(^r[eé](u?))", "ptd[oa]", "avd[oa]", "reclamad[oa]",
                 "denunciad[oa]", "litispas", sep = "|"),
  ")$") %>% 
  stringr::regex(ignore_case = TRUE)

partes <- partes %>% 
  dplyr::mutate(polo = dplyr::case_when(
    stringr::str_detect(parte, r_ativo) | stringr::str_detect(papel, r_ativo) ~ "Ativo",
    stringr::str_detect(parte, r_passivo) | stringr::str_detect(papel, r_passivo) ~ "Passivo",
    stringr::str_detect(parte, r_auxiliar) | stringr::str_detect(papel, r_auxiliar) ~ "Auxiliar",
    TRUE ~ NA_character_
  )) %>% 
  dplyr::filter(!stringr::str_detect(parte, "Advog") & !stringr::str_detect(papel, "Advog")) %>% 
  dplyr::filter(!is.na(polo), polo != "Auxiliar") 
  
partes_wide <- partes %>% 
  dplyr::filter(!is.na(nome)) %>% 
  dplyr::group_by(n_processo, polo) %>% 
  dplyr::summarise(nome = stringr::str_c(nome, collapse = ", "), .groups = "drop") %>% 
  tidyr::pivot_wider(names_from = polo, values_from = nome)


dados_pg <- dados_pg %>% 
  dplyr::select(-partes) %>% 
  dplyr::inner_join(partes_wide, "n_processo") %>% 
  dplyr::rename(polo_ativo = Ativo, polo_passivo = Passivo) %>% 
  dplyr::distinct()

dados_pg %>% readr::write_rds("/mnt/dados/abj/levantamentos/prop_industrial/prop_industrial.rds")
dados_pg %>% 
  writexl::write_xlsx("/mnt/dados/abj/levantamentos/prop_industrial/prop_industrial_pg.xlsx")


