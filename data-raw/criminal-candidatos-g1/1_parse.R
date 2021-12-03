library(magrittr)

path <- "/mnt/dados/abj/levantamentos/candidatos_sp_2020/vereadores"

fs::dir_ls(path, recurse = TRUE, glob = "*.html") %>%
  lex::tjsp_cpopg_parse() %>%
  readr::write_rds(paste0(path, ".rds"))

readr::read_rds(paste0(path, ".rds")) %>%
  dplyr::filter(area == "Criminal") %>%
  readr::write_rds(paste0(path, "_criminal.rds"))

read_rds("/mnt/dados/abj/levantamentos/candidatos_sp_2020/vereadores_criminal.rds") %>%
  mutate(cpf_candidato = str_remove_all(str_extract(arq, regex("/.+/")), "/")) %>%
  select(-c(erro, arq)) %>%
  relocate(cpf_candidato, 1) %>%
  write_rds("/mnt/dados/abj/levantamentos/candidatos_sp_2020/vereadores_criminal.rds")
