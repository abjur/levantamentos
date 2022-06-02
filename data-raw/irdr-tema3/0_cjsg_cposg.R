library(magrittr)

# Download CJSG -----------------------------------------------------------

termos_busca <- c("Tema 3", "IRDR nº 2121567-08.2016.8.26.0000",
           "Ação de Prestação de Contas",
           "Prestação de Contas") %>% 
  stringr::str_c(collapse = " OU ")

path <- "/mnt/dados/abj/levantamentos/irdr_tema3/cjsg"

download_cjsg <- function(ano) {
  path <- paste0(path, ano)
  lex::tjsp_cjsg_download(
    busca = termos_busca,
    dir = path,
    julgamento_ini = paste0(ano, "-01-01"), julgamento_fim = paste0(ano, "-12-31"))
}

poss_download_cjsg <- purrr::possibly(download_cjsg, NULL)
purrr::map(seq(2013, 2019), poss_download_cjsg)

# Parse CJSG -------------------------------------------------------------------

fs::dir_ls(path, recurse = TRUE, regexp = "pagina") %>% 
  lex::tjsp_cjsg_parse() %>% 
  readr::write_rds(paste0(path, ".rds"))

da_cjsg <- readr::read_rds(paste0(path, ".rds"))
dplyr::glimpse(da_cjsg)

# Download CPOSG ----------------------------------------------------------

ids <- readr::read_rds("/mnt/dados/abj/levantamentos/irdr_tema3/cjsg.rds") %>% 
  with(n_processo) %>% 
  abjutils::clean_cnj() %>% 
  unique()

poss_download <- purrr::possibly(lex::tjsp_cposg_download, NULL)

ids %>% 
  purrr::map(poss_download, dir = "/mnt/dados/abj/levantamentos/irdr_tema3/cposg",
             login = "270.229.718-89", senha = "pesquisa")

# Parse CPOSG -------------------------------------------------------------

fs::dir_ls("/mnt/dados/abj/levantamentos/irdr_tema3/cposg") %>% 
  lex::tjsp_cposg_parse() %>% 
  readr::write_rds("/mnt/dados/abj/levantamentos/irdr_tema3/cposg.rds")

da_cposg <- readr::read_rds("/mnt/dados/abj/levantamentos/irdr_tema3/cposg.rds")

# Export ------------------------------------------------------------------

da_cjsg %>% 
  writexl::write_xlsx("/mnt/dados/abj/levantamentos/irdr_tema3/decisoes_sg.xlsx")

da_cposg %>% 
  dplyr::select_if(purrr::negate(is.list)) %>% 
  dplyr::select(-arq) %>% 
  writexl::write_xlsx("/mnt/dados/abj/levantamentos/irdr_tema3/processos_sg.xlsx")

da_cposg %>% 
  dplyr::select(id_processo, partes) %>% 
  tidyr::unnest(partes) %>% 
  writexl::write_xlsx("/mnt/dados/abj/levantamentos/irdr_tema3/partes.xlsx")
