library(magrittr)

processos <- readxl::read_excel("data-raw/varas-empresariais-frederico/processos_empresariais_atualizado.xlsx")
ids <- processos$id_processo %>%
  abjutils::clean_cnj() %>%
  unique()

cposg_downloaded <- purrr::map(
  ids,
  lex::tjsp_cposg_download,
  "data-raw/varas-empresariais-frederico/bases/cposg",
  "27022971889", "pesquisa"
)

cposg_downloaded <- fs::dir_ls("data-raw/varas-empresariais-frederico/bases/cposg")
da_cposg <- purrr::map_dfr(cposg_downloaded, lex::tjsp_cposg_parse, .id = "file")
readr::write_rds(
  da_cposg, "data-raw/varas-empresariais-frederico/da_cposg.rds",
  compress = "xz"
)
