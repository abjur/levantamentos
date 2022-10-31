decisoes <- readr::read_csv("data-raw/justica-gratuita/decisoes_total.csv")
processos <- unique(decisoes$n_processo)

poss_download <- purrr::possibly(lex::tjsp_cpopg_download, otherwise = NULL)
download_slow <- function(cnj) {
  path <- poss_download(
    cnj,
    dir = "data-raw/justica-gratuita/dados/cpopg",
    login = "270.229.718-89",
    senha = "pesquisa"
  )
  Sys.sleep(2)
  return(path)
}
downloads <- purrr::map_chr(
  processos,
  download_slow
)

fs::dir_ls("data-raw/justica-gratuita/dados/cpopg/") |> length()
info <- fs::dir_info("data-raw/justica-gratuita/dados/cpopg/")

info |>
  dplyr::arrange(size) |>
  dplyr::filter(size < 60000) |>
  dplyr::pull(path) |>
  purrr::pluck(1)
  httr::BROWSE()
  dplyr::glimpse()



# poss_parse <- purrr::possibly(lex::tjsp_cpopg_parse, NULL)
# cpopg_parsed <- purrr::map_dfr(downloads, poss_parse)
#
# cpopg_parsed |>
#   dplyr::distinct() |>
#   dplyr::glimpse()
#
# downloads[[1]] |>
#   httr::BROWSE()
# downloads |>
#   fs::file_info()
