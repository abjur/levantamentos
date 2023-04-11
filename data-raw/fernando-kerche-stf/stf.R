# Fernando Kerche
# MPF no STF

ativo <- readr::read_csv("data-raw/fernando-kerche-stf/mpf_ativo.csv")
passivo <- readr::read_csv("data-raw/fernando-kerche-stf/mpf_passivo.csv")

links <- unique(ativo$link, passivo$link)

teste <- links[1]

xml2::read_html(teste)

ativo |>
  dplyr::slice(1) |>
  dplyr::glimpse()


