pesquisa <- c(
  '"acolho a impugnação à gratuidade da justiça"',
  '"acolho a impugnação à assistência judiciária gratuita"',
  '"acolho a impugnação à justiça gratuita"',
  '"indefiro a impugnação à gratuidade da justiça"',
  '"indefiro a impugnação à justiça gratuita"',
  '"indefiro a impugnação à assistência judiciária gratuita"',
  '"defiro a gratuidade da justiça"',
  '"defiro o pedido de gratuidade da justiça"',
  '"defiro a assistência judiciária gratuita"',
  '"defiro o pedido de assistência judiciária gratuita"',
  '"defiro a justiça gratuita"',
  '"defiro o pedido de justiça gratuita"',
  '"indefiro a justiça gratuita"',
  '"indefiro o pedido de justiça gratuita"',
  '"indefiro a assistência judiciária gratuita"',
  '"indefiro o pedido de assistência judiciária gratuita"',
  '"indefiro a gratuidade da justiça"',
  '"indefiro o pedido de gratuidade da justiça"'
)

query <- paste(pesquisa, collapse = " ou ")

cjpg <- lex::tjsp_cjpg_download(
  busca = query,
  dir = "data-raw/justica-gratuita/cjpg_2012",
  data_ini = "2012-07-01", data_fim = "2012-12-31"
)

for (year in c(2013:2021)) {
  cjpg <- lex::tjsp_cjpg_download(
    busca = pesquisa,
    dir = paste0("data-raw/justica-gratuita/cjpg_", year),
    data_ini = paste0(year, "-01-01"), data_fim = paste0(year, "-12-31")
  )
}

cjpg <- fs::dir_ls("data-raw/justica-gratuita/", glob = "*.html", recurse = TRUE)
poss_parse <- purrr::possibly(lex::tjsp_cjpg_parse, otherwise = NULL)
cjpg_parsed <- purrr::map_dfr(cjpg, poss_parse)


pesquisa <- pesquisa |>
  stringr::str_remove_all('"') |>
  stringi::stri_trans_general("Latin-ASCII")
cjpg_parsed <- cjpg_parsed |>
  dplyr::distinct() |>
  dplyr::mutate(
    resumo_lc = stringr::str_squish(stringi::stri_trans_general(tolower(resumo), "Latin-ASCII")),
    trecho_decisao = dplyr::case_when(
      stringr::str_detect(resumo_lc, pesquisa[[1]]) ~ pesquisa[[1]],
      stringr::str_detect(resumo_lc, pesquisa[[2]]) ~ pesquisa[[2]],
      stringr::str_detect(resumo_lc, pesquisa[[3]]) ~ pesquisa[[3]],
      stringr::str_detect(resumo_lc, pesquisa[[4]]) ~ pesquisa[[4]],
      stringr::str_detect(resumo_lc, pesquisa[[5]]) ~ pesquisa[[5]],
      stringr::str_detect(resumo_lc, pesquisa[[6]]) ~ pesquisa[[6]],
      stringr::str_detect(resumo_lc, pesquisa[[7]]) ~ pesquisa[[7]],
      stringr::str_detect(resumo_lc, pesquisa[[8]]) ~ pesquisa[[8]],
      stringr::str_detect(resumo_lc, pesquisa[[9]]) ~ pesquisa[[9]],
      stringr::str_detect(resumo_lc, pesquisa[[10]]) ~ pesquisa[[10]],
      stringr::str_detect(resumo_lc, pesquisa[[11]]) ~ pesquisa[[11]],
      stringr::str_detect(resumo_lc, pesquisa[[12]]) ~ pesquisa[[12]],
      stringr::str_detect(resumo_lc, pesquisa[[13]]) ~ pesquisa[[13]],
      stringr::str_detect(resumo_lc, pesquisa[[14]]) ~ pesquisa[[14]],
      stringr::str_detect(resumo_lc, pesquisa[[15]]) ~ pesquisa[[15]],
      stringr::str_detect(resumo_lc, pesquisa[[16]]) ~ pesquisa[[16]],
      stringr::str_detect(resumo_lc, pesquisa[[17]]) ~ pesquisa[[17]],
      stringr::str_detect(resumo_lc, pesquisa[[18]]) ~ pesquisa[[18]]
    )
  )

dplyr::count(cjpg_parsed, trecho_decisao)

readr::write_csv(cjpg_parsed, "data-raw/decisoes.csv")

cjpg_parsed |>
  dplyr::filter(is.na(trecho_decisao)) |>
  dplyr::pull(resumo) |>
  purrr::pluck(1)

