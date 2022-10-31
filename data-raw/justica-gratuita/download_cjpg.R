pesquisa <- c(
  '"acolho a impugnação à gratuidade da justiça"',
  '"acolho a impugnação à assistência judiciária gratuita"',
  '"acolho a impugnação à justiça gratuita"',
  '"indefiro a impugnação à gratuidade da justiça"',
  '"indefiro a impugnação à justiça gratuita"',
  '"indefiro a impugnação à assistência judiciária gratuita"',
  '"indefiro a justiça gratuita"',
  '"indefiro o pedido de justiça gratuita"',
  '"indefiro a assistência judiciária gratuita"',
  '"indefiro o pedido de assistência judiciária gratuita"',
  '"indefiro a gratuidade da justiça"',
  '"indefiro o pedido de gratuidade da justiça"',
  '"defiro a gratuidade da justiça"',
  '"defiro o pedido de gratuidade da justiça"',
  '"defiro a assistência judiciária gratuita"',
  '"defiro o pedido de assistência judiciária gratuita"',
  '"defiro a justiça gratuita"',
  '"defiro o pedido de justiça gratuita"'
)

query <- paste(pesquisa, collapse = " ou ")

cjpg <- lex::tjsp_cjpg_download(
  busca = query,
  dir = "data-raw/justica-gratuita/cjpg_2012",
  data_ini = "2012-07-01", data_fim = "2012-12-31"
)

for (year in c(2013:2021)) {
  cjpg <- lex::tjsp_cjpg_download(
    busca = query,
    dir = paste0("data-raw/justica-gratuita/cjpg_", year),
    data_ini = paste0(year, "-01-01"), data_fim = paste0(year, "-12-31")
  )
}
cjpg <- lex::tjsp_cjpg_download(
  busca = query,
  dir = "data-raw/justica-gratuita/cjpg_2022",
  data_ini = "2022-01-01", data_fim = "2022-07-31"
)

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
readr::write_csv(cjpg_parsed, "data-raw/justica-gratuita/decisoes.csv")



aa <- cjpg_parsed$trecho_decisao |>
  unique() |>
  purrr::discard(is.na)
pesquisa <- setdiff(pesquisa, aa)
query <- paste(pesquisa, collapse = "\" ou \"")

cjpg <- lex::tjsp_cjpg_download(
  busca = query,
  dir = "data-raw/justica-gratuita/cjpg_indef_2012",
  data_ini = "2012-07-01", data_fim = "2012-12-31"
)

for (year in c(2013:2021)) {
  cjpg <- lex::tjsp_cjpg_download(
    busca = query,
    dir = paste0("data-raw/justica-gratuita/cjpg_indef_", year),
    data_ini = paste0(year, "-01-01"), data_fim = paste0(year, "-12-31")
  )
}
cjpg <- lex::tjsp_cjpg_download(
  busca = query,
  dir = "data-raw/justica-gratuita/cjpg_indef_2022",
  data_ini = "2022-01-01", data_fim = "2022-07-31"
)


cjpg <- fs::dir_ls("data-raw/justica-gratuita/", regexp = "cjpg_indef_.*\\.html", recurse = TRUE)
cjpg_parsed <- purrr::map_dfr(cjpg, poss_parse)
readr::write_csv(cjpg_parsed, "data-raw/justica-gratuita/decisoes_indef.csv")
pesquisa <- pesquisa |>
  stringr::str_remove_all('"') |>
  stringi::stri_trans_general("Latin-ASCII")

total <- "data-raw/justica-gratuita/decisoes.csv" |>
  readr::read_csv() |>
  dplyr::bind_rows(cjpg_parsed) |>
  dplyr::distinct()

total <- total |>
  dplyr::select(-trecho_decisao) |>
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

dplyr::glimpse(total)

total |>
  dplyr::distinct() |>
  readr::write_csv("data-raw/justica-gratuita/decisoes_total.csv")

fs::dir_ls("data-raw/justica-gratuita/dados/", recurse = TRUE, type = "file", regexp = "cjpg_2.*")


# regex -----
rx_deferimento <- c(
  "(indefiro a impugnacao (de|a) (gratuidade|justica gratuita|assistencia judiciaria gratuita|assistencia gratuita))",
  "(defiro [ao] (pedido d[ea] )?gratuidade( d[ea] justica)?)",
  "(defiro [ao] (pedido d[ea] )?assistencia judiciaria gratuita)",
  "(defiro [ao] (pedido d[ea] )?justica gratuita)",
  "(gratuidade d[ea] justica deve ser mantida)"
) |>
  paste(collapse = "|") |>
  stringr::regex()
rx_indeferimento <- c(
  "(acolho a impugnacao (de|a) (gratuidade|justica gratuita|assistencia judiciaria gratuita|assistencia gratuita))",
  "(indefiro a (justica|assistencia judiciaria) gratuita)",
  "(indefiro o pedido d[ea] (justica|assistencia judiciaria) gratuita)",
  "(indefiro [ao] (pedido d[ea] )?gratuidade( d[ea] justica)?)",
  "(indefiro (os beneficios d)?a gratuidade (processual)?)"
) |>
  paste(collapse = "|") |>
  stringr::regex()

total <- readr::read_csv("data-raw/justica-gratuita/decisoes_total.csv")
total <- total |>
  dplyr::mutate(
    resumo_lc = stringr::str_remove_all(resumo_lc, "[\"',\\(\\)]"),
    trecho_decisao = dplyr::case_when(
      stringr::str_detect(resumo_lc, rx_indeferimento) ~
        stringr::str_extract(resumo_lc, rx_indeferimento),
      stringr::str_detect(resumo_lc, rx_deferimento) ~
        stringr::str_extract(resumo_lc, rx_deferimento)
    ),
    deferimento = dplyr::case_when(
      stringr::str_detect(resumo_lc, rx_indeferimento) ~ "Não",
      stringr::str_detect(resumo_lc, rx_deferimento) ~ "Sim"
    )
  )

total |>
  dplyr::filter(is.na(trecho_decisao)) |>
  #dplyr::count(deferimento) |> janitor::adorn_totals() |> janitor::adorn_percentages(denominator = "col")
  dplyr::pull(resumo_lc) |>
  purrr::pluck(7) |>
  #stringr::str_extract(rx_indeferimento)
  stringr::str_extract(".{50}gratui.{50}")
  dplyr::glimpse()



