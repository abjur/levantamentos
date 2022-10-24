cnpjs <- c(
  "60.770.336/0001-65",
  "33.987.793/0001-33",
  "33.588.252/0001-32",
  "34.169.557/0001-72",
  "40.429.946/0001-92",
  "24.933.830/0001-30",
  "45.283.173/0001-00",
  "13.220.493/0001-17",
  "34.111.187/0001-12",
  "18.520.834/0001-93"
  )


download_cnpj <- function(cpf, dir = ".", login = Sys.getenv("ESAJ_LOGIN"), senha = Sys.getenv("ESAJ_SENHA")) {
  stopifnot(length(cpf) == 1)
  cpf <- stringr::str_remove_all(cpf, "[^0-9]")
  dir <- paste0(dir, "/", cpf, "/")
  fs::dir_create(dir)
  lex::login_esaj(login, senha)
  u_search <- "https://esaj.tjsp.jus.br/cpopg/search.do?gateway=true"
  query <- list(
    paginaConsulta = 1, conversationId = "", dadosConsulta.localPesquisa.cdLocal = "-1",
    cbPesquisa = "DOCPARTE", dadosConsulta.tipoNuProcesso = "UNIFICADO",
    dadosConsulta.valorConsulta = cpf, uuidCaptcha = ""
  )
  n_pages <- u_search %>%
    httr::GET(
      query = query, httr::config(ssl_verifypeer = FALSE),
      lex:::esaj_ua()
    ) %>%
    xml2::read_html() %>%
    xml2::xml_find_first("//span[@id='contadorDeProcessos']") %>%
    xml2::xml_text() %>%
    stringr::str_squish() %>%
    stringr::str_extract("[0-9]+") %>%
    base::as.numeric() %>%
    magrittr::divide_by(25) %>%
    base::ceiling()
  out <- c()
  for (page in seq_len(n_pages)) {
    query$paginaConsulta <- page
    html <- u_search %>%
      httr::GET(query = query, httr::config(ssl_verifypeer = FALSE)) %>%
      xml2::read_html()
    links <- html %>%
      xml2::xml_find_all("//*[starts-with(@class, 'nuProcesso')]/a") %>%
      xml2::xml_attr("href") %>%
      stringr::str_c(
        "https://esaj.tjsp.jus.br",
        .
      )
    files <- html %>%
      xml2::xml_find_all("//*[starts-with(@class, 'nuProcesso')]/a") %>%
      xml2::xml_text() %>%
      stringr::str_squish() %>%
      stringr::str_remove_all("[^0-9]") %>%
      stringr::str_c(dir, ., ".html")
    purrr::walk2(links, files, ~ httr::GET(
      .x, httr::config(ssl_verifypeer = FALSE),
      httr::write_disk(.y, TRUE)
    ))
    out <- c(out, files)
  }
  return(out)
}

cpopg <- purrr::map(
  cnpjs, download_cnpj, dir = "/mnt/dados/abj/levantamentos/data-raw/bancos-cnpj/cpopg"
)

# Parse ----

cpopg <- fs::dir_ls("/mnt/dados/abj/levantamentos/data-raw/bancos-cnpj/cpopg", recurse = TRUE, glob = "*.html")

df <- purrr::map_dfr(cpopg, lex::tjsp_cpopg_parse)
partes <- df |> 
  dplyr::select(id_processo, partes) |> 
  tidyr::unnest(partes)
movs <- df |> 
  dplyr::select(id_processo, movimentacoes) |> 
  tidyr::unnest(movimentacoes)
df_final <- dplyr::select(df, !is.list)

readr::write_csv(df_final, "/mnt/dados/abj/levantamentos/data-raw/bancos-cnpj/processos.csv")
readr::write_csv(partes, "/mnt/dados/abj/levantamentos/data-raw/bancos-cnpj/partes.csv")
readr::write_csv(movs, "/mnt/dados/abj/levantamentos/data-raw/bancos-cnpj/movimentacoes.csv")
