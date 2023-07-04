tjsp_cpopg_cpf_download <- function(cpf, dir = ".", login = NULL, senha = NULL) {
  stopifnot(length(cpf) == 1)
  cpf <- stringr::str_remove_all(cpf, "[^0-9]")
  dir <- paste0(dir, "/", cpf, "/")
  fs::dir_create(dir)
  lex:::login_esaj(login, senha)
  u_search <- "https://esaj.tjsp.jus.br/cpopg/search.do?gateway=true"
  query <- list(
    "paginaConsulta" = 1,
    "conversationId" = "",
    "dadosConsulta.localPesquisa.cdLocal" = "-1",
    "cbPesquisa" = "DOCPARTE",
    "dadosConsulta.tipoNuProcesso" = "UNIFICADO",
    "dadosConsulta.valorConsulta" = cpf,
    "uuidCaptcha" = ""
  )
  n_pages <- u_search |>
    httr::GET(query = query, httr::config(ssl_verifypeer = FALSE)) |>
    xml2::read_html() |>
    xml2::xml_find_first("//span[@class='resultadoPaginacao']") |>
    xml2::xml_text() |>
    stringr::str_squish() |>
    stringr::str_extract("[0-9]+$") |>
    base::as.numeric() |>
    magrittr::divide_by(25) |>
    base::ceiling()
  out <- c()
  for (page in seq_len(n_pages)) {
    query$paginaConsulta <- page
    html <- u_search |>
      httr::GET(query = query, httr::config(ssl_verifypeer = FALSE)) |>
      xml2::read_html()
    # Pegar links na pagina
    links <- html |>
      xml2::xml_find_all("//*[@class='nuProcesso']/a") |>
      xml2::xml_attr("href") |>
      stringr::str_c("https://esaj.tjsp.jus.br", .)
    # Pegar números dos processos
    files <- html |>
      xml2::xml_find_all("//*[@class='nuProcesso']/a") |>
      xml2::xml_text() |>
      stringr::str_squish() |>
      stringr::str_remove_all("[^0-9]") |>
      stringr::str_c(dir, ., ".html")
    purrr::walk2(
      links, files,
      ~httr::GET(.x, httr::config(ssl_verifypeer = FALSE), httr::write_disk(.y, TRUE))
    )
    out <- c(out, files)
  }
  return(out)
}

poss_download <- purrr::possibly(tjsp_cpopg_cpf_download, NULL, FALSE)

candidatos <- readr::read_csv2("consulta_cand_2020_SP.csv", locale = readr::locale(encoding = "latin1"))

# Prefeitos ---------------------------------------------------------------

candidatos |>
  dplyr::filter(DS_CARGO == "PREFEITO") |>
  dplyr::pull(NR_CPF_CANDIDATO) |>
  purrr::map(poss_download,
             dir = "/mnt/dados/abj/levantamentos/criminal-candidatos-g1/prefeitos/",
             login = "270.229.718-89",
             senha = "pesquisa")


# Vice-prefeitos ---------------------------------------------------------------

candidatos |>
  dplyr::filter(DS_CARGO == "VICE-PREFEITO") |>
  dplyr::pull(NR_CPF_CANDIDATO) |>
  purrr::map(
    poss_download,
    dir = "/mnt/dados/abj/levantamentos/candidatos_sp_2020/vice_prefeitos",
    login = "270.229.718-89",
    senha = "pesquisa"
  )


# Vereadores --------------------------------------------------------------

candidatos |>
  dplyr::filter(DS_CARGO == "VEREADOR") |>
  dplyr::pull(NR_CPF_CANDIDATO) |>
  purrr::map(
    poss_download,
    dir = "/mnt/dados/abj/levantamentos/criminal-candidatos-g1/vereadores",
    login = "270.229.718-89",
    senha = "pesquisa"
  )


