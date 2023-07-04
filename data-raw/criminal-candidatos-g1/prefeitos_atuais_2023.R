pep <- readr::read_csv2(
  "data-raw/criminal-candidatos-g1/202304_PEP.csv", locale = readr::locale(encoding = "latin1")
) |>
  janitor::clean_names()

pep <- pep |>
  dplyr::filter(sigla_funcao == "PREFEI") |>
  dplyr::mutate(
    uf = stringr::str_extract(nome_orgao, "(?<=-)[A-Z]{2}$"),
    dplyr::across(dplyr::starts_with("data"), lubridate::dmy),
    cpf_join = paste0(stringr::str_sub(cpf, 5, 7), stringr::str_sub(cpf, 9, 11))
  ) |>
  dplyr::filter(uf == "SP", data_fim_exercicio >= lubridate::today())

candidatos <- readr::read_csv2(
  "data-raw/criminal-candidatos-g1/consulta_cand_2020_SP.csv",
  locale = readr::locale(encoding = "latin1")
)

candidatos <- candidatos |>
  janitor::clean_names() |>
  dplyr::filter(stringr::str_detect(ds_cargo, "PREF"), sg_uf == "SP") |>
  dplyr::transmute(
    sg_uf, sg_ue, nm_ue, nm_candidato, nr_cpf_candidato, sg_partido,
    cpf_join = stringr::str_sub(nr_cpf_candidato, 4, 9)
  )

cpf_busca <- candidatos |>
  dplyr::inner_join(pep, by = c("nm_candidato" = "nome_pep", "cpf_join")) |>
  dplyr::pull(nr_cpf_candidato) |>
  unique()


# download cpopg ----

tjsp_cpopg_cpf_download <- function(cpf, dir = ".",
                                    login = Sys.getenv("ESAJ_LOGIN"),
                                    senha = Sys.getenv("ESAJ_SENHA")) {
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
    httr::GET(query = query, httr::config(ssl_verifypeer = FALSE), lex:::esaj_ua()) |>
    xml2::read_html() |>
    xml2::xml_find_first("//span[@id='contadorDeProcessos']") |>
    xml2::xml_text() |>
    stringr::str_squish() |>
    stringr::str_extract("[0-9]+") |>
    base::as.numeric() |>
    magrittr::divide_by(25) |>
    base::ceiling() |>
    dplyr::coalesce(0)

  if (n_pages == 0) {
    fs::dir_delete(dir)
  } else {
    out <- c()

    for (page in seq_len(n_pages)) {
      query$paginaConsulta <- page

      Sys.sleep(1)

      html <- u_search |>
        httr::GET(query = query, httr::config(ssl_verifypeer = FALSE)) |>
        xml2::read_html()

      # Pegar links na pagina
      links <- html |>
        xml2::xml_find_all("//*[starts-with(@class, 'nuProcesso')]/a") |>
        xml2::xml_attr("href") |>
        stringr::str_c("https://esaj.tjsp.jus.br", ... = _)

      # Pegar números dos processos
      files <- html |>
        xml2::xml_find_all("//*[starts-with(@class, 'nuProcesso')]/a") |>
        xml2::xml_text() |>
        stringr::str_squish() |>
        stringr::str_remove_all("[^0-9]") |>
        stringr::str_c(dir, ... = _, ".html")

      purrr::walk2(
        links, files,
        ~ httr::GET(.x, httr::config(ssl_verifypeer = FALSE), httr::write_disk(.y, TRUE))
      )

      out <- c(out, files)
    }

    return(out)
  }
  Sys.sleep(1)
}

cpopg <- purrr::map(
  cpf_busca, tjsp_cpopg_cpf_download,
  dir = "data-raw/criminal-candidatos-g1/cpopg_prefeitos2023",
  .progress = TRUE
)
