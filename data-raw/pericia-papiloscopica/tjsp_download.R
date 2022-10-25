# download cjpg

tjsp_download <- function(
  busca, dir = ".", classe = "", assunto = "", comarca = "", n_processo = "",
  data_ini = "", data_fim = "", pagina_ini = 1, pagina_fim = Inf
) {
  stopifnot(pagina_ini <= pagina_fim)
  strings <- list(classe, assunto, comarca) %>%
    purrr::modify(stringr::str_c, collapse = ",")
  dates <- list(data_ini, data_fim) %>% purrr::modify(lex:::date_pt)
  n_processo <- if (n_processo != "") {
    lex:::build_id(n_processo)
  } else {
    n_processo
  }
  query_post <- list(
    conversationId = "",
    dadosConsulta.pesquisaLivre = busca,
    tipoNumero = "UNIFICADO",
    numeroDigitoAnoUnificado = stringr::str_sub(n_processo, 1, 15),
    foroNumeroUnificado = stringr::str_sub(n_processo,-4,-1),
    dadosConsulta.nuProcesso = n_processo,
    classeTreeSelection.values = strings[[1]],
    assuntoTreeSelection.values = strings[[2]],
    contadoragente = 0,
    contadorMaioragente = 0,
    dadosConsulta.dtInicio = dates[[1]],
    dadosConsulta.dtFim = dates[[2]],
    varasTreeSelection.values = strings[[3]],
    dadosConsulta.ordenacao = "DESC"
  )
  dir.create(dir, FALSE, TRUE)
  path <- normalizePath(dir)
  file <- stringr::str_c(path, "/search.html")
  httr::POST(
    "https://esaj.tjsp.jus.br/cjpg/pesquisar.do",
    body = query_post,
    httr::config(ssl_verifypeer = FALSE),
    httr::write_disk(file, TRUE),
    lex:::esaj_ua()
  )
  n_pages <- file %>%
    xml2::read_html() %>% xml2::xml_find_all("//*[@id='resultados']/table[1]") %>%
    xml2::xml_text() %>% stringr::str_extract_all(" [0-9]+") %>%
    purrr::pluck(1) %>% stringr::str_trim() %>% as.numeric() %>%
    magrittr::divide_by(.[1]) %>% purrr::pluck(2) %>%
    base::ceiling() %>% min(pagina_fim)
  download_pages <- function(page, path) {
    query_get <- list(pagina = page, conversationId = "")
    GET <- purrr::possibly(httr::GET, "")
    file <- stringr::str_c(
      path, "/pag_", stringr::str_pad(page, 5, "left", "0"), ".html"
    )
    out <- GET(
      "https://esaj.tjsp.jus.br/cjpg/trocarDePagina.do",
      query = query_get,
      httr::config(ssl_verifypeer = FALSE),
      httr::write_disk(file, TRUE),
      lex:::esaj_ua()
    )
    if (is.character(out)) {
      file <- out
    }
    else {
      file <- normalizePath(file)
    }
    return(file)
  }
  files <- purrr::map(pagina_ini:n_pages, download_pages, path = path)
  return(c(file, purrr::flatten_chr(files)))
}

download_ano <- function(ano) {
  tjsp_download(
    busca = "papiloscopia OU papiloscopica OU papiloscopico",
    dir = paste0("/mnt/dados/abj/levantamentos/data-raw/pericia-papiloscopica/tjsp_cjpg/", ano),
    data_ini = paste0(ano, "-01-01"),
    data_fim = paste0(ano, "-12-31")
  )
}

tjsp <- purrr::map(2015:2022, download_ano)

tjsp <- fs::dir_ls("data-raw/pericia-papiloscopica/tjsp_cjpg/", recurse = TRUE, glob = "*.html")
cjpg_parsed <- purrr::map_dfr(tjsp, lex::tjsp_cjpg_parse)

dplyr::glimpse(cjpg_parsed)
readr::write_csv(cjpg_parsed, "data-raw/pericia-papiloscopica/tjsp.csv")
