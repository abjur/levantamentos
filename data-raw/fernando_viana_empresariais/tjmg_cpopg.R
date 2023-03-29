bh <- readr::read_rds("data-raw/fernando_viana_empresariais/bh.rds")
bh <- dplyr::distinct(bh, processo, .keep_all = TRUE)

# download ----

tjmg_pje_download <- function(id, dir) {
  fs::dir_create(dir)

  file <- fs::path(dir, stringr::str_remove_all(id, "[^0-9]"), ext = "html")
  id <- lex:::build_id(id)
  resp_meta <- httr::GET("https://pje-consulta-publica.tjmg.jus.br/pje/ConsultaPublica/listView.seam")
  viewstate <- lex:::get_viewstate(resp_meta)
  fpp <- xml2::xml_attr(xml2::xml_find_all(
    xml2::read_html(resp_meta),
    "//script[contains(@id, 'fPP')]"
  ), "id")
  body_query <- list(
    AJAXREQUEST = "_viewRoot",
    `fPP:numProcesso-inputNumeroProcessoDecoration:numProcesso-inputNumeroProcesso` = id,
    mascaraProcessoReferenciaRadio = "on",
    `fPP:j_id150:processoReferenciaInput` = "",
    `fPP:dnp:nomeParte` = "",
    `fPP:j_id166:nomeSocial` = "",
    `fPP:j_id175:alcunha` = "",
    `fPP:j_id184:nomeAdv` = "",
    `fPP:j_id193:classeProcessualProcessoHidden` = "",
    tipoMascaraDocumento = "on",
    `fPP:dpDec:documentoParte` = "",
    `fPP:Decoration:numeroOAB` = "",
    `fPP:Decoration:j_id228` = "",
    `fPP:Decoration:estadoComboOAB` = "org.jboss.seam.ui.NoSelectionConverter.noSelectionValue",
    fPP = "fPP",
    autoScroll = "",
    javax.faces.ViewState = viewstate,
    `AJAX:EVENTS_COUNT` = "1"
  )
  body_query[fpp] <- fpp

  resp_query <- httr::POST(
    "https://pje-consulta-publica.tjmg.jus.br/pje/ConsultaPublica/listView.seam",
    body = body_query, encode = "form"
  )
  url_data <- xml2::read_html(resp_query) |>
    xml2::xml_find_first("//a[@href='javascript:void();']") |>
    xml2::xml_attr("onclick") |>
    stringr::str_extract("(?<=\\,').+(?='\\))") |>
    stringr::str_c("https://pje-consulta-publica.tjmg.jus.br", ... = _)

  httr::GET(url_data, httr::write_disk(file, TRUE))
  file
}

processos <- purrr::map(
  bh$processo,
  purrr::possibly(tjmg_pje_download),
  dir = "data-raw/fernando_viana_empresariais/tjmg"
)

tjmg_pje_download(
  id <- bh$processo[[19]],
  dir = "data-raw/fernando_viana_empresariais/tjmg"
)


# parse ----

tjmg_pje_parse_info <- function(html) {
  capa <- xml2::read_html(html)
  meta <- capa |>
    xml2::xml_find_all("//div[@class='rich-stglpanel-body ']/table") |>
    lex:::xml_table() |>
    purrr::pluck(1) |>
    purrr::flatten() |>
    purrr::discard(is.na) |>
    purrr::discard(~.x == "") |>
    stringr::str_split("\n", n = 2) |>
    purrr::map(stringr::str_squish) |>
    purrr::transpose() |>
    purrr::set_names(
      "key",
      "val"
    ) |>
    tibble::as_tibble() |>
    tidyr::unnest(key) |>
    tidyr::unnest(val) |>
    tidyr::pivot_wider(names_from = key, values_from = val) |>
    janitor::clean_names()
  meta
}

