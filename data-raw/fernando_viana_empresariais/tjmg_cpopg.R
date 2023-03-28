bh <- readr::read_rds("data-raw/fernando_viana_empresariais/bh.rds")
bh <- dplyr::distinct(bh, processo, .keep_all = TRUE)

teste <- bh$processo[[1]]
https://pje-consulta-publica.tjmg.jus.br/
tjmg_pje_download <- function (id, dir = ".")
{
  stopifnot(length(id) == 1)
  fs::dir_create(dir)
  file <- fs::path(dir, stringr::str_remove_all(id, "[^0-9]"),
                   ext = "html")
  id <- build_id(id)
  resp_meta <- httr::GET("https://pje-consulta-publica.tjmg.jus.br/pje/ConsultaPublica/listView.seam")
  viewstate <- lex:::get_viewstate(resp_meta)
  fpp <- xml2::xml_attr(xml2::xml_find_all(xml2::read_html(resp_meta),
                                           "//script[contains(@id, 'fPP')]"), "id")
  body_query <- list(AJAXREQUEST = "_viewRoot", `fPP:numProcesso-inputNumeroProcessoDecoration:numProcesso-inputNumeroProcesso` = id,
                     mascaraProcessoReferenciaRadio = "on", `fPP:j_id150:processoReferenciaInput` = "",
                     `fPP:dnp:nomeParte` = "", `fPP:j_id168:nomeAdv` = "",
                     `fPP:j_id177:classeProcessualProcessoHidden` = "", tipoMascaraDocumento = "on",
                     `fPP:dpDec:documentoParte` = "", `fPP:Decoration:numeroOAB` = "",
                     `fPP:Decoration:j_id212` = "", `fPP:Decoration:estadoComboOAB` = "org.jboss.seam.ui.NoSelectionConverter.noSelectionValue",
                     fPP = "fPP", autoScroll = "", javax.faces.ViewState = viewstate,
                     `AJAX:EVENTS_COUNT` = "1")
  body_query[fpp] <- fpp
  resp_query <- httr::POST("https://pje-consulta-publica.tjmg.jus.br/pje/ConsultaPublica/listView.seam",
                           body = body_query, encode = "form")
  url_data <- stringr::str_c("https://pje-consulta-publica.tjmg.jus.br",
                             ... = stringr::str_extract(xml2::xml_attr(xml2::xml_find_first(xml2::read_html(resp_query),
                                                                                            "//a[@href='javascript:void();']"), "onclick"),
                                                        "/consulta.+(?=')"))
  httr::GET(url_data, httr::write_disk(file, TRUE))
  return(file)
}
