library(RSelenium)
library(magrittr)

driver <- RSelenium::rsDriver(browser = "firefox")
u <- "https://juris.trt8.jus.br/pesquisajulgados/"
driver$client$navigate(u)

# parametros gerais
el <- driver$client$findElement("xpath", "//*[@id='form:idData1_input']")
el$sendKeysToElement(list('16/03/2016'))
el <- driver$client$findElement("xpath", "//*[@id='form:idData2_input']")
el$sendKeysToElement(list('16/03/2022'))
el <- driver$client$findElement("xpath", "//label[@for='form:idBasePesquisa:1']/preceding-sibling::div")
el$clickElement()


# procrastinatorio --------------------------------------------------------

el <- driver$client$findElement("xpath", "//*[@id='form:idTermo']")
el$sendKeysToElement(list('"procrastinatório"'))
el <- driver$client$findElement("xpath", "//*[@id='form:idBtnPesquisar2']")
el$clickElement()

el <- driver$client$findElement("xpath", "//*[@id='form:j_idt150:j_idt151']")

n_items <- el$getElementText() |>
  unlist() |>
  readr::parse_number()

pags <- seq_len(n_items %/% 10 + 1)

path <- "data-raw/tadeu-procastinatorio/pags"
fs::dir_create(path)

for (ii in pags) {
  f <- sprintf("%s/%02d.html", path, ii)
  if (!file.exists(f)) {
    xp <- stringr::str_glue("//*[@aria-label='Page {ii}']")
    el <- driver$client$findElement("xpath", xp)
    el$clickElement()
    Sys.sleep(3)
    pag <- driver$client$getPageSource()[[1]]
    readr::write_file(pag, f)
  }
}


# embargos de declaracao --------------------------------------------------

# foi necessario quebrar a consulta por ano, pois o sistema só mostra
# as 500 primeiras páginas de uma consulta
for (ano in 2021:2021) {

  el <- driver$client$findElement("xpath", "//*[@id='form:idData1_input']")
  el$clearElement()
  el$sendKeysToElement(list(stringr::str_glue('16/03/{ano}')))
  el <- driver$client$findElement("xpath", "//*[@id='form:idData2_input']")
  el$clearElement()
  el$sendKeysToElement(list(stringr::str_glue('16/03/{ano+1}')))

  el <- driver$client$findElement("xpath", "//*[@id='form:idTermo']")
  el$clearElement()
  el$sendKeysToElement(list('"embargos de declaração"'))
  el <- driver$client$findElement("xpath", "//*[@id='form:idBtnPesquisar2']")
  el$click()
  el$clickElement()

  Sys.sleep(5)

  el <- driver$client$findElement("xpath", "//*[contains(@class,'ui-paginator-first')]")
  el$clickElement()

  el <- driver$client$findElement("xpath", "//*[@id='form:j_idt150:j_idt151']")
  n_items <- el$getElementText() |>
    unlist() |>
    readr::parse_number()

  pags <- seq_len(n_items %/% 10 + 1)
  path <- stringr::str_glue("data-raw/tadeu-procastinatorio/pags_embargos/{ano}")
  fs::dir_create(path)

  for (ii in pags) {
    f <- sprintf("%s/%02d.html", path, ii)
    if (!file.exists(f)) {
      xp <- stringr::str_glue("//*[@aria-label='Page {ii}']")
      el <- driver$client$findElement("xpath", xp)
      el$clickElement()
      Sys.sleep(3)
      pag <- driver$client$getPageSource()[[1]]
      readr::write_file(pag, f)
    }
  }

}


driver$client$close()


# parse -------------------------------------------------------------------

path <- "data-raw/tadeu-procastinatorio/pags"
arqs <- fs::dir_ls(path)

path_embargos <- "data-raw/tadeu-procastinatorio/pags_embargos"
arqs_embargos <- fs::dir_ls(path_embargos, type = "file", recurse = TRUE)

parse_row <- function(h) {
  fields <- h |>
    xml2::xml_find_all(".//*[@class='result-field']") |>
    xml2::xml_text()
  values <- h |>
    xml2::xml_find_all(".//*[@class='result-value']") |>
    xml2::xml_text() |>
    purrr::set_names(fields) |>
    tibble::enframe() |>
    tidyr::pivot_wider() |>
    janitor::clean_names() |>
    dplyr::mutate(
      data = lubridate::dmy(data),
      id_processo = h |>
        xml2::xml_find_all(".//*[@class='result-tipo-base']") |>
        xml2::xml_text(),
      ementa_min = h |>
        xml2::xml_find_all(".//*[@class='result-textlong']") |>
        xml2::xml_text(),
      titulo = h |>
        xml2::xml_find_all(".//*[@class='result-title']") |>
        xml2::xml_text(),
      link_processo = h |>
        xml2::xml_find_all(".//*[@class='result-tipo-base']") |>
        xml2::xml_attr("href"),
      link_docs = h |>
        xml2::xml_find_all(".//*[@class='result-link-icon']") |>
        xml2::xml_attr("href")
    ) |>
    dplyr::relocate(id_processo, .before = 1)
}

parse_page <- function(arq) {
  arq |>
    xml2::read_html(encoding = "UTF-8") |>
    xml2::xml_find_all("//*[@class='ui-datagrid-row']") |>
    purrr::map_dfr(parse_row, .id = "id_item")
}

results_procastinatorio <- purrr::map_dfr(arqs, parse_page, .id = "id_page")
results_embargos <- purrr::map_dfr(arqs_embargos, parse_page, .id = "id_page")


readr::write_rds(
  results_procastinatorio,
  "data-raw/tadeu-procastinatorio/da_results.rds"
)
readr::write_rds(
  results_embargos,
  "data-raw/tadeu-procastinatorio/da_results_embargos.rds"
)

writexl::write_xlsx(
  results_procastinatorio,
  "data-raw/tadeu-procastinatorio/procrastinatorio.xlsx"
)
writexl::write_xlsx(
  results_embargos,
  "data-raw/tadeu-procastinatorio/embargos.xlsx"
)
