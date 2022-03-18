library(RSelenium)
library(magrittr)

driver <- RSelenium::rsDriver(browser = "firefox")
u <- "https://juris.trt8.jus.br/pesquisajulgados/"
driver$client$navigate(u)


# Clicar no brasil
el <- driver$client$findElement("xpath", "//*[@id='form:idTermo']")
el$sendKeysToElement(list('"procrastinatório"'))
el <- driver$client$findElement("xpath", "//label[@for='form:idBasePesquisa:1']/preceding-sibling::div")
el$clickElement()
el <- driver$client$findElement("xpath", "//*[@id='form:idBtnPesquisar2']")
el$clickElement()

el <- driver$client$findElement("xpath", "//*[@id='form:j_idt150:j_idt151']")
n_items <- el$getElementText() |> 
  unlist() |> 
  readr::parse_number()

pags <- seq_len(n_items %/% 10 + 1)

path <- "~/Documents/abj/levantamentos/data-raw/tadeu-procastinatorio/pags"
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

driver$client$close()

arqs <- fs::dir_ls(path)

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

da_results <- purrr::map_dfr(arqs, parse_page, .id = "id_page")
readr::write_rds(da_results, "data-raw/tadeu-procastinatorio/da_results.rds")

writexl::write_xlsx(da_results, "data-raw/tadeu-procastinatorio/procrastinatorio.xlsx")
