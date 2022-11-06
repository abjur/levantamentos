get_href <- function(url, xpath) {
  url |>
    paste0("https://dadosabertos.web.stj.jus.br", ... = _) |>
    xml2::read_html() |>
    xml2::xml_find_all(xpath) |>
    xml2::xml_attr("href")
}

acordaos <- "/group/jurisprudencia" |>
  get_href("//h2[@class='dataset-heading']/a") |>
  purrr::map(get_href, "//a[@class='heading' and contains(@title,'json')]") |>
  purrr::flatten_chr() |>
  purrr::map(get_href, "//a[@class='resource-url-analytics']") |>
  purrr::flatten_chr() |>
  purrr::map_dfr(jsonlite::fromJSON)

readr::write_rds(acordaos, "data-raw/pericia-papiloscopica/stj_acordaos.rds")

acordaos |>
  dplyr::filter(
    stringr::str_detect(ementa, stringr::regex("papilosc", ignore_case = TRUE))
  ) |>
  dplyr::glimpse()

