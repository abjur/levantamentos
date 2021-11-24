


path <- "~/Documents/abj/trabalhista/Processos/Processos"
pastas <- fs::dir_ls(path)

todos_arquivos <- purrr::map_dfr(pastas, ~fs::file_info(fs::dir_ls(.x)), .id = "pasta")

rx_planilha <- stringr::regex("planilha", TRUE)
planilhas <- todos_arquivos |>
  dplyr::filter(stringr::str_detect(path, rx_planilha))

browseURL(planilhas$path[1])

safe_extract <- tabulizer::extract_tables |>
  purrr::partial(output = "data.frame") |>
  purrr::possibly(list(tibble::tibble(erro = "erro"))) |>
  purrr::quietly()

progressr::with_progress({
  f <- planilhas$path
  prog <- progressr::progressor(along = f)
  tabelas <- f |>
    purrr::map(~{
      prog()
      safe_extract(.x)
    })
})

readr::write_rds(tabelas, "data-raw/planilhas-trabalhistas/tabelas.rds", compress = "xz")

tabelas[[1]]

results <- tabelas |>
  purrr::map("result")


filtrar_tabelas <- function(tabs, .y) {
  purrr::keep(tabs, ~is.data.frame(.x) && nrow(.x)>=10)
}

results_big <- results |>
  purrr::imap(filtrar_tabelas) |>
  purrr::keep(~length(.x) > 0)

readr::write_rds(results_big, "data-raw/planilhas-trabalhistas/results_big.rds", compress = "xz")
