# Baixa dados do CJPG

path <- "/media/jt/129CE2040F463C57/ivan_ribeiro"
muni_rmsp <- "https://pt.wikipedia.org/wiki/Regi%C3%A3o_Metropolitana_de_S%C3%A3o_Paulo#Munic%C3%ADpios" |>
  httr::GET() |>
  xml2::read_html() |>
  xml2::xml_find_first("//table[contains(@class, 'sortable')]") |>
  rvest::html_table() |>
  janitor::clean_names() |>
  with(municipio) |>
  stringr::str_subset("Total", negate = TRUE) |>
  stringr::str_remove_all("[^[:alpha:] -]") |>
  arrumar_txt()

arrumar_txt <- function(x) {
  x |>
    toupper() |>
    abjutils::rm_accent()
}


# download cjpg -----------------------------------------------------------


path_cjpg <- paste0(path, "/cjpg")
files <- lex::tjsp_cjpg_download(
  busca = "",
  dir = path_cjpg,
  classe = c("8714", "8501"),
  pagina_fim = 3000
)



# parse cjpg --------------------------------------------------------------

files <- fs::dir_ls(path_cjpg)
dados <- purrr::map_dfr(files, lex::tjsp_cjpg_parse, .id = "file")


# filter cjpg municipios --------------------------------------------------

da_cjpg_rmsp <- dados |>
  dplyr::mutate(comarca = arrumar_txt(comarca)) |>
  dplyr::filter(comarca %in% muni_rmsp)

p_cjpg_rmsp <- unique(da_cjpg_rmsp$n_processo)


# download documentos -----------------------------------------------------

path_docs <- paste0(path, "/docs/")
future::plan(future::multisession)
progressr::with_progress({
  p <- progressr::progressor(length(p_cjpg_rmsp))
  furrr::future_walk(p_cjpg_rmsp, ~{
    p()
    if (!fs::dir_exists(paste0(path_docs, .x))) {
      lex::tjsp_peticoes_download(
        .x,
        path = path_docs,
        only_petitions = TRUE
      )
    }
  })
})

pastas_com_arquivos <- fs::dir_ls(path_docs) |>
  purrr::keep(~length(fs::dir_ls(.x, regexp = "pdf$")) > 0)

processos_validos <- pastas_com_arquivos |>
  tibble::enframe() |>
  dplyr::mutate(value = basename(value))

da_filtrado <- da_cjpg_rmsp |>
  dplyr::semi_join(processos_validos, c("n_processo" = "value")) |>
  dplyr::mutate(resumo = stringr::str_trunc(resumo, 32000))

writexl::write_xlsx(da_filtrado, "/media/jt/129CE2040F463C57/ivan_ribeiro/dados_processos.xlsx")

