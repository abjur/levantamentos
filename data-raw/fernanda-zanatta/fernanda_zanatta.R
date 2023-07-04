# preparação --------------------------------------------------------------

mes_fim <- lubridate::today() |> lubridate::month()
mes_inicio <- mes_fim - 1

if(mes_fim < 10) {
  dt_ini <- glue::glue("2023-0{mes_inicio}-17")
  dt_fim <- glue::glue("2023-0{mes_fim}-16")
} else if(mes_fim == 10) {
  dt_ini <- glue::glue("2023-0{mes_inicio}-17")
  dt_fim <- glue::glue("2023-{mes_fim}-16")
} else {
  dt_ini <- glue::glue("2023-{mes_inicio}-17")
  dt_fim <- glue::glue("2023-{mes_fim}-16")
}


trf4_cjsg_tidy <- function(da) {
  da |>
    dplyr::mutate(
      key = ifelse(
        test = key == val,
        yes = NA_character_,
        no = key
      ),
      key = ifelse(
        test = stringr::str_detect(val, "^[\\w ]+\\:"),
        yes = stringr::str_extract(val, "^[\\w ]+\\:"),
        no = key
      ),
      key = stringr::str_remove(key, "\\:"),
      key = ifelse(
        test = stringr::str_detect(key, "[0-9]"),
        yes = "Tipo",
        no = key
      ),
      key = ifelse(key %in% c("Relatora", "Relator"), "Rel", key),
      val = stringr::str_remove_all(val, "^.+\\: ")
    ) |>
    tidyr::pivot_wider(names_from = key, values_from = val) |>
    janitor::clean_names()
}

# download e parse --------------------------------------------------------

tipos_pesquisa <- c("Turmas Recursais", "TRF4")

da_trf4 <- tibble::tibble()

for(tipo_pesquisa in tipos_pesquisa){

  if(tipo_pesquisa == "TRF4") {
    dir <- "data-raw/fernanda-zanatta/html/trf4"
  } else {
   dir <- "data-raw/fernanda-zanatta/html/turmas_recursais"
  }

  # download
  lex::trf4_cjsg_download(
    busca = "1234",
    dt_ini = dt_ini,
    dt_fim = dt_fim,
    tipo = tipo_pesquisa,
    dir = dir
  )

  # parse
  da <- fs::dir_ls(dir) |>
    purrr::map_dfr(lex::trf4_cjsg_parse) |>
    dplyr::mutate(data = purrr::map_dfr(data, trf4_cjsg_tidy)) |>
    tidyr::unnest(data) |>
    dplyr::mutate(
      origem = tipo_pesquisa
    ) |>
    dplyr::mutate(
      processo = abjutils::clean_cnj(processo),
      classe = stringr::str_squish(classe),
      classe = stringr::str_remove(classe, "^- ")
    ) |>
    dplyr::select(
      processo,
      origem,
      tipo,
      classe,
      data_da_decisao,
      rel
    )

  da_trf4 <- da_trf4 |>
    dplyr::bind_rows(da)

}

# salva --------------------------------------------------------------------

googlesheets4::gs4_auth("rfeliz@abj.org.br")

link <- "https://docs.google.com/spreadsheets/d/1EGbkYaohqakJ9ues2A3ch6CcH88xT37MM2krih1fju8/edit?usp=sharing"

if(mes_fim < 10) {
  nome_arquivo <- glue::glue("da_0{mes_inicio}_0{mes_fim}")
} else if(mes_fim == 10) {
  nome_arquivo <- glue::glue("da_0{mes_inicio}_{mes_fim}")
} else {
  nome_arquivo <- glue::glue("da_{mes_inicio}_{mes_fim}")
}

googlesheets4::write_sheet(
  da_trf4,
  link,
  nome_arquivo
)
