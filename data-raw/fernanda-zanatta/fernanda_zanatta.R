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
    dplyr::mutate(
      origem = tipo_pesquisa
    ) |>
    dplyr::mutate(
      processo = abjutils::clean_cnj(processo),
      classe = stringr::str_squish(classe),
      classe = stringr::str_remove(classe, "^- ")
    ) |>
    dplyr::transmute(
      processo,
      origem,
      tipo,
      classe,
      data_da_decisao,
      rel = dplyr::coalesce(relator, relatora)
    )

  fs::dir_ls(dir) |>
    purrr::map(fs::file_delete)

  da_trf4 <- da_trf4 |>
    dplyr::bind_rows(da)

}

# salva --------------------------------------------------------------------


# 0) se precisar transferir do servidor para o pessoal --------------------

readr::write_rds(da_trf4, "data-raw/fernanda-zanatta/da/da_trf4.rds")

# 1) autenticar
# se eu estiver no servidor
obsutils::autenticar_gsheets()
# e compartilha o arquivo com o email: obsdash@abj-dev.iam.gserviceaccount.com

# se eu estiver no local
googlesheets4::gs4_auth("rfeliz@abj.org.br")

# 2) salva o link
link <- "https://docs.google.com/spreadsheets/d/1EGbkYaohqakJ9ues2A3ch6CcH88xT37MM2krih1fju8/edit?usp=sharing"

# 3) nome do arquivo
if(mes_fim < 10) {
  nome_arquivo <- glue::glue("da_0{mes_inicio}_0{mes_fim}")
} else if(mes_fim == 10) {
  nome_arquivo <- glue::glue("da_0{mes_inicio}_{mes_fim}")
} else {
  nome_arquivo <- glue::glue("da_{mes_inicio}_{mes_fim}")
}

# 4) roda tudo :)
googlesheets4::write_sheet(
  da_trf4,
  link,
  nome_arquivo
)
