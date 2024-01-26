fs::dir_delete("data-raw/alienacao_parental")

cjpg <- lex::tjsp_cjpg_download(
  '"alienação parental"', "data-raw/alienacao_parental",
  data_ini = "2022-01-01", data_fim = "2022-12-31"
)

parsed <- cjpg |>
  purrr::map(lex::tjsp_cjpg_parse) |>
  purrr::list_rbind()

parsed
parsed_total <- parsed_total |>
  dplyr::bind_rows(parsed)

parsed_total <- parsed_total |>
  dplyr::distinct() |>
  dplyr::mutate(ano = lubridate::year(lubridate::dmy(data_de_disponibilizacao)))

alienacao <- parsed_total |>
  dplyr::mutate(resumo = stringr::str_trunc(resumo, 32767))

ano <- parsed_total |>
  dplyr::count(ano)

assunto <- parsed_total |>
  dplyr::count(assunto, sort = TRUE)

classe <- parsed_total |>
  dplyr::count(classe, sort = TRUE)

foro <- parsed_total |>
  dplyr::count(foro, sort = TRUE)


writexl::write_xlsx(
  list(alienacao = alienacao, ano = ano, assunto = assunto, classe = classe, foro = foro),
  "data-raw/alienacao_parental/alienacao_parental.xlsx"
)

