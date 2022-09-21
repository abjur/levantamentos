# Rodrigo Garcia

# base rjsp ---------------------------------------------------------------
# da_rjsp <-
obsFase2::da_relatorio |>
  dplyr::glimpse()

# base fsp ----------------------------------------------------------------
# da_fsp <-
obsFase3::da_leilao_tidy |>
  dplyr::glimpse()

id <- googledrive::as_id("1eTjhSCh0xL8Lg3RjEAcC8rA4e_jHlMW8aPC4GElU46c")
sheets <- googlesheets4::sheet_names(id)
aux_glossario <- sheets  |>
  purrr::set_names() |>
  purrr::map(~googlesheets4::read_sheet(id, .x))

formatar_glossario <- function(da) {
  da |>
    dplyr::filter(incluir == "s") |>
    dplyr::select("Variável" = nm, "Descrição" = desc)
}

aux_glossario_format <- aux_glossario |>
  purrr::map(formatar_glossario)

knitr::kable(
  aux_glossario_format$processo,
  caption = "Glossário da base de processos."
)

knitr::kable(
  aux_glossario_format$avaliacao,
  caption = "Glossário da base de avaliacao"
)

knitr::kable(
  aux_glossario_format$leilao,
  caption = "Glossário da base de leilões"
)

knitr::kable(
  aux_glossario_format$parte,
  caption = "Glossário da base de partes"
)

# base rjsj ---------------------------------------------------------------
# da_rjrj <-
obsRJRJ::da_processo_tidy |>
  dplyr::glimpse()

# base rjrs ---------------------------------------------------------------
# da_rjrs <-
obsRJRS::da_processo_tidy |>
  dplyr::glimpse()


