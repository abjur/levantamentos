# glossário geral
googlesheets4::gs4_auth("rfeliz@abj.org.br")

# glossario falencias -----------------------------------------------------
id_f <- googledrive::as_id("1eTjhSCh0xL8Lg3RjEAcC8rA4e_jHlMW8aPC4GElU46c")
sheets_f <- googlesheets4::sheet_names(id_f)
aux_glossario <- sheets_f |>
  purrr::set_names() |>
  purrr::map(~googlesheets4::read_sheet(id_f, .x))

formatar_glossario <- function(da) {
  da |>
    dplyr::filter(incluir == "s") |>
    dplyr::select("Variável" = nm, "Descrição" = desc)
}

aux_glossario_falencias <- aux_glossario |>
  purrr::map(formatar_glossario)

# glossario recuperações judiciais ----------------------------------------

id_rj <- googledrive::as_id("11bKkt9J5zfMeIYqyvrQHWNl6D3QK-I-H_dD5cp6YTYM")
sheets_rj <- googlesheets4::sheet_names(id_rj)

aux_glossario_rj <- sheets_rj |>
  purrr::set_names() |>
  purrr::map(~googlesheets4::read_sheet(id_rj, .x))


# glossarios falencias -------------------------------------------------------

link <- "https://docs.google.com/spreadsheets/d/1UOTeted5p70lAziWECmvFECQeB1EruEmuFofmdZ5R1g/edit?usp=sharing"

googlesheets4::write_sheet(
  aux_glossario_falencias$processo,
  link,
  "falencias_processo"
)
googlesheets4::write_sheet(
  aux_glossario_falencias$avaliacao,
  link,
  "falencias_avaliacao"
)
googlesheets4::write_sheet(
  aux_glossario_falencias$leilao,
  link,
  "falencias_leilao"
)
googlesheets4::write_sheet(
  aux_glossario_rj$glossario_pesquisa,
  link,
  "recuperacao_processo"
)
