library(tidyverse)

candidatos <- read_csv2("levantamentos/criminal-candidatos-g1/consulta_cand_2020_SP.csv",
                        locale = readr::locale(encoding = "latin1"))

# Prefeitos ---------------------------------------------------------------

cand_pref <- candidatos %>%
  filter(DS_CARGO == "PREFEITO") %>%
  select(NR_CPF_CANDIDATO, NM_CANDIDATO) %>%
  rename(cpf_candidato = NR_CPF_CANDIDATO)


prefeitos <- read_rds("/mnt/dados/abj/levantamentos/criminal-candidatos-g1/prefeitos_criminal.rds")
prefeitos %>% glimpse()

prefeitos_partes <- prefeitos %>%
  select(cpf_candidato, id_processo, partes) %>%
  left_join(cand_pref, "cpf_candidato") %>%
  unnest(partes)

prefeitos_partes %>% count(parte, sort = TRUE) %>% print(n = 100)

polo_passivo <- c("Reqd[oa]", "(Co)?Ré(u)?", "Querelad[oa]", "Indiciad[oa]", "Averiguad[oa]",
                  "Imptd[oa]", "Exectd[oa]", "Interpd[oa]", "Apeld[oa]", "Denunciad[oa]", "Excpt[oa]",
                  "Investigad[oa]", "Reclamad[oa]") %>%
  paste0(collapse = "|") %>% regex()
apenas_reu <- regex("(Co)?Ré(u)?")

# Comparar nomes
aprox <- function(nome, nome2) {
  nome <- abjutils::rm_accent(nome)
  nome2 <- abjutils::rm_accent(nome2)
  if (length(agrep(nome, nome2, ignore.case = TRUE, max.distance = 0.3)) == 0) {
    return(FALSE)
  } else {
    return(TRUE)
  }
}
nome_igual <- purrr::map2_lgl(prefeitos_partes$nome, prefeitos_partes$NM_CANDIDATO, aprox)

partes_filtrado <- prefeitos_partes %>%
  bind_cols(nome_igual = nome_igual) %>%
  filter(!is.na(nome), !str_detect(papel, "Advog"), nome_igual, str_detect(parte, polo_passivo))

ids_cpf_ok <- partes_filtrado %>%
  select(id_processo, cpf_candidato) %>%
  distinct()

# checar <- prefeitos_partes %>%
#   filter(!id_processo %in% ids_ok, str_detect(parte, polo_passivo), !str_detect(papel, "Advog"))
#
# aprox2 <- function(nome, nome2) {
#   nome <- abjutils::rm_accent(nome)
#   nome2 <- abjutils::rm_accent(nome2)
#   if (length(agrep(nome, nome2, ignore.case = TRUE, max.distance = 0.5)) == 0) {
#     return(FALSE)
#   } else {
#     return(TRUE)
#   }
# }
#
# checar_igual <- purrr::map2_lgl(checar$nome, checar$NM_CANDIDATO, aprox2)
#
# checar %>%
#   bind_cols(nome_igual = checar_igual) %>%
#   filter(!is.na(nome), nome_igual) %>%
#   View()

prefeitos %>%
  inner_join(ids_cpf_ok, c("cpf_candidato", "id_processo")) %>%
  write_rds("/mnt/dados/abj/levantamentos/criminal-candidatos-g1/prefeitos_criminal_passivo.rds")


# APENAS RÉUS

reus <- partes_filtrado %>%
  filter(str_detect(parte, apenas_reu)) %>%
  select(id_processo, cpf_candidato) %>%
  distinct()

prefeitos %>%
  inner_join(reus, c("cpf_candidato", "id_processo")) %>%
  write_rds("/mnt/dados/abj/levantamentos/criminal-candidatos-g1/prefeitos_criminal_reus.rds")

# Vice-prefeitos ----------------------------------------------------------

cand_vice <- candidatos %>%
  filter(DS_CARGO == "VICE-PREFEITO") %>%
  select(NR_CPF_CANDIDATO, NM_CANDIDATO) %>%
  rename(cpf_candidato = NR_CPF_CANDIDATO)

vice <- read_rds("/mnt/dados/abj/levantamentos/criminal-candidatos-g1/vice_prefeitos_criminal.rds")

vice_partes <- vice %>%
  select(cpf_candidato, id_processo, partes) %>%
  left_join(cand_vice, "cpf_candidato") %>%
  unnest(partes)

vice_partes %>% count(parte, sort = TRUE) %>% print(n = 100)
nome_igual <- purrr::map2_lgl(vice_partes$nome, vice_partes$NM_CANDIDATO, aprox)

partes_filtrado <- vice_partes %>%
  bind_cols(nome_igual = nome_igual) %>%
  filter(!is.na(nome), !str_detect(papel, "Advog"), nome_igual, str_detect(parte, polo_passivo))

ids_cpf_ok <- partes_filtrado %>%
  select(id_processo, cpf_candidato) %>%
  distinct()

# checar <- vice_partes %>%
#   filter(!id_processo %in% ids_ok, str_detect(parte, polo_passivo), !str_detect(papel, "Advog"))
#
# checar_igual <- purrr::map2_lgl(checar$nome, checar$NM_CANDIDATO, aprox2)
# checar %>%
#   bind_cols(nome_igual = checar_igual) %>%
#   filter(!is.na(nome), !str_detect(papel, "Advog"), nome_igual) %>%
#   View()

vice %>%
  inner_join(ids_cpf_ok, c("cpf_candidato", "id_processo")) %>%
  write_rds("/mnt/dados/abj/levantamentos/criminal-candidatos-g1/vice_prefeitos_criminal_passivo.rds")

# APENAS RÉUS

reus <- partes_filtrado %>%
  filter(str_detect(parte, apenas_reu)) %>%
  select(id_processo, cpf_candidato) %>%
  distinct()

vice %>%
  inner_join(reus, c("cpf_candidato", "id_processo")) %>%
  write_rds("/mnt/dados/abj/levantamentos/criminal-candidatos-g1/vice_prefeitos_criminal_reus.rds")

# Vereadores --------------------------------------------------------------

cand_vereador <- candidatos %>%
  filter(DS_CARGO == "VEREADOR") %>%
  select(NR_CPF_CANDIDATO, NM_CANDIDATO) %>%
  rename(cpf_candidato = NR_CPF_CANDIDATO)

vereadores <- read_rds("/mnt/dados/abj/levantamentos/criminal-candidatos-g1/vereadores_criminal.rds")

vereadores_partes <- vereadores %>%
  select(cpf_candidato, id_processo, partes) %>%
  left_join(cand_vereador, "cpf_candidato") %>%
  unnest(partes)

vereadores_partes %>% count(parte, sort = TRUE) %>% print(n = 100)

nome_igual <- purrr::map2_lgl(vereadores_partes$nome, vereadores_partes$NM_CANDIDATO, aprox)

partes_filtrado <- vereadores_partes %>%
  bind_cols(nome_igual = nome_igual) %>%
  filter(!is.na(nome), !str_detect(papel, "Advog"), nome_igual, str_detect(parte, polo_passivo))

ids_cpf_ok <- partes_filtrado %>%
  select(id_processo, cpf_candidato) %>%
  add_row(id_processo = "0002891-59.2012.8.26.0394", cpf_candidato = "96197021820") %>%
  distinct()

# checar <- vereadores_partes %>%
#   filter(!id_processo %in% ids_ok, str_detect(parte, polo_passivo), !str_detect(papel, "Advog"))
#
# checar_igual <- purrr::map2_lgl(checar$nome, checar$NM_CANDIDATO, aprox2)
# checar %>%
#   bind_cols(nome_igual = checar_igual) %>%
#   filter(!is.na(nome), !str_detect(papel, "Advog"), nome_igual) %>%
#   View()
#
# ids_ok <- c(ids_ok, "0002891-59.2012.8.26.0394")

vereadores %>%
  inner_join(ids_cpf_ok, c("cpf_candidato", "id_processo")) %>%
  write_rds("/mnt/dados/abj/levantamentos/criminal-candidatos-g1/vereadores_criminal_passivo.rds")

# APENAS RÉUS

reus <- partes_filtrado %>%
  filter(str_detect(parte, apenas_reu)) %>%
  select(id_processo, cpf_candidato) %>%
  distinct()

vereadores %>%
  inner_join(reus, c("cpf_candidato", "id_processo")) %>%
  write_rds("/mnt/dados/abj/levantamentos/criminal-candidatos-g1/vereadores_criminal_reus.rds")
