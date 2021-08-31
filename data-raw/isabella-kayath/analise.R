library(magrittr)
# Importação ------------------------------------------------------------------------------------------------------------------------------------

subclasses_base_raw <- readxl::read_excel("~/Documents/levantamentos/data-raw/isabella-kayath/bases/Coleta de Dados - IC - Subclasses.xlsx")

# Função ----------------------------------------------------------------------------------------------------------------------------------------


limpa_strings <- function(str) {
  str |>
    abjutils::rm_accent() |>
    stringr::str_to_lower() |>
    stringr::str_squish()
}

# Manipulação -----------------------------------------------------------------------------------------------------------------------------------

subclasses_base <- subclasses_base_raw %>%
  janitor::clean_names() %>%
  dplyr::rename(n_processo = numero_do_processo,
    nome_recuperanda = nome_da_recuperanda,
    previsao_subclasse = previsao_de_subclasses_sim_nao,
    qual_classe = em_qual_classe_e_a_subclasse_i_ii_iii,
    subclasse_colaboradores = a_subclasse_e_de_credores_colaboradores_fornecedores_estrategicos,
    caracteristica_subclasse = caracteristicas_da_subclasses) %>%
  dplyr::mutate(dplyr::across(c(nome_recuperanda,
                              previsao_subclasse,
                              qual_classe,
                              subclasse_colaboradores,
                              caracteristica_subclasse), limpa_strings)) %>%
  dplyr::mutate(n_processo = abjutils::clean_cnj(n_processo)) %>%
  dplyr::mutate(qual_classe = dplyr::na_if(qual_classe, "n/a")) %>%
  dplyr::mutate(subclasse_colaboradores = dplyr::na_if(subclasse_colaboradores, "n/a")) %>%
  dplyr::mutate(subclasse_colaboradores = dplyr::na_if(subclasse_colaboradores, "na")) %>%
  dplyr::mutate(caracteristica_subclasse = dplyr::na_if(caracteristica_subclasse, "n/a")) %>%
  dplyr::mutate(previsao_subclasse = dplyr::na_if(previsao_subclasse, "n/a")) %>%
  dplyr::mutate(previsao_subclasse = dplyr::na_if(previsao_subclasse, "na"))

readr::write_rds(subclasses_base, "~/Documents/levantamentos/data-raw/isabella-kayath/bases/subclasses_base.rds")
