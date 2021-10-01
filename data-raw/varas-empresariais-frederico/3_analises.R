library(magrittr)

# taxa de recorribilidade e reforma -------------------------------------------

processos <- "data-raw/varas-empresariais-frederico/da_completa.rds" %>%
  readr::read_rds() %>%
  dplyr::mutate(
    id_processo = abjutils::clean_cnj(id_processo),
    distribuicao = dplyr::coalesce(distribuicao, recebido_em),
    distribuicao = stringr::str_extract(distribuicao, "[0-9]{2}/[0-9]{2}/[0-9]{4}"),
    distribuicao = lubridate::dmy(distribuicao),
    ano_dist = dplyr::case_when(
      !is.na(distribuicao) ~ lubridate::year(distribuicao),
      is.na(distribuicao) ~ as.numeric(stringr::str_extract(
        id_processo, "(?<=[0-9]{9})[0-9]{4}"
      ))
    )
  )
dplyr::count(processos, ano_dist)

# filtrar dados até fev/2020
processos_filtrados <- processos %>%
  dplyr::filter(distribuicao < "2020-03-01" | ano_dist < 2020)

da_cposg_raw <- readr::read_rds("data-raw/varas-empresariais-frederico/da_cposg.rds")

da_cposg <- da_cposg_raw %>%
  dplyr::filter(classe %in% c("Apelação Cível")) %>%
  dplyr::transmute(
    id_processo = basename(tools::file_path_sans_ext(file)),
    id_processo2 = abjutils::clean_cnj(processo),
    id_original = abjutils::clean_cnj(id_original),
    classe,
    status_2g = status,
    decisoes_2g = decisoes,
    partes_2g = partes
  )

classe_1grau <- processos_filtrados %>%
  dplyr::filter(!stringr::str_detect(classe, "Cumprimento|Liquida")) %>%
  dplyr::count(classe, sort = TRUE) %>%
  dplyr::rename(classe_1grau = classe) %>%
  dplyr::mutate(pct = n/sum(n)) %>%
  janitor::adorn_totals()

processos_filtrados %>%
  dplyr::mutate(
    id_processo = abjutils::clean_cnj(id_processo),
    segundo_grau = dplyr::case_when(
      id_processo %in% da_cposg$id_processo |
        id_processo %in% da_cposg$id_original ~ TRUE,
      TRUE ~ FALSE
    )
  ) %>%
  dplyr::count(segundo_grau)

# tirar ids fora do escopo temporal
ids_tirar <- da_cposg %>%
  dplyr::anti_join(
    processos_filtrados %>% dplyr::transmute(id_processo = abjutils::clean_cnj(id_processo)),
    "id_processo"
  ) %>%
  dplyr::pull(id_processo)

da_cposg <- da_cposg %>%
  dplyr::filter(!id_processo %in% ids_tirar)

# Classes no segundo grau
classe_2grau <- da_cposg %>%
  dplyr::filter(classe == "Apelação Cível") %>%
  dplyr::inner_join(processos_filtrados, "id_processo") %>%
  dplyr::count(classe.y, sort = TRUE) %>%
  dplyr::rename(classe_2grau = classe.y) %>%
  dplyr::mutate(pct = n/sum(n)) %>%
  janitor::adorn_totals()

nrow(da_cposg)/nrow(processos_filtrados %>% dplyr::filter(!stringr::str_detect(classe, "Cumprimento|Liquida")))

# reforma -----------------------------------------------------------------

rx_deram <- stringr::regex("deram provimento|anula", TRUE)
rx_conhec <- stringr::regex("conhec|prejud", TRUE)
rx_parcial <- stringr::regex("parcial", TRUE)
rx_negaram <- stringr::regex("negaram|não provido", TRUE)

aux_decisao <- da_cposg %>%
  tidyr::unnest(decisoes_2g) %>%
  dplyr::arrange(dplyr::desc(data)) %>%
  dplyr::filter(!stringr::str_detect(decisao, "embarg|design")) %>%
  dplyr::distinct(id_processo, .keep_all = TRUE) %>%
  dplyr::mutate(desfecho = dplyr::case_when(
    stringr::str_detect(decisao, rx_negaram) ~ "Não reformou",
    stringr::str_detect(decisao, rx_parcial) ~ "Parcial",
    stringr::str_detect(decisao, rx_deram) ~ "Reformou",
    stringr::str_detect(decisao, rx_conhec) ~ "Não conheceram/Prejudicado",
    TRUE ~ "Outros"
  ))

# resultados finais
aux_decisao %>%
  dplyr::count(desfecho, sort = TRUE) %>%
  dplyr::mutate(prop = formattable::percent(n/sum(n))) %>%
  janitor::adorn_totals() %>%
  knitr::kable()

# Contagens de processos (contagens por mês, ano, assunto)

# Contagem de assuntos:
processos_filtrados %>%
  dplyr::filter(assunto != "NA") %>%
  dplyr::mutate(assunto = forcats::fct_lump_n(assunto, 9, other_level = "Outros")) %>%
  dplyr::count(assunto) %>%
  dplyr::arrange(n) %>%
  dplyr::mutate(prop = formattable::percent(n / sum(n))) %>%
  janitor::adorn_totals() %>%
  purrr::set_names("Assunto", "Quantidade", "%") %>%
  knitr::kable()



# Gŕafico de contagem por ano e mês
cores_abj <-  viridis::viridis(2, 1, .2, .8)
blue_abj <- cores_abj[1]
pct <- function(x) {
  scales::percent(x, accuracy = .1)
}

aux_contagem <- processos_filtrados %>%
  dplyr::mutate(mes = lubridate::month(distribuicao, label = TRUE, locale = "pt_BR.UTF-8")) %>%
  dplyr::mutate(ano = ano_dist)

p_mes_ano <- aux_contagem %>%
  dplyr::filter(mes != "NA") %>%
  dplyr::group_by(ano) %>%
  dplyr::count(mes) %>%
  ggplot2::ggplot() +
  ggplot2::aes(x = n, y = forcats::fct_rev(mes), label = n) +
  ggplot2::geom_col(fill = blue_abj) +
  ggplot2::facet_wrap(~ ano, scales = "free_x") +
  ggplot2::geom_label(
    ggplot2::aes(x = n),
    size = 3,
    position = ggplot2::position_stack(vjust = .5)
  ) +
  ggplot2::labs(x = "Quantidade", y = "Mês") +
  ggplot2::theme_minimal(14)

ggplot2::ggsave(
  "data-raw/varas-empresariais-frederico/plot_mes_ano.png",
  p_mes_ano, width = 7, height = 5
)

# Coluna nova com o tipo empresário
rx_tipoempresa <- c(
  "LTDA", "E\\.?\\s?P\\.?\\s?P\\.?\\s?", " M\\.?\\s?E\\.?\\s?(?!\\w)",
  " S\\.?\\s?A\\.?\\s?(?!\\w)", "EIRELI"
) %>%
  stringr::str_c(collapse = "|") %>%
  stringr::regex(ignore_case = TRUE)

aux_tipo_empresario <- processos_filtrados %>%
  tidyr::unnest(partes) %>%
  dplyr::select(id_processo, distribuicao, controle, id_parte, nome, parte, papel) %>%
  dplyr::mutate(tipo_empresario = stringr::str_extract_all(nome, rx_tipoempresa)) %>%
  dplyr::mutate(tipo_empresario = purrr::map_chr(tipo_empresario, stringr::str_c, collapse = "|")) %>%
  dplyr::mutate(tipo_empresario = toupper(stringr::str_remove_all(tipo_empresario, "[\\s\\.]"))) %>%
  dplyr::mutate(tipo_empresario = dplyr::na_if(tipo_empresario, ""))

p_tipo_empresario <- aux_tipo_empresario %>%
  dplyr::filter(!is.na(tipo_empresario)) %>%
  dplyr::mutate(tipo_empresario = forcats::fct_lump_n(tipo_empresario, 5, other_level = "Outros")) %>%
  dplyr::count(tipo_empresario) %>%
  dplyr::mutate(pct = scales::percent(n/sum(n))) %>%
  dplyr::mutate(tipo_empresario = forcats::fct_reorder(tipo_empresario, n)) %>%
  ggplot2::ggplot(ggplot2::aes(y = tipo_empresario, x = n, label = pct)) +
  ggplot2::geom_col(fill = blue_abj) +
  ggplot2::geom_label(size = 3, position = ggplot2::position_stack(vjust = .5)) +
  ggplot2::theme_minimal(14) +
  ggplot2::labs(
    x = "Partes",
    y = "Tipo empresário"
  )

ggplot2::ggsave(
  "data-raw/varas-empresariais-frederico/plot_tipo_empresario.png",
  p_tipo_empresario, width = 5, height = 3
)

# Tabelas com a porcentagem e quantidade de processos por Tipo empresario. (complementa o gráfico de cima)
tabela_tempresario <-  aux_tipo_empresario %>%
  dplyr::filter(!is.na(tipo_empresario)) %>%
  dplyr::count(tipo_empresario) %>%
  dplyr::arrange(desc(n)) %>%
  janitor::adorn_totals() %>%
  dplyr::mutate(prop = n / sum(n) * 2) %>%
  dplyr::mutate(prop = formattable::percent(prop)) %>%
  purrr::set_names("Tipo empresário", "Quantidade", "%")
  #knitr::kable(caption = "Tipos empresarios")

# Tabelas com a porcentagem e quantidade de processos por Vara.
tabela_varas <-  processos_filtrados %>%
  dplyr::count(vara) %>%
  dplyr::arrange(desc(n)) %>%
  janitor::adorn_totals() %>%
  dplyr::mutate(prop = n / sum(n) * 2) %>%
  dplyr::mutate(prop = formattable::percent(prop)) %>%
  purrr::set_names("Vara", "Quantidade", "%")
  #knitr::kable(caption = "Quantidade de processos por Vara")

writexl::write_xlsx(
  list(
    "classes primeiro grau" = classe_1grau,
    "classes segundo grau" = classe_2grau,
    "tipo empresario" = tabela_tempresario,
    "varas" = tabela_varas
  ),
  "data-raw/varas-empresariais-frederico/tabelas_varas_empresariais.xlsx"
)
