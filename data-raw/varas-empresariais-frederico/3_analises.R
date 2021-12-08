library(magrittr)

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

# filtrar dados até fev/2020
processos_filtrados <- processos %>%
  dplyr::filter(distribuicao < "2020-03-01" | ano_dist < 2020)

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

# Coluna nova com o tipo empresário
rx_tipoempresa <- c(
  "LTDA", "E\\.?\\s?P\\.?\\s?P\\.?\\s?", " M\\.?\\s?E\\.?\\s?(?!\\w)",
  " S\\.?\\s?A\\.?\\s?(?!\\w)", "EIRELI"
) %>%
  stringr::str_c(collapse = "|") %>%
  stringr::regex(ignore_case = TRUE)

aux_tipo_empresario <- processos_filtrados %>%
  tidyr::unnest(partes) %>%
  dplyr::select(id_processo, assunto, distribuicao, controle, id_parte, nome, parte, papel) %>%
  dplyr::mutate(tipo_empresario = stringr::str_extract_all(nome, rx_tipoempresa)) %>%
  dplyr::mutate(tipo_empresario = purrr::map_chr(tipo_empresario, stringr::str_c, collapse = "|")) %>%
  dplyr::mutate(tipo_empresario = toupper(stringr::str_remove_all(tipo_empresario, "[\\s\\.]"))) %>%
  dplyr::mutate(
    tipo_empresario = dplyr::na_if(tipo_empresario, ""),
    tipo_empresario = dplyr::case_when(
      tipo_empresario == "LTDA|LTDA" ~ "LTDA",
      tipo_empresario == "ME|ME" ~ "ME",
      tipo_empresario == "SA|SA" ~ "SA",
      TRUE ~ tipo_empresario
    )
  )


# Coluna nova com polo
unique(c(aux_tipo_empresario$papel, aux_tipo_empresario$parte))

ativo <- c("Reqte", "Exeqte", "Credor", "Embargte", "Exeqte")
passivo <- c("Embargdo", "Exectda", "Exectdo", "Reqda", "Reqdo", "Réu")

aux_tipo_empresario %>%
  dplyr::mutate(polo = dplyr::case_when(
    parte %in% ativo | papel %in% ativo ~ "ativo",
    parte %in% passivo | papel %in% passivo ~ "passivo"
  )) %>%
  dplyr::count(tipo_empresario, polo) %>%
  tidyr::pivot_wider(names_from = polo, values_from = n) %>%
  tidyr::replace_na(list(
    tipo_empresario = "Pessoa física ou tipo não identificado", ativo = 0,
    passivo = 0, `NA` = 0
  )) %>%
  purrr::set_names(
    "Tipo empresário", "Polo ativo", "Polo passivo", "Não identificado"
  ) %>%
  writexl::write_xlsx("data-raw/varas-empresariais-frederico/tipo_empresario_polo.xslsx")


# Gráficos e tabelas ------------------------------------------------------
cores_abj <-  viridis::viridis(2, 1, .2, .8)
blue_abj <- cores_abj[1]
pct <- function(x) {
  scales::percent(x, accuracy = .1)
}


# Assunto -----------------------------------------------------------------

t_assunto <- processos_filtrados %>%
  dplyr::filter(assunto != "NA") %>%
  dplyr::mutate(assunto = forcats::fct_lump_n(assunto, 9, other_level = "Outros")) %>%
  dplyr::count(assunto) %>%
  dplyr::arrange(n)
t_assunto %>%
  dplyr::mutate(prop = formattable::percent(n / sum(n))) %>%
  janitor::adorn_totals() %>%
  purrr::set_names("Assunto", "Quantidade", "%") %>%
  knitr::kable()
p_assunto <- t_assunto %>%
  dplyr::mutate(
    prop = n / sum(n),
    lab = glue::glue("{n} ({formattable::percent(prop, 1)})"),
    assunto = stringr::str_wrap(assunto, 20),
    assunto = forcats::fct_reorder(assunto, n)
  ) %>%
  ggplot2::ggplot(ggplot2::aes(x = n, y = assunto, label = lab)) +
  ggplot2::geom_col(fill = blue_abj) +
  ggplot2::geom_label(size = 2, position = ggplot2::position_stack(vjust = .5)) +
  ggplot2::labs(x = "Quantidade", y = "Assunto") +
  ggplot2::theme_minimal(12)
ggplot2::ggsave(
  "data-raw/varas-empresariais-frederico/plot_assunto.png",
  p_assunto, width = 7, height = 6
)

# Assunto ano -------------------------------------------------------------

t_assunto_ano <- processos_filtrados %>%
  dplyr::mutate(assunto = forcats::fct_lump(assunto, n = 19, other_level = "Outros"))  %>%
  dplyr::filter(!is.na(assunto)) %>%
  dplyr::count(assunto, ano_dist) %>%
  dplyr::arrange(desc(n))
tabela_assunto_ano <- t_assunto_ano %>%
  dplyr::mutate(ano_dist = as.character(ano_dist)) %>%
  dplyr::filter(!is.na(assunto)) %>%
  janitor::adorn_totals() %>%
  dplyr::mutate(prop = n / sum(n) * 2) %>%
  dplyr::mutate(prop = formattable::percent(prop)) %>%
  purrr::set_names("Assunto", "Ano", "Quantidade", "%")


# Assunto vara ------------------------------------------------------------

t_assunto_vara <- processos_filtrados %>%
  dplyr::mutate(assunto = forcats::fct_lump(assunto, n = 19, other_level = "Outros"))  %>%
  dplyr::filter(!is.na(assunto)) %>%
  dplyr::count(assunto, vara) %>%
  dplyr::arrange(desc(n))
tabela_assunto_vara <- t_assunto_vara %>%
  janitor::adorn_totals() %>%
  dplyr::mutate(prop = n / sum(n) * 2) %>%
  dplyr::mutate(prop = formattable::percent(prop)) %>%
  purrr::set_names("Assunto", "Vara", "Quantidade", "%")

# Vara --------------------------------------------------------------------

t_varas <- processos_filtrados %>%
  dplyr::count(vara) %>%
  dplyr::arrange(desc(n))
tabela_varas <- t_varas %>%
  janitor::adorn_totals() %>%
  dplyr::mutate(prop = n / sum(n) * 2) %>%
  dplyr::mutate(prop = formattable::percent(prop)) %>%
  purrr::set_names("Vara", "Quantidade", "%")
p_vara <- t_varas %>%
  dplyr::mutate(
    vara = stringr::str_wrap(vara, 20),
    vara = forcats::fct_reorder(vara, n),
    prop = n/sum(n),
    lab = glue::glue("{n} ({pct(prop)})")
  ) %>%
  ggplot2::ggplot(ggplot2::aes(x = n, y = vara, label = lab)) +
  ggplot2::geom_col(fill = blue_abj) +
  ggplot2::geom_label(size = 4, position = ggplot2::position_stack(vjust = .5)) +
  ggplot2::theme_minimal(14) +
  ggplot2::labs(
    x = "Quantidade",
    y = "Vara"
  )
ggplot2::ggsave(
  "data-raw/varas-empresariais-frederico/plot_vara.png",
  p_vara, width = 12, height = 6
)

# Valor ----------------------------------------------------------
t_valor <- processos_filtrados %>%
  dplyr::select(id_processo, valor = valor_da_acao) %>%
  dplyr::filter(!is.na(valor)) %>%
  dplyr::mutate(
    valor = stringr::str_remove_all(valor, "R\\$ "),
    valor = stringr::str_remove_all(valor, "\\."),
    valor = stringr::str_replace_all(valor, "\\,", "."),
    valor = as.double(valor)
  )
p_valor <- t_valor %>%
  ggplot2::ggplot(ggplot2::aes(x = valor)) +
  ggplot2::geom_histogram(bins = 50, fill = cores_abj[1]) +
  ggplot2::scale_x_log10(labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  ggplot2::geom_vline(ggplot2::aes(xintercept = median(valor)),col='red',size=.3, linetype = 2) +
  ggplot2::geom_text(ggplot2::aes(x=median(valor)+ 4*1e4, label=paste0("mediana\n", median(valor)), y=500), colour="red") +
  ggplot2::theme_minimal(14)
ggplot2::ggsave(
  "data-raw/varas-empresariais-frederico/plot_valor_da_acao.png",
  p_valor, width = 12, height = 6
)

# Valor assunto -----------------------------------------------------------

t_valor_assunto <- processos_filtrados %>%
  dplyr::select(id_processo, assunto, valor = valor_da_acao) %>%
  dplyr::filter(!is.na(valor)) %>%
  dplyr::mutate(
    valor = stringr::str_remove_all(valor, "R\\$ "),
    valor = stringr::str_remove_all(valor, "\\."),
    valor = stringr::str_replace_all(valor, "\\,", "."),
    valor = as.double(valor)
  ) %>%
  dplyr::group_by(assunto) %>%
  dplyr::summarise(mediana = median(valor)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(assunto = dplyr::case_when(
    mediana < 200000.00 ~ "Outros",
    TRUE ~ assunto
  )) %>%
  dplyr::group_by(assunto) %>%
  dplyr::summarise(mediana = sum(mediana)) %>%
  dplyr::arrange(desc(mediana))


# Valor vara --------------------------------------------------------------

t_valor_vara <- processos_filtrados %>%
  dplyr::select(id_processo, vara, valor = valor_da_acao) %>%
  dplyr::filter(!is.na(valor)) %>%
  dplyr::mutate(
    valor = stringr::str_remove_all(valor, "R\\$ "),
    valor = stringr::str_remove_all(valor, "\\."),
    valor = stringr::str_replace_all(valor, "\\,", "."),
    valor = as.double(valor)
  ) %>%
  dplyr::group_by(vara) %>%
  dplyr::summarise(mediana = median(valor)) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(desc(mediana))


# Valor assunto vara ------------------------------------------------------
t_valor_assunto_vara <- processos_filtrados %>%
  dplyr::select(id_processo, assunto, vara, valor = valor_da_acao) %>%
  dplyr::filter(!is.na(valor)) %>%
  dplyr::mutate(
    valor = stringr::str_remove_all(valor, "R\\$ "),
    valor = stringr::str_remove_all(valor, "\\."),
    valor = stringr::str_replace_all(valor, "\\,", "."),
    valor = as.double(valor)
  ) %>%
  dplyr::group_by(assunto, vara) %>%
  dplyr::summarise(mediana = median(valor)) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(desc(mediana))

# Tipo empresário ---------------------------------------------------------

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

writexl::write_xlsx(
  list(
    "classes primeiro grau" = classe_1grau,
    "classes segundo grau" = classe_2grau,
    "tipo empresario" = tabela_tempresario,
    "varas" = tabela_varas
  ),
  "data-raw/varas-empresariais-frederico/tabelas_varas_empresariais.xlsx"
)


# Tipo empresário com polo ------------------------------------------------
unique(c(aux_tipo_empresario$papel, aux_tipo_empresario$parte))

ativo <- c("Reqte", "Exeqte", "Credor", "Embargte", "Exeqte")
passivo <- c("Embargdo", "Exectda", "Exectdo", "Reqda", "Reqdo", "Réu")

p_tipo_empresario_polo <- aux_tipo_empresario %>%
  dplyr::mutate(polo = dplyr::case_when(
    parte %in% ativo | papel %in% ativo ~ "ativo",
    parte %in% passivo | papel %in% passivo ~ "passivo"
  )) %>%
  dplyr::filter(!is.na(tipo_empresario), !is.na(polo)) %>%
  dplyr::mutate(tipo_empresario = forcats::fct_lump_n(tipo_empresario, 5, other_level = "Outros")) %>%
  dplyr::count(tipo_empresario, polo) %>%
  dplyr::group_by(tipo_empresario) %>%
  dplyr::summarise(polo = polo,
                   n = n,
                   n_rel = n/sum(n)) %>%
  dplyr::mutate(pct = scales::percent(n/sum(n))) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(tipo_empresario = forcats::fct_reorder(tipo_empresario, n)) %>%
  ggplot2::ggplot(ggplot2::aes(y = tipo_empresario, x = n_rel, fill = polo, label = n)) +
  ggplot2::geom_col() +
  ggplot2::scale_fill_manual(values = cores_abj) +
  ggplot2::geom_label(size = 3, position = ggplot2::position_stack(vjust = .5)) +
  ggplot2::theme_minimal(14) +
  ggplot2::labs(
    x = "Partes",
    y = "Tipo empresário"
  )
ggplot2::ggsave(
  "data-raw/varas-empresariais-frederico/plot_tipo_empresario_polo.png",
  p_tipo_empresario_polo, width = 5, height = 3
)

# Tipo empresário com polo e assunto ------------------------------------------------
unique(c(aux_tipo_empresario$papel, aux_tipo_empresario$parte))

ativo <- c("Reqte", "Exeqte", "Credor", "Embargte", "Exeqte")
passivo <- c("Embargdo", "Exectda", "Exectdo", "Reqda", "Reqdo", "Réu")

p_tipo_empresario_polo_assunto <- aux_tipo_empresario %>%
  dplyr::mutate(polo = dplyr::case_when(
    parte %in% ativo | papel %in% ativo ~ "ativo",
    parte %in% passivo | papel %in% passivo ~ "passivo"
  )) %>%
  dplyr::filter(!is.na(tipo_empresario), !is.na(polo), !is.na(assunto)) %>%
  dplyr::mutate(tipo_empresario = forcats::fct_lump_n(tipo_empresario, 5, other_level = "Outros")) %>%
  dplyr::mutate(assunto = forcats::fct_lump_n(assunto, 9, other_level = "Outros")) %>%
  dplyr::count(tipo_empresario, assunto, polo) %>%
  dplyr::group_by(tipo_empresario, assunto) %>%
  dplyr::summarise(polo = polo,
                   assunto = assunto,
                   n = n,
                   n_rel = n/sum(n)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(pct = scales::percent(n/sum(n))) %>%
  dplyr::mutate(tipo_empresario = forcats::fct_reorder(tipo_empresario, n)) %>%
  ggplot2::ggplot(ggplot2::aes(y = tipo_empresario, x = n_rel, fill = polo, label = n)) +
  ggplot2::geom_col() +
  ggplot2::scale_fill_manual(values = cores_abj) +
  ggplot2::facet_wrap(.~assunto, labeller = ggplot2::label_wrap_gen()) +
  ggplot2::geom_label(size = 3, position = ggplot2::position_stack(vjust = .5)) +
  ggplot2::theme_minimal(14) +
  ggplot2::labs(
    x = "Partes",
    y = "Tipo empresário"
  )
ggplot2::ggsave(
  "data-raw/varas-empresariais-frederico/plot_tipo_empresario_polo_assunto.png",
  p_tipo_empresario_polo_assunto, width = 15, height = 10
)

# Tempo -------------------------------------------------------------------
extincao <- processos_filtrados |>
  dplyr::select(-data) |>
  tidyr::unnest(movimentacoes) |>
  dplyr::mutate(data = lubridate::dmy(data)) |>
  dplyr::group_by(id_processo) |>
  dplyr::filter(stringr::str_detect(movimento, "Arquivad")) |>
  dplyr::arrange(desc(data)) |>
  dplyr::slice(1) |>
  dplyr::ungroup() |>
  dplyr::select(id_processo, data_extincao = data)

distribuicao <- processos_filtrados |>
  dplyr::select(-data) |>
  tidyr::unnest(movimentacoes) |>
  dplyr::mutate(data = lubridate::dmy(data)) |>
  dplyr::group_by(id_processo) |>
  dplyr::arrange(data) |>
  dplyr::slice(1) |>
  dplyr::ungroup() |>
  dplyr::select(id_processo, data_distribuicao = data)

da_tempo <- processos_filtrados |>
  dplyr::left_join(extincao) |>
  dplyr::left_join(distribuicao) |>
  dplyr::mutate(duracao = data_extincao - data_distribuicao,
                duracao = as.integer(duracao)) |>
  dplyr::select(id_processo, assunto, vara, duracao, data_extincao, data_distribuicao)

p_tempo <- da_tempo |>
  dplyr::filter(!is.na(duracao)) |>
  ggplot2::ggplot(ggplot2::aes(x = duracao)) +
  ggplot2::geom_histogram(fill = cores_abj[1], bins = 60) +
  ggplot2::geom_vline(ggplot2::aes(xintercept = mean(duracao)),col='red',size=.7, linetype = 2) +
  ggplot2::geom_text(ggplot2::aes(x=median(duracao)+17, label=paste0("mediana:\n", median(duracao), " dias"), y=35), colour="red") +
  ggplot2::labs(x = "Duração até extinção") +
  ggplot2::theme_minimal(14)

ggplot2::ggsave(
  "data-raw/varas-empresariais-frederico/plot_tempo.png",
  p_tempo, width = 7, height = 5
)

# Tempo assunto -------------------------------------------------------------------

assuntos <- da_tempo |>
  dplyr::filter(!is.na(duracao)) |>
  dplyr::group_by(assunto) |>
  dplyr::summarise(duracao_media = mean(duracao)) |>
  dplyr::ungroup() |>
  dplyr::arrange(desc(duracao_media)) |>
  utils::head(19) |>
  dplyr::select(assunto)

t_tempo_assunto_sem_outros <- da_tempo |>
  dplyr::filter(!is.na(duracao)) |>
  dplyr::group_by(assunto) |>
  dplyr::summarise(duracao_media = mean(duracao),
                   n_obs = dplyr::n()) |>
  dplyr::ungroup() |>
  dplyr::mutate(assunto = forcats::fct_reorder(assunto, duracao_media))

t_tempo_assunto_com_outros <- da_tempo |>
  dplyr::filter(!is.na(duracao)) |>
  dplyr::mutate(assunto = dplyr::case_when(
    !(assunto %in% assuntos$assunto) ~ "Outros",
    TRUE ~ assunto
  )) |>
  dplyr::group_by(assunto) |>
  dplyr::summarise(duracao_media = mean(duracao),
                   n_obs = dplyr::n()) |>
  dplyr::ungroup() |>
  dplyr::mutate(assunto = stringr::str_to_lower(assunto),
                assunto = stringr::str_replace(assunto, "\\/", "\\/ \\\n"),
                assunto = forcats::fct_reorder(assunto, duracao_media))

media_tempo <- mean(da_tempo$duracao, na.rm = TRUE)

p_tempo_assunto <- t_tempo_assunto_com_outros |>
  ggplot2::ggplot(ggplot2::aes(x = duracao_media, y = assunto)) +
  ggplot2::geom_col(fill = cores_abj[1]) +
  ggplot2::geom_label(
    ggplot2::aes(
      label = paste0(round(duracao_media), " dias (", n_obs, " processos)")
    ),
    size = 3, position = ggplot2::position_stack(vjust = .5),
    fill = cores_abj[2]
  ) +
  ggplot2::geom_vline(
    ggplot2::aes(xintercept = media_tempo), col='red', size=.7, linetype = 2
  ) +
  ggplot2::geom_text(
    ggplot2::aes(x = media_tempo + 25, y = "sustação de protesto"),
    label = paste0("média:\n", round(media_tempo), " dias"),
    lineheight = .8, size = 3.5, colour = "red"
  )
ggplot2::ggsave(
  "data-raw/varas-empresariais-frederico/plot_tempo_assunto.png",
  p_tempo_assunto, width = 7, height = 5
)

# Tempo assunto vara* -------------------------------------------------------------------
t_tempo_assunto_vara_sem_outros <- da_tempo |>
  dplyr::filter(!is.na(duracao)) |>
  dplyr::group_by(assunto, vara) |>
  dplyr::summarise(duracao_media = mean(duracao),
                   n_obs = dplyr::n()) |>
  dplyr::ungroup() |>
  dplyr::mutate(assunto = forcats::fct_reorder(assunto, duracao_media))

t_tempo_assunto_vara_com_outros <- da_tempo |>
  dplyr::filter(!is.na(duracao)) |>
  dplyr::mutate(assunto = dplyr::case_when(
    !(assunto %in% assuntos$assunto) ~ "Outros",
    TRUE ~ assunto
  )) |>
  dplyr::group_by(vara) |>
  dplyr::mutate(duracao_media_vara = mean(duracao)) |>
  dplyr::ungroup() |>
  dplyr::group_by(assunto, vara) |>
  dplyr::summarise(duracao_media = mean(duracao),
                   n_obs = dplyr::n(),
                   duracao_media_vara = dplyr::first(duracao_media_vara)) |>
  dplyr::mutate(assunto = stringr::str_to_lower(assunto),
                assunto = stringr::str_replace(assunto, "\\/", "\\/ \\\n"),
                duracao_media = round(duracao_media, digits=2),
                assunto = forcats::fct_reorder(assunto, duracao_media)) |>
  dplyr::ungroup()

p_tempo_assunto_vara <- t_tempo_assunto_vara_com_outros |>
  dplyr::arrange(desc(n_obs), duracao_media) |>
  dplyr::mutate(assunto = forcats::fct_inorder(assunto)) |>
  ggplot2::ggplot(ggplot2::aes(x = duracao_media, y = assunto)) +
  ggplot2::geom_col(fill = cores_abj[1]) +
  ggplot2::geom_label(ggplot2::aes(label = paste0(round(duracao_media), " dias (", n_obs, " processos)")), size = 3, position = ggplot2::position_stack(vjust = .5), fill = cores_abj[2]) +
  ggplot2::facet_grid(vara~., scales = "free", space = "free", labeller = ggplot2::label_wrap_gen()) +
  ggplot2::geom_vline(ggplot2::aes(xintercept = duracao_media_vara), col='red',size=.7, linetype = 2) +
  ggplot2::geom_text(ggplot2::aes(x=duracao_media_vara+10, label=paste0("média:\n", round(duracao_media_vara), " dias"), y = assunto[1]),lineheight = .8,size=3.5, colour="red") +
  ggplot2::theme_minimal(14)

ggplot2::ggsave(
  "data-raw/varas-empresariais-frederico/plot_tempo_assunto_vara.png",
  p_tempo_assunto_vara, width = , height = 7
)

# Gráficos ----------------------------------------------------------------

# Ano/mês
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
    size = 2,
    position = ggplot2::position_stack(vjust = .5)
  ) +
  ggplot2::labs(x = "Quantidade", y = "Mês") +
  ggplot2::theme_minimal(14)

ggplot2::ggsave(
  "data-raw/varas-empresariais-frederico/plot_mes_ano.png",
  p_mes_ano, width = 7, height = 5
)
