
# preparação --------------------------------------------------------------

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

cores_abj <-  viridis::viridis(2, 1, .2, .8)

format_reais <- function(valor) {
  real <- paste("R$", format(valor, decimal.mark = ",", big.mark = ".", nsmall = 2))
  return(real)
}

# distribuicao por ano ----------------------------------------------------

processos |>
  dplyr::filter(ano_dist > 2008) |>
  dplyr::count(ano_dist) |>
  ggplot2::ggplot(ggplot2::aes(x = ano_dist, y = n, label = n)) +
  ggplot2::geom_col(fill = cores_abj[1]) +
  ggplot2::geom_label()+
  ggplot2::labs(
    x = "Ano de distribuição",
    y = "Quantidade de processos"
  )


# base com valores ---------------------------------------------------------

options(digits=10, scipen=999)
processos_valores <- processos |>
  dplyr::transmute(
    id_processo,
    assunto,
    classe,
    vara,
    valor = stringr::str_remove_all(valor_da_acao, stringr::regex("R\\$ |\\.", TRUE)),
    valor = stringr::str_replace(valor, "\\,", "\\."),
    valor = as.numeric(valor),
    valor_log = log10(valor)
  )


# plot valores gerais -----------------------------------------------------

media_valores <- mean(processos_valores$valor, na.rm = TRUE)
mediana_valores <- median(processos_valores$valor, na.rm = TRUE)

processos_valores |>
  ggplot2::ggplot(ggplot2::aes(x = valor)) +
  ggplot2::geom_histogram(fill = cores_abj[1], bins = 40) +
  ggplot2::geom_vline(xintercept = media_valores, color = "red", linetype = 2) +
  ggplot2::geom_text(ggplot2::aes(x=media_valores + 3400000, label=paste0("média\n", format_reais(media_valores)), y=500), colour="red") +
  ggplot2::geom_vline(xintercept = mediana_valores, color = "green", linetype = 2) +
  ggplot2::geom_text(ggplot2::aes(x=mediana_valores - 25000, label=paste0("mediana\n", format_reais(mediana_valores)), y=1050), colour="green") +
  ggplot2::labs(
    title = "Valor da causa (todos os processos)",
    x = "Valor da causa",
    y = "Quantidade de processos"
  ) +
  ggplot2::scale_x_log10(
    labels = scales::label_number_si(),
    breaks = 10^c(1:9)
  )


# plot categorias (5 categorias) ---------------------------------------------------------

processos_valores |>
  dplyr::mutate(
    valor_categorias = dplyr::case_when(
      valor < quantile(valor, 0.25, na.rm = TRUE) ~ "Até 10M",
      valor >= quantile(valor, 0.25, na.rm = TRUE) & valor < 30000 ~ "10M-30M",
      valor >= 30000 & valor < 100000 ~ "30M-100M",
      valor >= 100000 &  valor < 500000 ~ "100M-500M",
      valor >= 500000 ~ "Mais de 500M"
    ),
    valor_categorias = forcats::as_factor(valor_categorias),
    valor_categorias = forcats::lvls_reorder(
      valor_categorias,
      c(3, 1, 5, 2, 4) # 5 categorias
    ),
  ) |>
  dplyr::filter(!is.na(valor)) |>
  dplyr::count(valor_categorias) |>
  dplyr::mutate(
    top10 = valor_categorias == "Mais de 500M"
  ) |>
  ggplot2::ggplot() +
  ggplot2::aes(x = valor_categorias,
               y = n, label = n, fill = top10) +
  ggplot2::geom_col() +
  ggplot2::geom_label(fill = 'white') +
  ggplot2::labs(
    title = "Faixa de valores (todos os processos)",
    x = "Faixa de valor dos processos",
    y = "Quantidade de processos"
  ) +
  ggplot2::theme(
    legend.text = ggplot2::element_blank(),
    legend.title = ggplot2::element_blank(),
    legend.position = 'none'
  ) +
  ggplot2::scale_fill_manual(values = c(cores_abj[1], cores_abj[2]))



# plot categorias (7 categorias) ------------------------------------------------------------

processos_valores |>
  dplyr::mutate(
    valor_categorias = dplyr::case_when(
      valor < quantile(valor, 0.25, na.rm = TRUE) ~ "Até 10M",
      valor >= quantile(valor, 0.25, na.rm = TRUE) & valor < 30000 ~ "10M-30M",
      valor >= 30000 & valor < 100000 ~ "30M-100M",
      valor >= 100000 &  valor < 500000 ~ "100M-500M",
      valor >= 500000 &  valor < 1000000 ~ "500M-1MM",
      valor >= 1000000 &  valor < 10000000 ~ "1MM-10MM",
      valor >= 10000000 ~ "Mais de 10MM"
    ),
    valor_categorias = forcats::as_factor(valor_categorias),
    valor_categorias = forcats::lvls_reorder(
      valor_categorias,
      c(3, 1, 5, 2, 6, 4, 7) # 7 categorias
    )
  ) |>
  dplyr::filter(!is.na(valor)) |>
  dplyr::count(valor_categorias) |>
  dplyr::mutate(
    top10 = valor_categorias == "500M-1MM" |
        valor_categorias == "1MM-10MM" |
        valor_categorias == "Mais de 10MM"
  ) |>
  ggplot2::ggplot() +
  ggplot2::aes(x = valor_categorias,
               y = n, label = n, fill = top10) +
  ggplot2::geom_col() +
  ggplot2::geom_label() +
  ggplot2::labs(
    x = "Faixa de valor dos processos",
    y = "Quantidade de processos"
  ) +
  ggplot2::theme(
    legend.text = ggplot2::element_blank(),
    legend.title = ggplot2::element_blank(),
    legend.position = 'none'
  ) +
  ggplot2::scale_fill_manual(values = c(cores_abj[1], cores_abj[2]))

# maiores processos base -------------------------------------------------------

maiores_processos <- processos_valores |>
  dplyr::mutate(
    maiores_processos = valor >= quantile(valor, 0.90, na.rm = TRUE)
  ) |>
  dplyr::filter(maiores_processos)


# plot maiores processos (valor) ------------------------------------------

media_maiores_processos <- round(mean(maiores_processos$valor),2)
mediana_maiores_processos <- round(median(maiores_processos$valor),2)

maiores_processos |>
  ggplot2::ggplot(ggplot2::aes(x = valor)) +
  ggplot2::geom_histogram(fill = cores_abj[1], bins = 40) +
  ggplot2::geom_vline(xintercept = media_maiores_processos, color = "red", linetype = 2) +
  ggplot2::geom_text(ggplot2::aes(x=media_maiores_processos + 10000000, label=paste0("média\n", format_reais(media_maiores_processos)), y=80), colour="red") +
  ggplot2::geom_vline(xintercept = mediana_maiores_processos, color = "green", linetype = 2) +
  ggplot2::geom_text(ggplot2::aes(x=mediana_maiores_processos + 700000, label=paste0("mediana\n", format_reais(mediana_maiores_processos)), y=80), colour="green") +
  ggplot2::labs(
    title = "Valor da causa",
    subtitle = "10% processos mais com valores mais altos",
    x = "Valor da causa",
    y = "Quantidade de processos"
  ) +
  ggplot2::scale_x_log10(
    labels = scales::label_number_si()
  )


# plot maiores processos (assuntos) -------------------------------------------------------

maiores_processos |>
  dplyr::mutate(
    assunto = forcats::fct_infreq(assunto),
    assunto = forcats::fct_lump_n(assunto, 10, other_level = "Outros"),
    assunto = forcats::fct_rev(assunto)
  ) |>
  dplyr::count(assunto) |>
  ggplot2::ggplot() +
  ggplot2::aes(y = assunto, x = n) +
  ggplot2::geom_col(fill = cores_abj[1]) +
  ggplot2::labs(
    title = "10 assuntos mais frequentes, dentre os maiores processos",
    x = "Quantidade de processos",
    y = "Assunto"
  )

# plot maiores processos (classe) -------------------------------------------------------

maiores_processos |>
  dplyr::mutate(
    classe = forcats::fct_infreq(classe),
    classe = forcats::fct_lump_n(classe, 5, other_level = "Outros"),
    classe = forcats::fct_rev(classe)
  ) |>
  dplyr::count(classe) |>
  ggplot2::ggplot() +
  ggplot2::aes(y = classe, x = n) +
  ggplot2::geom_col(fill = cores_abj[1]) +
  ggplot2::labs(
    title = "5 classes mais frequentes, dentre os maiores processos",
    x = "Quantidade de processos",
    y = "Classe"
  )

# plot maiores processos (vara) -------------------------------------------------------

maiores_processos |>
  dplyr::mutate(
    vara = stringr::str_remove(vara, " - Foro Central Cível"),
    vara = forcats::fct_infreq(vara),
    vara = forcats::fct_rev(vara)
  ) |>
  dplyr::count(vara) |>
  ggplot2::ggplot() +
  ggplot2::aes(y = vara, x = n) +
  ggplot2::geom_col(fill = cores_abj[1]) +
  ggplot2::labs(
    title = "Varas em que tramitam os processos com valores mais altos",
    x = "Quantidade de processos",
    y = "Vara"
  )

