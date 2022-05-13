
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

# filtrar dados até fev/2020
processos_filtrados <- processos %>%
  dplyr::filter(distribuicao < "2020-03-01" | ano_dist < 2020)

cores_abj <-  viridis::viridis(2, 1, .2, .8)

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


# valor dos casos ---------------------------------------------------------

options(digits=10, scipen=999)
processos_valores <- processos |>
  dplyr::select(valor_da_acao) |>
  dplyr::mutate(
    valor = stringr::str_remove_all(valor_da_acao, stringr::regex("R\\$ |\\.", TRUE)),
    valor = stringr::str_replace(valor, "\\,", "\\."),
    valor = as.numeric(valor),
    valor_log = log10(valor)
  )

media_valores <- mean(processos_valores$valor, na.rm = TRUE)
mediana_valores <- median(processos_valores$valor, na.rm = TRUE)

processos_valores |>
  ggplot2::ggplot(ggplot2::aes(x = valor_log)) +
  ggplot2::geom_histogram(fill = cores_abj[1]) +
  ggplot2::geom_vline(xintercept = log10(media_valores), color = "red", linetype = 2) +
  ggplot2::geom_vline(xintercept = log10(mediana_valores), color = "green", linetype = 2) +
  ggplot2::labs(
    x = "Valor da causa\n(em log)",
    y = "Quantidade de processos"
  )

