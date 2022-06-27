# preparacao --------------------------------------------------------------
da <- readr::read_rds("data-raw/bruno-nassar-puc/data/homicidios_tidy.rds")
cores_abj <-  viridis::viridis(2, 1, .2, .8)

grafico_base <- function(da, var) {
  cores_abj <-  viridis::viridis(2, 1, .2, .8)

  da |>
    dplyr::filter(!is.na({{var}})) |>
    dplyr::count({{var}}) |>
    dplyr::mutate(
      prop = n/sum(n),
      perc = formattable::percent(prop)
    ) |>
    ggplot2::ggplot() +
    ggplot2::aes(x = {{var}}, y = n, label = perc) +
    ggplot2::geom_col(fill = cores_abj[1]) +
    ggplot2::geom_label()
}

fs::dir_create("data-raw/bruno-nassar-puc/img")

# 1 – Coluna C: qual a distribuição de processos entre as varas? =========================================================================
p01 <- da |>
  dplyr::mutate(
    ano_dist = lubridate::year(dt_dist)
  ) |>
  dplyr::count(ano_dist) |>
  ggplot2::ggplot() +
  ggplot2::aes(x = ano_dist, y = n, label = n) +
  ggplot2::geom_col(fill = cores_abj[1]) +
  ggplot2::geom_label() +
  ggplot2::scale_x_continuous(breaks = c(1993, 2005, 2008:2019)) +
  ggplot2::labs(
    title = "Distribuição dos processos ao longo dos anos",
    x = "Ano de distribuição",
    y = "Quantidade de processos"
  ) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust=1))

ggplot2::ggsave(
  "data-raw/bruno-nassar-puc/img/p01.png",
  p01, width = 10, height = 4
)
# 2 – Coluna D: qual a distribuição de processos eletrônicos e físicos? =========================================================================
p02 <- da |>
  grafico_base(digital) +
  ggplot2::labs(
    title = "Distribuição de processos eletrônicos e físicos",
    x = "O processo é digital?",
    y = "Quantidade de processos"
  )

ggplot2::ggsave(
  "data-raw/bruno-nassar-puc/img/p02.png",
  p02, width = 7, height = 5
)
# 3 – Coluna E: qual a temática de cada processo. =========================================================================
p03 <- da |>
  dplyr::mutate(homicidio = forcats::fct_infreq(homicidio)) |>
  grafico_base(homicidio) +
  ggplot2::labs(
    title = "Processos relacionados a homicídios",
    x = "Trata-se de processo de homicídio?",
    y = "Quantidade de processos"
  )

ggplot2::ggsave(
  "data-raw/bruno-nassar-puc/img/p03.png",
  p03, width = 7, height = 5
)
# 4 – Coluna H: qual a distribuição de horários do fato? =========================================================================
p04 <- da |>
  dplyr::filter(!is.na(parte_do_dia_fato)) |>
  dplyr::count(parte_do_dia_fato) |>
  dplyr::mutate(
    parte_do_dia_fato = factor(parte_do_dia_fato, levels = c("Madrugada (00:01 às 06:00)", "Manhã (06:01 às 12:00)", "Tarde (12:01 às 18:00)", "Noite (18:01 às 00:00)", "Não consta")),
    prop = n/sum(n),
    perc = formattable::percent(prop),
    col_dif = parte_do_dia_fato == "Não consta"
  ) |>
  ggplot2::ggplot() +
  ggplot2::aes(x = parte_do_dia_fato, y = n, label = perc) +
  ggplot2::geom_col(ggplot2::aes(fill = col_dif), show.legend = FALSE) +
  ggplot2::geom_label() +
  ggplot2::scale_fill_manual(values = c(cores_abj[1], "gray70")) +
  ggplot2::labs(
    title = "Período do dia em que aconteceu o fato",
    x = "Período do dia",
    y = "Quantidade de processos"
  )

ggplot2::ggsave(
  "data-raw/bruno-nassar-puc/img/p04.png",
  p04, width = 10, height = 5
)

# 5 – Coluna O: quantos processos foram suspensos com base no art. 366. =========================================================================
p05 <- da |>
  dplyr::filter(!is.na(suspensao)) |>
  dplyr::count(suspensao) |>
  dplyr::mutate(
    suspensao = factor(suspensao, levels = c("Não", "Sim", "Não consta")),
    prop = n/sum(n),
    perc = formattable::percent(prop),
    col_dif = suspensao == "Não consta"
  ) |>
  ggplot2::ggplot() +
  ggplot2::aes(x = suspensao, y = n, label = perc) +
  ggplot2::geom_col(ggplot2::aes(fill = col_dif), show.legend = FALSE) +
  ggplot2::geom_label() +
  ggplot2::scale_fill_manual(values = c(cores_abj[1], "gray70")) +
  ggplot2::labs(
    title = "Suspensões com base no art. 366",
    x = "O processo chegou a ser suspenso com base no art. 366?",
    y = "Quantidade de processos"
  )

ggplot2::ggsave(
  "data-raw/bruno-nassar-puc/img/p05.png",
  p05, width = 7, height = 5
)

# 6 – Coluna U: quantos casos tiveram concurso com menor de idade? =========================================================================
p06 <- da |>
  dplyr::filter(!is.na(concurso_menor)) |>
  dplyr::count(concurso_menor) |>
  dplyr::mutate(
    prop = n / sum(n),
    perc = formattable::percent(prop),
    col_dif = concurso_menor == "Não consta"
  ) |>
  dplyr::arrange(desc(n)) |>
  dplyr::mutate(concurso_menor = forcats::fct_inorder(concurso_menor)) |>
  ggplot2::ggplot() +
  ggplot2::aes(x = concurso_menor, y = n, label = glue::glue("{n} casos\n({perc})")) +
  ggplot2::geom_col(ggplot2::aes(fill = col_dif), show.legend = FALSE) +
  ggplot2::geom_label() +
  ggplot2::scale_fill_manual(values = c(cores_abj[1], "gray70")) +
  ggplot2::labs(
    title = "Homicídios com concurso de agentes com menor de idade",
    x = "Houve concurso com menor de idade",
    y = "Quantidade de processos"
  )

ggplot2::ggsave(
  "data-raw/bruno-nassar-puc/img/p06.png",
  p06, width = 7, height = 6
)

# 7 – Coluna AA: qual a distribuição de número de réus julgados? =========================================================================
p07 <- da |>
  dplyr::filter(!is.na(n_reus_julgados)) |>
  dplyr::count(n_reus_julgados) |>
  dplyr::mutate(
    prop = n/sum(n),
    perc = formattable::percent(prop)
  ) |>
  ggplot2::ggplot() +
  ggplot2::aes(x = n_reus_julgados, y = n, label = perc) +
  ggplot2::geom_col(fill = cores_abj[1]) +
  ggplot2::geom_label() +
  ggplot2::scale_x_continuous(breaks = c(1,2)) +
  ggplot2::labs(
    title = "Distribuição da quantidade de réus julgados por processo",
    x = "Número de réus julgados",
    y = "Quantidade de processos"
  )

ggplot2::ggsave(
  "data-raw/bruno-nassar-puc/img/p07.png",
  p07, width = 6, height = 5
)

# 8 – Coluna AB: qual a distribuição no que diz respeito ao sexo dos réus? =========================================================================
p08 <- da |>
  dplyr::transmute(
    id_processo,
    sexo_reu1 = dplyr::case_when(
      n_reus_julgados == 1 ~ sexo_reus_julgados,
      n_reus_julgados == 2 & sexo_reus_julgados == "Masculino" ~ "Masculino",
      n_reus_julgados == 2 & sexo_reus_julgados == "Ambas" ~ "Masculino"
    ),
    sexo_reu2 = dplyr::case_when(
      n_reus_julgados == 1 ~ NA_character_,
      n_reus_julgados == 2 & sexo_reus_julgados == "Masculino" ~ "Masculino",
      n_reus_julgados == 2 & sexo_reus_julgados == "Ambas" ~ "Feminino"
    )
  ) |>
  tidyr::pivot_longer(cols = contains("sexo_reu"), values_to = "sexo_reus_julgados") |>
  dplyr::filter(!is.na(sexo_reus_julgados)) |>
  dplyr::count(sexo_reus_julgados) |>
  dplyr::arrange(desc(n)) |>
  dplyr::mutate(
    prop = n/sum(n),
    perc = formattable::percent(prop),
    sexo_reus_julgados = forcats::fct_inorder(sexo_reus_julgados)
  ) |>
  ggplot2::ggplot() +
  ggplot2::aes(x = sexo_reus_julgados, y = n, label = perc) +
  ggplot2::geom_col(fill = cores_abj[1]) +
  ggplot2::geom_label() +
  ggplot2::labs(
    title = "Distribuição de sexo dos réus julgados",
    x = "Sexo dos réus julgados",
    y = "Quantidade de réus"
  )

ggplot2::ggsave(
  "data-raw/bruno-nassar-puc/img/p08.png",
  p08, width = 7, height = 4
)

# 9 – Coluna AC: qual a distribuição no que diz respeito à cor dos réus? =========================================================================
p09 <- da |>
  dplyr::transmute(
    id_processo,
    cor_reu1 = dplyr::case_when(
      n_reus_julgados == 1 ~ cor_reus_julgados,
      n_reus_julgados == 2 & cor_reus_julgados == "Preta" ~ "Preta",
      n_reus_julgados == 2 & cor_reus_julgados == "Branca" ~ "Branca",
      n_reus_julgados == 2 & cor_reus_julgados == "Branca, Parda" ~ "Branca",
      n_reus_julgados == 2 & cor_reus_julgados == "Branca, Preta" ~ "Branca",
      n_reus_julgados == 2 & cor_reus_julgados == "Parda" ~ "Parda",
      n_reus_julgados == 2 & cor_reus_julgados == "Parda, Preta" ~ "Parda",
      n_reus_julgados == 2 & cor_reus_julgados == "Preta" ~ "Preta"
    ),
    cor_reu2 = dplyr::case_when(
      n_reus_julgados == 1 ~ NA_character_,
      n_reus_julgados == 2 & cor_reus_julgados == "Preta" ~ "Preta",
      n_reus_julgados == 2 & cor_reus_julgados == "Branca" ~ "Branca",
      n_reus_julgados == 2 & cor_reus_julgados == "Branca, Parda" ~ "Parda",
      n_reus_julgados == 2 & cor_reus_julgados == "Branca, Preta" ~ "Preta",
      n_reus_julgados == 2 & cor_reus_julgados == "Parda" ~ "Parda",
      n_reus_julgados == 2 & cor_reus_julgados == "Parda, Preta" ~ "Preta",
      n_reus_julgados == 2 & cor_reus_julgados == "Preta" ~ "Preta"
    )
  ) |>
  tidyr::pivot_longer(cols = contains("cor_reu"), values_to = "cor_reus_julgados") |>
  dplyr::filter(!is.na(cor_reus_julgados)) |>
  dplyr::count(cor_reus_julgados) |>
  dplyr::arrange(desc(n)) |>
  dplyr::mutate(
    prop = n/sum(n),
    perc = formattable::percent(prop),
    col_dif = cor_reus_julgados == "Não consta",
    cor_reus_julgados = forcats::fct_inorder(cor_reus_julgados)
  ) |>
  ggplot2::ggplot() +
  ggplot2::aes(x = cor_reus_julgados, y = n, label = perc) +
  ggplot2::geom_col(ggplot2::aes(fill = col_dif), show.legend = FALSE) +
  ggplot2::geom_label() +
  ggplot2::scale_fill_manual(values = c(cores_abj[1], "gray70")) +
  ggplot2::labs(
    title = "Distribuição de cor dos réus julgados",
    x = "Cor dos réus julgados",
    y = "Quantidade de réus"
  )

ggplot2::ggsave(
  "data-raw/bruno-nassar-puc/img/p09.png",
  p09, width = 10, height = 6
)

# 10 – Coluna AD: qual a distribuição no que diz respeito a se o réu é policial? =========================================================================
p10 <- da |>
  dplyr::filter(!is.na(reu_policial)) |>
  dplyr::mutate(
    reu_policial_generico = dplyr::case_when(
      stringr::str_detect(reu_policial, "Sim") ~ "Sim",
      TRUE ~ reu_policial
    )
  ) |>
  dplyr::count(reu_policial_generico) |>
  dplyr::mutate(
    prop = n/sum(n),
    perc = formattable::percent(prop),
    col_dif = reu_policial_generico == "Não consta",
    reu_policial_generico = factor(reu_policial_generico, levels = c("Não", "Sim", "Não consta"))
  ) |>
  ggplot2::ggplot() +
  ggplot2::aes(x = reu_policial_generico, y = n, label = perc) +
  ggplot2::geom_col(ggplot2::aes(fill = col_dif), show.legend = FALSE) +
  ggplot2::geom_label() +
  ggplot2::scale_fill_manual(values = c(cores_abj[1], "gray70")) +
  ggplot2::labs(
    x = "O réu é policial?",
    y = "Quantidade de processos"
  )

ggplot2::ggsave(
  "data-raw/bruno-nassar-puc/img/p10.png",
  p10, width = 7, height = 5
)

# da |>
#   dplyr::mutate(
#     reu_policial_generico = dplyr::case_when(
#       stringr::str_detect(reu_policial, "Sim") ~ "Sim",
#       TRUE ~ reu_policial
#     )
#   ) |>
#   dplyr::filter(reu_policial_generico == "Sim") |>
#   dplyr::count(reu_policial) |>
#   dplyr::mutate(
#     prop = n/sum(n),
#     perc = formattable::percent(prop),
#     group = 1
#   ) |>
#   ggplot2::ggplot() +
#   ggplot2::aes(x = group, y = prop,
#                label = perc,
#                group = reu_policial) +
#   ggplot2::geom_col(
#     ggplot2::aes(fill = reu_policial),
#     width = .4) +
#   ggplot2::geom_label(
#     fill = "white",
#     position=ggplot2::position_fill(vjust = 0.5)
#   ) +
#   ggplot2::scale_fill_manual("Tipos de policiais", values = cores_abj[2:1]) +
#   ggplot2::theme(
#     axis.title.x = ggplot2::element_blank(),
#     axis.text.x = ggplot2::element_blank(),
#     axis.ticks.x = ggplot2::element_blank()
#   ) +
#   ggplot2::labs(
#     title = glue::glue("Tipo de policial, quando algum réu é policial"),
#     y = "Proporção de casos"
#   )

# 11 – Coluna AE: qual a distribuição no que diz respeito à quantidade de vítimas? =========================================================================
p11 <- da |>
  dplyr::filter(!is.na(n_vitimas)) |>
  dplyr::count(n_vitimas) |>
  dplyr::mutate(
    prop = n/sum(n),
    perc = formattable::percent(prop),
    col_dif = n_vitimas == "Não consta",
    n_vitimas = factor(n_vitimas, levels = c("1", "2", "3", "4 ou mais", "Não consta"))
  ) |>
  ggplot2::ggplot() +
  ggplot2::aes(x = n_vitimas, y = n, label = perc) +
  ggplot2::geom_col(ggplot2::aes(fill = col_dif), show.legend = FALSE) +
  ggplot2::geom_label() +
  ggplot2::scale_fill_manual(values = c(cores_abj[1], "gray70")) +
  ggplot2::labs(
    x = "Número de vítimas",
    y = "Quantidade de processos"
  )

ggplot2::ggsave(
  "data-raw/bruno-nassar-puc/img/p11.png",
  p11, width = 8, height = 6
)

# 12 – Coluna AF: qual a distribuição no que diz respeito ao sexo das vítimas? =========================================================================
p12 <- da |>
  dplyr::transmute(
    id_processo,
    sexo_vitima1 = dplyr::case_when(
      sexo_vitimas == "Não consta" ~ "Não consta",
      n_vitimas == "1" ~ sexo_vitimas,
      n_vitimas == "2" & sexo_vitimas == "Masculino" ~ "Masculino",
      n_vitimas == "2" & sexo_vitimas == "Feminino" ~ "Feminino",
      n_vitimas == "2" & sexo_vitimas == "Ambos" ~ "Masculino",
      n_vitimas == "3" & sexo_vitimas == "Masculino" ~ "Masculino",
      n_vitimas == "3" & sexo_vitimas == "Ambos" ~ "Masculino",
      n_vitimas == "4 ou mais" & sexo_vitimas == "Masculino" ~ "Masculino"
    ),
    sexo_vitima2 = dplyr::case_when(
      n_vitimas == "1" ~ NA_character_,
      n_vitimas == "2" & sexo_vitimas == "Masculino" ~ "Masculino",
      n_vitimas == "2" & sexo_vitimas == "Feminino" ~ "Feminino",
      n_vitimas == "2" & sexo_vitimas == "Ambos" ~ "Feminino",
      n_vitimas == "3" & sexo_vitimas == "Masculino" ~ "Masculino",
      n_vitimas == "3" & sexo_vitimas == "Ambos" ~ "Masculino",
      n_vitimas == "4 ou mais" & sexo_vitimas == "Masculino" ~ "Masculino"
    ),
    sexo_vitima3 = dplyr::case_when(
      n_vitimas == "1" ~ NA_character_,
      n_vitimas == "2" ~ NA_character_,
      n_vitimas == "3" & sexo_vitimas == "Masculino" ~ "Masculino",
      n_vitimas == "3" & sexo_vitimas == "Ambos" ~ "Feminino",
      n_vitimas == "4 ou mais" & sexo_vitimas == "Masculino" ~ "Masculino"
    ),
    sexo_vitima4 = dplyr::case_when(
      n_vitimas == "1" ~ NA_character_,
      n_vitimas == "2" ~ NA_character_,
      n_vitimas == "3" ~ NA_character_,
      n_vitimas == "4 ou mais" & sexo_vitimas == "Masculino" ~ "Masculino"
    ),
  ) |>
  tidyr::pivot_longer(cols = contains("sexo_vitima"), values_to = "sexo_vitimas") |>
  dplyr::filter(!is.na(sexo_vitimas)) |>
  dplyr::count(sexo_vitimas) |>
  dplyr::arrange(desc(n)) |>
  dplyr::mutate(
    sexo_vitimas = forcats::fct_inorder(sexo_vitimas),
    sexo_vitimas = forcats::fct_relevel(sexo_vitimas, after = Inf, "Não consta"),
    prop = n/sum(n),
    perc = formattable::percent(prop),
    col_dif = sexo_vitimas == "Não consta"
  ) |>
  ggplot2::ggplot() +
  ggplot2::aes(x = sexo_vitimas, y = n, label = perc) +
  ggplot2::geom_col(ggplot2::aes(fill = col_dif), show.legend = FALSE) +
  ggplot2::geom_label() +
  ggplot2::scale_fill_manual(values = c(cores_abj[1], "gray70")) +
  ggplot2::labs(
    title = "Distribuição do sexo das vítimas",
    x = "Sexo das vítimas",
    y = "Quantidade de vítimas"
  )

ggplot2::ggsave(
  "data-raw/bruno-nassar-puc/img/p12.png",
  p12, width = 10, height = 6
)
# 13 – Coluna AG - qual a distribuição no que diz respeito à delegacia que desenvolveu o IP? =========================================================================
p13 <- da |>
  dplyr::mutate(agente_ip = forcats::fct_infreq(agente_ip)) |>
  grafico_base(agente_ip) +
  ggplot2::labs(
    x = "Delegacia que desenvolveu o Inquérito Policial",
    y = "Quantidade de processos"
  )

ggplot2::ggsave(
  "data-raw/bruno-nassar-puc/img/p13.png",
  p13, width = 7, height = 6
)

# 14 – Coluna AH: qual a distribuição no que diz respeito à natureza da decisão? =========================================================================
p14 <- da |>
  dplyr::mutate(
    # natureza_decisao = stringr::str_remove_all(natureza_decisao, "\""),
    natureza_decisao = forcats::fct_infreq(natureza_decisao)
  ) |>
  grafico_base(natureza_decisao) +
  ggplot2::scale_x_discrete(labels=scales::label_wrap(20)) +
  ggplot2::labs(
    x = "Natureza da decisão",
    y = "Quantidade de processos"
  )

ggplot2::ggsave(
  "data-raw/bruno-nassar-puc/img/p14.png",
  p14, width = 10, height = 4
)

# 15 – Coluna AI: qual a distribuição no que diz respeito a se o processo foi adiado pela pandemia? =========================================================================
p15 <- da |>
  grafico_base(pandemia_juri) +
  ggplot2::labs(
    x = "Ainda não foi designado o plenário do júri em razão da pandemia?",
    y = "Quantidade de processos"
  )

ggplot2::ggsave(
  "data-raw/bruno-nassar-puc/img/p15.png",
  p15, width = 7, height = 6
)

# 16 – Coluna AJ (processos com resposta “absolvição sumária” na coluna AH): qual a distribuição das causas de absolvição sumária? =========================================================================

# 17 – Coluna AK (processos com resposta “extinção da punibilidade” na coluna AH): qual a distribuição das causas de extinção da punibilidade? =========================================================================

# 18 – Processos que tiveram pronúncia (resposta “sentença de pronúncia” ou “sentença após plenário do júri” na coluna AH): =========================================================================
# a) Coluna AL: quantos casos tiveram uma dessas decisões convertida em pronúncia? -----------------------------------------------------------------------
# b) Coluna AO: quantos casos tiveram RESE? -----------------------------------------------------------------------
da |>
  grafico_base(pronuncia_rese) +
  ggplot2::labs(
    x = "A pronúncia foi alvo de RESE",
    y = "QUantidade de processos"
  )

# c) Coluna AP: para os casos que tiveram como resposta “sim” na coluna AO, medir o tempo entre a sentença de pronúncia (coluna M) e a decisão sobre o RESE (coluna AP). Para esses casos, medir também o tempo entre a sentença de pronúncia (coluna M) e o plenário (coluna N. Mas só se já houve plenário, ou seja, casos com resposta “sentença após plenário do júri” na coluna AH”). -----------------------------------------------------------------------
da |>
  dplyr::filter(pronuncia_rese == "Sim") |>
  dplyr::mutate(
    tempo_sentenca_rese =  dt_acordao_rese - dt_pronuncia
  ) |>
  dplyr::select(id_processo, tempo_sentenca_rese) |>
  dplyr::filter(tempo_sentenca_rese >= 0)

# 19 – Processos que tiveram sentença após plenário (resposta “sentença após plenário do júri” na coluna AH): =========================================================================
# a) Coluna AH: qual a distribuição no que diz respeito à natureza da defesa? -----------------------------------------------------------------------
# b) Coluna AR: em quantos casos houve mudança da defesa no curso do processo? -----------------------------------------------------------------------
# c) Coluna AS: em quantos casos houve assistente de acusação colaborando em plenário? -----------------------------------------------------------------------
# d) Coluna AT: qual foi a distribuição quanto ao resultado do julgamento? -----------------------------------------------------------------------
# e) Coluna AU: para os casos com resultado “desclassificação” na coluna AT, qual foi o resultado do julgamento? -----------------------------------------------------------------------

# 20 – Processos que tiveram resultado condenação após plenário (resposta “condenação sobre crime de homicídio” na coluna AT): =========================================================================

# a) Coluna AW: qual a distribuição quanto ao resultado do crime? -----------------------------------------------------------------------
# b) Coluna BA: qual a distribuição quanto às possíveis causas de aumento? -----------------------------------------------------------------------
# c) Coluna BB: quantos réus são reincidentes e quantos não são? -----------------------------------------------------------------------
# d) Coluna BC: quantos casos foram homicídios privilegiados? -----------------------------------------------------------------------

# 21 – Pena dos processos que tiveram condenação: =========================================================================
# a) Coluna BD: qual foi o tempo de pena? -----------------------------------------------------------------------
# b) Coluna BE: qual foi o regime de pena aplicado? -----------------------------------------------------------------------

# 22 – Recurso: =========================================================================
# a) Coluna BF: O réu pôde recorrer em liberdade? -----------------------------------------------------------------------
# b) Coluna BG: em quantos casos foi interposto recurso contra a sentença? -----------------------------------------------------------------------
# c) Coluna BH: quem interpôs o recurso? -----------------------------------------------------------------------
# d) Coluna BI: o recurso já foi julgado? -----------------------------------------------------------------------
# e) Coluna BJ: nos casos em que o recurso já foi julgado (resposta “sim” na coluna BI), qual o lapso temporal entre a data da sentença (coluna N) e a data do julgamento do recurso -----------------------------------------------------------------------
# f) Coluna BK: nos casos em que o recurso já foi julgado (resposta “sim” na coluna BI), qual foi o resultado do acórdão? Nos casos em que o recurso foi provido ou parcialmente provido, indicar em quantos deles foi alterada a pena (coluna BL) e/ou o regime de cumprimento -----------------------------------------------------------------------
# (coluna BM) e para qual patamar em cada caso -----------------------------------------------------------------------

# 23 – Sentença nulificada: =========================================================================
# a) Coluna BN: em quantos casos houve sentença nulificada? -----------------------------------------------------------------------
# b) Nos casos em que houve sentença nulificada (resposta “sim” na coluna BN): -----------------------------------------------------------------------
# (i) Coluna BO: medir o lapso temporal entre a sentença nulificada (coluna BO) e a sentença atual (coluna N); medir o lapso temporal entre a sentença nulificada (coluna BO) e o acórdão que a nulificou (coluna BR); comparar o tempo de duração total desses processos com sentença nulificada com os processos sem sentença nulificada. -----------------------------------------------------------------------
# (ii) Coluna BP: qual o fundamento para nulificar a sentença? -----------------------------------------------------------------------
# (iii) Coluna BQ: qual Tribunal nulificou a sentença? -----------------------------------------------------------------------

# 24 – Colunas F, I, J, K, L, M, N: =========================================================================
# a) Tempo de duração total do processo (do fato até a sentença) -----------------------------------------------------------------------
# b) Tempo entre os marcos temporais: data do fato até instauração do IP; instauração do IP até distribuição do processo; distribuição do processo até denúncia; denúncia até seu recebimento; do recebimento até a pronúncia; do recebimento até a impronúncia, desclassificação na primeira fase, absolvição sumária ou extinção da punibilidade; da pronúncia até a sentença após plenário -----------------------------------------------------------------------

# 25 – Colunas O, P e Q: para os processos que foram suspensos com base no art. 366 (resposta “sim” na coluna O), qual o intervalo de duração entre a coluna P (data da suspensão) e a coluna Q (data da revogação da suspensão) =========================================================================

# 26 – Colunas R e S + AM e NA + AY e AZ: =========================================================================
# a) Primeiro, medir quantos casos são homicídios simples em cada um desses momentos (denúncia – pronúncia – sentença) =========================================================================
# b) Se for qualificado, qual o grau de incidência de cada qualificadora -----------------------------------------------------------------------
# c) Por fim, verificar como o enquadramento típico evoluiu ao longo desses três momentos processuais. Na prática, comparar os resultados das perguntas “a” e “b” acima revelarão isso. No entanto, a comparação completa só fará sentido nos casos em que houve denúncia, pronúncia e plenário, devendo ser isolados os casos em que houve denúncia, mas não pronúncia e os casos em que houve pronúncia mas ainda não houve plenário. -----------------------------------------------------------------------

# 27 – Colunas V, W, X, Y e Z (perguntas sobre prisão provisória): =========================================================================
# a) Em quantos casos houve flagrante (coluna V) -----------------------------------------------------------------------
# b) Em quantos casos houve preventiva (coluna W) -----------------------------------------------------------------------
# c) Coluna Y: nos casos em que houve prisão (flagrante ou preventiva), em quantos houve relaxamento da prisão? E em quantos o réu ficou preso até o fim do processo? -----------------------------------------------------------------------
# d) Colunas X + Z: nos casos em que houve relaxamento da prisão, qual o intervalo de tempo entre a decisão que decretou a prisão e a decisão que determinou a soltura -----------------------------------------------------------------------

# 28 – Coluna AH (menos quando a resposta foi “sentença de pronúncia”) e Coluna AT (resultado do julgamento) X colunas variadas = basicamente, comparar o resultado do julgamento conforme: =========================================================================
# a) Se o réu foi preso preventivamente ou não durante o processo (coluna W) -----------------------------------------------------------------------
# b) O número de réus julgados (coluna AA) -----------------------------------------------------------------------
# c) O sexo dos réus julgados (coluna AB) -----------------------------------------------------------------------
# d) A cor dos réus julgados (coluna AC) -----------------------------------------------------------------------
# e) Se o réu era policial (coluna AD) -----------------------------------------------------------------------
# f) A quantidade de vítimas (coluna AE) -----------------------------------------------------------------------
# g) O sexo das vítimas (coluna AF) -----------------------------------------------------------------------
# h) Quem desenvolveu o inquérito (coluna AG) -----------------------------------------------------------------------
# i) Horário do fato (coluna H) -----------------------------------------------------------------------
# j) A natureza da defesa (coluna AQ) -----------------------------------------------------------------------
# k) Se houve mudança da defesa no curso dos autos (coluna AR) -----------------------------------------------------------------------
# l) O resultado do crime (coluna AW) -----------------------------------------------------------------------

# 29 – Tempo de duração do processo X colunas variadas = basicamente, comparar o tempo de duração do processo conforme: =========================================================================
# a) Se o réu chegou a ser preso preventivamente (coluna X) -----------------------------------------------------------------------
# b) O número de réus julgados (coluna AA) -----------------------------------------------------------------------
# c) Se o réu era policial (coluna AD) -----------------------------------------------------------------------
# d) Quem desenvolveu o IP (coluna AG) -----------------------------------------------------------------------
# e) A natureza da defesa (coluna AQ) -----------------------------------------------------------------------
# f) Se houve mudança da defesa no curso dos autos (coluna AR) -----------------------------------------------------------------------
# g) O resultado do julgamento (coluna AT) -----------------------------------------------------------------------

# 30 – Colunas H e AF: qual a distribuição do sexo das vítimas dentro de cada resposta da coluna H, ou seja, de acordo com o horário do crime =========================================================================

# 31 – Quantidade de pena aplicada (coluna BD) X colunas variadas: basicamente, comparar a gravidade da pena imposta conforme: =========================================================================
# a) Se o réu foi preso preventivamente ou não durante o processo (coluna W) -----------------------------------------------------------------------
# b) O número de réus julgados (coluna AA) -----------------------------------------------------------------------
# c) O sexo dos réus julgados (coluna AB) -----------------------------------------------------------------------
# d) A cor dos réus julgados (coluna AC) -----------------------------------------------------------------------
# e) Se o réu era policial (coluna AD) -----------------------------------------------------------------------
# f) A quantidade de vítimas (coluna AE) -----------------------------------------------------------------------
# g) O sexo das vítimas (coluna AF) -----------------------------------------------------------------------
# h) Quem desenvolveu o inquérito (coluna AG) -----------------------------------------------------------------------
# i) Horário do fato (coluna H) -----------------------------------------------------------------------
