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

# fs::dir_create("data-raw/bruno-nassar-puc/img")

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
      n_vitimas == "3" & sexo_vitimas == "Ambos" ~ "Feminino",
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
n14 <- da |>
  dplyr::filter(!is.na(natureza_decisao)) |>
  nrow()

p14 <- da |>
  dplyr::mutate(
    # natureza_decisao = stringr::str_remove_all(natureza_decisao, "\""),
    natureza_decisao = forcats::fct_infreq(natureza_decisao)
  ) |>
  grafico_base(natureza_decisao) +
  ggplot2::scale_x_discrete(labels=scales::label_wrap(20)) +
  ggplot2::labs(
    title = glue::glue("Distribuição dos tipos de decisão (N = {n14})"),
    x = "Natureza da decisão",
    y = "Quantidade de processos"
  )

ggplot2::ggsave(
  "data-raw/bruno-nassar-puc/img/p14.png",
  p14, width = 10, height = 6
)

# 15 – Coluna AI: qual a distribuição no que diz respeito a se o processo foi adiado pela pandemia? =========================================================================

decisao_sem_juri <- c(
  "Sentença de absolvição sumária",
  "\"Sentença\" de impronúncia",
  "Decisão de desclassificação proferida ao fim da primeira fase do júri",
  "Sentença de extinção da punibilidade"
)

p15 <- da |>
  dplyr::mutate(
    id_processo,
    pandemia_juri = dplyr::case_when(
      natureza_decisao %in% decisao_sem_juri ~ NA_character_,
      pandemia_juri == "Sim" ~ "Não, em razão da pandemia",
      pandemia_juri == "Não" ~ "Sim"
    ),
    pandemia_juri = forcats::fct_infreq(pandemia_juri)
  ) |>
  grafico_base(pandemia_juri) +
  ggplot2::labs(
    title = "Efeitos da pandemia na realização do plenário do júri",
    x = "O plenário do júri já foi designado?",
    y = "Quantidade de processos"
  )

ggplot2::ggsave(
  "data-raw/bruno-nassar-puc/img/p15.png",
  p15, width = 7, height = 6
)

# 16 – Coluna AJ (processos com resposta “absolvição sumária” na coluna AH): qual a distribuição das causas de absolvição sumária? =========================================================================
n_absolvicao_sumaria <- da |>
  dplyr::filter(natureza_decisao == "Sentença de absolvição sumária") |>
  nrow()

p16 <- da |>
  dplyr::filter(natureza_decisao == "Sentença de absolvição sumária") |>
  dplyr::mutate(
    fundamento_36 = forcats::fct_infreq(fundamento_36)
  ) |>
  grafico_base(fundamento_36) +
  ggplot2::labs(
    title = glue::glue("Fundamentos para a Absolvição Sumária (N = {n_absolvicao_sumaria})"),
    x = "Fundamento",
    y = "Quantidade de processos"
  )

ggplot2::ggsave(
  "data-raw/bruno-nassar-puc/img/p16.png",
  p16, width = 7, height = 6
)

# 17 – Coluna AK (processos com resposta “extinção da punibilidade” na coluna AH): qual a distribuição das causas de extinção da punibilidade? =========================================================================
n_extincao_punibilidade <- da |>
  dplyr::filter(natureza_decisao == "Sentença de extinção da punibilidade") |>
  nrow()

p17 <- da |>
  dplyr::filter(natureza_decisao == "Sentença de extinção da punibilidade") |>
  dplyr::mutate(
    causa_de_extincao_da_punibilidade = forcats::fct_infreq(causa_de_extincao_da_punibilidade)
  ) |>
  grafico_base(causa_de_extincao_da_punibilidade) +
  ggplot2::labs(
    title = glue::glue("Fundamentos para a Extinção da Punibilidade (N = {n_extincao_punibilidade})"),
    x = "Fundamento",
    y = "Quantidade de processos"
  )

ggplot2::ggsave(
  "data-raw/bruno-nassar-puc/img/p17.png",
  p17, width = 8, height = 6
)

# 18 – Processos que tiveram pronúncia (resposta “sentença de pronúncia” ou “sentença após plenário do júri” na coluna AH): =========================================================================
da18 <- da |>
  dplyr::filter(
    natureza_decisao == "\"Sentença\" de pronúncia" |
      natureza_decisao == "Sentença após plenário do júri"
  )

n18 <- nrow(da18)

# a) Coluna AL: quantos casos tiveram uma dessas decisões convertida em pronúncia? -----------------------------------------------------------------------
p18a <- da18 |>
  dplyr::mutate(conversao_pronuncia = forcats::fct_infreq(conversao_pronuncia)) |>
  grafico_base(conversao_pronuncia) +
  ggplot2::labs(
    title = glue::glue("Conversão em pronúncia (N = {n18})"),
    x = "A decisão foi convertida em pronúncia?",
    y = "Quantidade de processos"
  )

ggplot2::ggsave(
  "data-raw/bruno-nassar-puc/img/p18a.png",
  p18a, width = 8, height = 6
)

# b) Coluna AO: quantos casos tiveram RESE? -----------------------------------------------------------------------
p18b <- da18 |>
  dplyr::mutate(pronuncia_rese = forcats::fct_infreq(pronuncia_rese)) |>
  grafico_base(pronuncia_rese) +
  ggplot2::labs(
    title = glue::glue("RESE (N = {n18})"),
    x = "A pronúncia foi alvo de RESE?",
    y = "QUantidade de processos"
  )

ggplot2::ggsave(
  "data-raw/bruno-nassar-puc/img/p18b.png",
  p18b, width = 8, height = 6
)

# c) Coluna AP: para os casos que tiveram como resposta “sim” na coluna AO, medir o tempo entre a sentença de pronúncia (coluna M) e a decisão sobre o RESE (coluna AP).  -----------------------------------------------------------------------
n_rese <- da18 |>
  dplyr::filter(pronuncia_rese == "Sim") |>
  nrow()

da18c <- da18 |>
  dplyr::filter(pronuncia_rese == "Sim") |>
  dplyr::mutate(
    tempo_sentenca_rese =  as.numeric(dt_acordao_rese - dt_pronuncia)
  ) |>
  dplyr::select(id_processo, tempo_sentenca_rese) |>
  dplyr::filter(tempo_sentenca_rese >= 0)

media_pronuncia_rese <- round(mean(da18c$tempo_sentenca_rese))

p18c <- da18c |>
  ggplot2::ggplot() +
  ggplot2::aes(x = tempo_sentenca_rese) +
  ggplot2::geom_histogram(fill = cores_abj[1], bins = 60) +
  ggplot2::geom_vline(xintercept = media_pronuncia_rese, color = "red", linetype = 2) +
  ggplot2::geom_text(
    ggplot2::aes(
      label = paste0(media_pronuncia_rese, " dias"),
      x = media_pronuncia_rese + 40,
      y = 3.5
    ),
    color = "red"
  ) +
  ggplot2::labs(
    title = glue::glue("Tempo entre a decisão de pronúncia e a decisão sobre o RESE\n(N = {n_rese})"),
    x = "Tempo (dias)",
    y = "Quantidade de processos"
  )

ggplot2::ggsave(
  "data-raw/bruno-nassar-puc/img/p18c.png",
  p18c, width = 8, height = 4
)
# d) Para esses casos, medir também o tempo entre a sentença de pronúncia (coluna M) e o plenário (coluna N. Mas só se já houve plenário, ou seja, casos com resposta “sentença após plenário do júri” na coluna AH”). -----------------------------------------------------------------------
n_plenario <- da18 |>
  dplyr::filter(natureza_decisao == "Sentença após plenário do júri") |>
  nrow()

da18d <- da18 |>
  dplyr::filter(natureza_decisao == "Sentença após plenário do júri") |>
  dplyr::mutate(
    tempo_sentenca_plenario = as.numeric(dt_plenario - dt_pronuncia)
  ) |>
  dplyr::select(id_processo, tempo_sentenca_plenario) |>
  dplyr::filter(tempo_sentenca_plenario >= 0)

media_pronuncia_plenario <- round(mean(da18d$tempo_sentenca_plenario))

p18d <- da18d |>
  ggplot2::ggplot() +
  ggplot2::aes(x = tempo_sentenca_plenario) +
  ggplot2::geom_histogram(fill = cores_abj[1], bins = 60) +
  ggplot2::geom_vline(xintercept = media_pronuncia_plenario, color = "red", linetype = 2) +
  ggplot2::geom_text(
    ggplot2::aes(
      label = paste0(media_pronuncia_rese, " dias"),
      x = media_pronuncia_rese + 70,
      y = 5.2
    ),
    color = "red"
  ) +
  ggplot2::labs(
    title = glue::glue("Tempo entre a decisão de pronúncia e a decisão sobre o RESE\n(N = {n_plenario})"),
    x = "Tempo (dias)",
    y = "Quantidade de processos"
  )

ggplot2::ggsave(
  "data-raw/bruno-nassar-puc/img/p18d.png",
  p18d, width = 8, height = 4
)

# 19 – Processos que tiveram sentença após plenário (resposta “sentença após plenário do júri” na coluna AH): =========================================================================
da19 <- da |>
  dplyr::filter(natureza_decisao == "Sentença após plenário do júri")

n19 <- nrow(da19)

# a) Coluna AH: qual a distribuição no que diz respeito à natureza da defesa? -----------------------------------------------------------------------
p19a <- da19 |>
  dplyr::mutate(defesa_inicial = forcats::fct_infreq(defesa_inicial)) |>
  grafico_base(defesa_inicial) +
  ggplot2::labs(
    title = glue::glue("Natureza da defesa inicial (N = {n19})"),
    x = "Tipo de defesa",
    y = "Quantidade de processos"
  )

ggplot2::ggsave(
  "data-raw/bruno-nassar-puc/img/p19a.png",
  p19a, width = 8, height = 4
)

# b) Coluna AR: em quantos casos houve mudança da defesa no curso do processo? -----------------------------------------------------------------------
p19b <- da19 |>
  dplyr::mutate(defesa_mudanca = forcats::fct_infreq(defesa_mudanca)) |>
  dplyr::filter(!is.na(defesa_mudanca)) |>
  dplyr::count(defesa_inicial, defesa_mudanca) |>
  dplyr::group_by(defesa_inicial) |>
  dplyr::mutate(
    prop = n/sum(n),
    perc = formattable::percent(prop),
    col_dif = defesa_mudanca == "Não consta"
  ) |>
  dplyr::ungroup() |>
  ggplot2::ggplot() +
  ggplot2::aes(x = defesa_mudanca, y = n, label = perc) +
  ggplot2::geom_col(ggplot2::aes(fill = col_dif), show.legend = FALSE) +
  ggplot2::geom_label() +
  ggplot2::facet_wrap(~defesa_inicial,scales = "free_x") +
  ggplot2::scale_fill_manual(values = c(cores_abj[1], "gray70")) +
  ggplot2::labs(
    title = glue::glue("Mudança de defesa (N = {n19})"),
    x = "Houve mudança da defesa no curso do processo?",
    y = "Quantidade de processos"
  )

ggplot2::ggsave(
  "data-raw/bruno-nassar-puc/img/p19b.png",
  p19b, width = 7, height = 6
)

# c) Coluna AS: em quantos casos houve assistente de acusação colaborando em plenário? -----------------------------------------------------------------------
p19c <- da19 |>
  dplyr::mutate(assistente_de_acusacao = forcats::fct_infreq(assistente_de_acusacao)) |>
  dplyr::filter(!is.na(assistente_de_acusacao)) |>
  dplyr::count(assistente_de_acusacao) |>
  dplyr::mutate(
    prop = n/sum(n),
    perc = formattable::percent(prop),
    col_dif = assistente_de_acusacao == "Não consta"
  ) |>
  ggplot2::ggplot() +
  ggplot2::aes(x = assistente_de_acusacao, y = n, label = perc) +
  ggplot2::geom_col(ggplot2::aes(fill = col_dif), show.legend = FALSE) +
  ggplot2::geom_label() +
  ggplot2::scale_fill_manual(values = c(cores_abj[1], "gray70")) +
  ggplot2::labs(
    title = glue::glue("Assistente de acusação (N = {n19})"),
    x = "Houve assistente de acusação colaborando em plenário?",
    y = "Quantidade de processos"
  )

ggplot2::ggsave(
  "data-raw/bruno-nassar-puc/img/p19c.png",
  p19c, width = 7, height = 6
)

# d) Coluna AT: qual foi a distribuição quanto ao resultado do julgamento? -----------------------------------------------------------------------
p19d <- da19 |>
  dplyr::mutate(julgamento = forcats::fct_infreq(julgamento)) |>
  grafico_base(julgamento) +
  ggplot2::labs(
    title = glue::glue("Resultados do julgamento (N = {n19})"),
    x = "Resultado do julgamento",
    y = "Quantidade de processos"
  )

ggplot2::ggsave(
  "data-raw/bruno-nassar-puc/img/p19d.png",
  p19d, width = 7, height = 6
)

# e) Coluna AU: para os casos com resultado “desclassificação” na coluna AT, qual foi o resultado do julgamento? -----------------------------------------------------------------------
n19d <- da19 |>
  dplyr::filter(julgamento == "Desclassificação") |>
  nrow()

p19e <- da19 |>
  dplyr::filter(julgamento == "Desclassificação") |>
  dplyr::mutate(desclassificacao = forcats::fct_infreq(desclassificacao)) |>
  grafico_base(desclassificacao) +
  ggplot2::labs(
    title = glue::glue("Natureza da defesa mudada (N = {n19d})"),
    x = "Tipo de defesa",
    y = "Quantidade de processos"
  )

ggplot2::ggsave(
  "data-raw/bruno-nassar-puc/img/p19e.png",
  p19e, width = 7, height = 6
)

# 20 – Processos que tiveram resultado condenação após plenário (resposta “condenação sobre crime de homicídio” na coluna AT): =========================================================================
da_condenacao <- da |>
  dplyr::filter(julgamento == "Condenação sobre o crime de homicídio")

n_condenacao <- nrow(da_condenacao)

# a) Coluna AW: qual a distribuição quanto ao resultado do crime? -----------------------------------------------------------------------
p20a <- da_condenacao |>
  dplyr::mutate(resultado_crime = forcats::fct_infreq(resultado_crime)) |>
  grafico_base(resultado_crime) +
  ggplot2::labs(
    title = glue::glue("Resultado do crime  nos casos de condenação por homicídio (N = {n_condenacao})"),
    x = "Resultado do crime",
    y = "Quantidade de processos"
  )

ggplot2::ggsave(
  "data-raw/bruno-nassar-puc/img/p20a.png",
  p20a, width = 8, height = 5
)

# b) Coluna BA: qual a distribuição quanto às possíveis causas de aumento? -----------------------------------------------------------------------
n20b <- da_condenacao |>
  dplyr::filter(plenario_causas_aumento != "N/A") |>
  nrow()

p20b <- da_condenacao |>
  dplyr::mutate(
    plenario_causas_aumento = ifelse(plenario_causas_aumento == "N/A", NA_character_, plenario_causas_aumento),
    plenario_causas_aumento = forcats::fct_infreq(plenario_causas_aumento)
  ) |>
  grafico_base(plenario_causas_aumento) +
  ggplot2::labs(
    title = glue::glue("Distribuição das causas de aumento da pena nos casos de\ncondenação por homicídio (N = {n20b})"),
    x = "Causas de aumento",
    y = "Quantidade de processos"
  )

ggplot2::ggsave(
  "data-raw/bruno-nassar-puc/img/p20b.png",
  p20b, width = 7, height = 6
)

# tomar cuidado, N muito pequeno

# c) Coluna BB: quantos réus são reincidentes e quantos não são? -----------------------------------------------------------------------
p20c <- da_condenacao |>
  dplyr::mutate(reincidente = forcats::fct_infreq(reincidente)) |>
  grafico_base(reincidente) +
  ggplot2::labs(
    title = glue::glue("Reincidência dos réus condendos por homicídio (N = {n_condenacao})"),
    x = "O réu é reincidente?",
    y = "Quantidade de processos"
  )

ggplot2::ggsave(
  "data-raw/bruno-nassar-puc/img/p20c.png",
  p20c, width = 7, height = 6
)

# d) Coluna BC: quantos casos foram homicídios privilegiados? -----------------------------------------------------------------------
p20d <- da_condenacao |>
  dplyr::mutate(homicidio_privilegido = forcats::fct_infreq(homicidio_privilegido)) |>
  grafico_base(homicidio_privilegido) +
  ggplot2::labs(
    title = glue::glue("Incidência de homicídio privilegiado nos casos de condenação\npor homicídio (N = {n_condenacao})"),
    x = "O homicídio foi privilegiado?",
    y = "Quantidade de processos"
  )

ggplot2::ggsave(
  "data-raw/bruno-nassar-puc/img/p20d.png",
  p20d, width = 7, height = 6
)

# 21 – Pena dos processos que tiveram condenação: =========================================================================
# a) Coluna BD: qual foi o tempo de pena? -----------------------------------------------------------------------
p21a <- da_condenacao |>
  dplyr::mutate(
    tempo_pena = dplyr::case_when(
      tempo_pena == "Inferior a 2 anos" ~ "Até 2 anos",
      tempo_pena == "Igual a 2 anos e que não exceda 4 anos" ~ "Entre 2 e 4 anos",
      tempo_pena == "Superior a 4 anos e que não exceda 8 anos" ~ "Entre 4 e 8 anos",
      tempo_pena == "Superior a 8 anos e inferior a 12 anos" ~ "Entre 8 e 12 anos",
      tempo_pena == "Igual a 12 anos e inferior a 15 anos" ~ "Entre 12 e 15 anos",
      tempo_pena == "Igual a 15 anos e inferior a 20 anos" ~ "Entre 15 e 20 anos",
      tempo_pena == "Igual a 20 anos e inferior a 30 anos" ~ "Entre 20 e 30 anos",
      tempo_pena == "Igual ou superior a 30 anos" ~ "Igual ou superior a 30 anos"
    ),
    tempo_pena = factor(
      tempo_pena,
      levels = c(
        "Até dois anos",
        "Entre 2 e 4 anos",
        "Entre 4 e 8 anos",
        "Entre 8 e 12 anos",
        "Entre 12 e 15 anos",
        "Entre 15 e 20 anos",
        "Entre 20 e 30 anos",
        "Igual ou superior a 30 anos"
      )
    )
  ) |>
  grafico_base(tempo_pena) +
  ggplot2::scale_x_discrete(labels=scales::label_wrap(20)) +
  ggplot2::labs(
    title = glue::glue("Tempo de pena dos casos de condenação por homicídio (N = {n_condenacao})"),
    x = "Tempo de pena",
    y = "Quantidade de processos"
  )

ggplot2::ggsave(
  "data-raw/bruno-nassar-puc/img/p21a.png",
  p21a, width = 10, height = 6
)

# b) Coluna BE: qual foi o regime de pena aplicado? -----------------------------------------------------------------------
p21b <- da_condenacao |>
  dplyr::mutate(regime_inicial = forcats::fct_infreq(regime_inicial)) |>
  grafico_base(regime_inicial) +
  ggplot2::labs(
    title = glue::glue("Regime de pena inicial nos casos de condenação\npor homicídio (N = {n_condenacao})"),
    x = "Regime de pena",
    y = "Quantidade de processos"
  )

ggplot2::ggsave(
  "data-raw/bruno-nassar-puc/img/p21b.png",
  p21b, width = 8, height = 5
)

# 22 – Recurso: =========================================================================
# a) Coluna BF: O réu pôde recorrer em liberdade? -----------------------------------------------------------------------
n_liberdade <- da |>
  dplyr::filter(!is.na(pode_liberdade)) |>
  nrow()

p22a <- da |>
  dplyr::mutate(pode_liberdade = forcats::fct_infreq(pode_liberdade)) |>
  grafico_base(pode_liberdade) +
  ggplot2::labs(
    title = glue::glue("Recurso em liberdade (N = {n_liberdade})"),
    x = "O réu pôde recorrer em liberdade?",
    y = "Quantidade de processos"
  )

ggplot2::ggsave(
  "data-raw/bruno-nassar-puc/img/p22a.png",
  p22a, width = 7, height = 6
)

# b) Coluna BG: em quantos casos foi interposto recurso contra a sentença? -----------------------------------------------------------------------
n_recurso <- da |>
  dplyr::filter(!is.na(recurso)) |>
  nrow()

p22b <- da |>
  dplyr::mutate(recurso = forcats::fct_infreq(recurso)) |>
  grafico_base(recurso) +
  ggplot2::labs(
    title = glue::glue("Presença de recurso (N = {n_recurso})"),
    x = "Foi interposto recurso contra sentença?",
    y = "Quantidade de processos"
  )

ggplot2::ggsave(
  "data-raw/bruno-nassar-puc/img/p22b.png",
  p22b, width = 7, height = 6
)

# c) Coluna BH: quem interpôs o recurso? -----------------------------------------------------------------------
n_teve_recurso <- da |>
  dplyr::filter(!is.na(quem_interpos_recurso)) |>
  nrow()

p22c <- da |>
  dplyr::mutate(quem_interpos_recurso = forcats::fct_infreq(quem_interpos_recurso)) |>
  grafico_base(quem_interpos_recurso) +
  ggplot2::labs(
    title = glue::glue("Polo que interpôs recurso (N = {n_teve_recurso})"),
    x = "Quem interpôs recurso",
    y = "Quantidade de processos"
  )

ggplot2::ggsave(
  "data-raw/bruno-nassar-puc/img/p22c.png",
  p22c, width = 8, height = 5
)

# d) Coluna BI: o recurso já foi julgado? -----------------------------------------------------------------------
p22d <- da |>
  dplyr::mutate(recurso_ja_foi_julgado = forcats::fct_infreq(recurso_ja_foi_julgado)) |>
  dplyr::filter(!is.na(recurso_ja_foi_julgado)) |>
  dplyr::count(recurso_ja_foi_julgado) |>
  dplyr::mutate(
    prop = n/sum(n),
    perc = formattable::percent(prop),
    col_dif = recurso_ja_foi_julgado == "Não consta"
  ) |>
  ggplot2::ggplot() +
  ggplot2::aes(x = recurso_ja_foi_julgado, y = n, label = perc) +
  ggplot2::geom_col(ggplot2::aes(fill = col_dif), show.legend = FALSE) +
  ggplot2::geom_label() +
  ggplot2::scale_fill_manual(values = c(cores_abj[1], "gray70")) +
  ggplot2::labs(
    title = glue::glue("Julgamento dos recursos (N = {n_teve_recurso})"),
    x = "O recurso já foi julgado?",
    y = "Quantidade de processos"
  )

ggplot2::ggsave(
  "data-raw/bruno-nassar-puc/img/p22d.png",
  p22d, width = 8, height = 5
)

# e) Coluna BJ: nos casos em que o recurso já foi julgado (resposta “sim” na coluna BI), qual o lapso temporal entre a data da sentença (coluna N) e a data do julgamento do recurso -----------------------------------------------------------------------
n_teve_julgamento_recurso <- da |>
  dplyr::filter(recurso_ja_foi_julgado == "Sim") |>
  nrow()

p22e <- da |>
  dplyr::filter(recurso_ja_foi_julgado == "Sim") |>
  dplyr::select(dt_plenario, dt_acordao) |>
  dplyr::mutate(tempo_sentenca_acordao = as.numeric(dt_acordao - dt_plenario)) |>
  ggplot2::ggplot() +
  ggplot2::aes(x = tempo_sentenca_acordao) +
  ggplot2::geom_histogram(fill = cores_abj[1], bins = 10) +
  ggplot2::labs(
    title = glue::glue("Tempo entre a sentença e o acórdão (N = {n_teve_julgamento_recurso})"),
    x = "Tempo (dias)",
    y = "Quantidade de processos"
  )

ggplot2::ggsave(
  "data-raw/bruno-nassar-puc/img/p22e.png",
  p22e, width = 10, height = 5
)
# f) Coluna BK: nos casos em que o recurso já foi julgado (resposta “sim” na coluna BI), qual foi o resultado do acórdão? -----------------------------------------------------------------------
p22f <- da |>
  dplyr::filter(recurso_ja_foi_julgado == "Sim") |>
  dplyr::mutate(
    decisao_acordao = ifelse(decisao_acordao == "Parcialmente improvido", "Parcialmente provido", decisao_acordao)
  ) |>
  dplyr::mutate(decisao_acordao = forcats::fct_infreq(decisao_acordao)) |>
  dplyr::count(decisao_acordao) |>
  dplyr::mutate(
    prop = n/sum(n),
    perc = formattable::percent(prop)
  ) |>
  ggplot2::ggplot() +
  ggplot2::aes(x = decisao_acordao, y = n, label = perc) +
  ggplot2::geom_col(fill = cores_abj[1]) +
  ggplot2::geom_label() +
  ggplot2::labs(
    title = glue::glue("Resultado dos acórdãos (N = {n_teve_julgamento_recurso})"),
    x = "Resultado do acórdão",
    y = "Quantidade de processos"
  )

ggplot2::ggsave(
  "data-raw/bruno-nassar-puc/img/p22f.png",
  p22f, width = 7, height = 6
)

# g) Nos casos em que o recurso foi provido ou parcialmente provido, indicar em quantos deles foi alterada a pena (coluna BL) e/ou o regime de cumprimento -----------------------------------------------------------------------
n22g <- da |>
  dplyr::mutate(
    decisao_acordao = ifelse(decisao_acordao == "Parcialmente improvido", "Parcialmente provido", decisao_acordao)
  ) |>
  dplyr::filter(recurso_ja_foi_julgado == "Sim") |>
  dplyr::filter(decisao_acordao == "Parcialmente provido") |>
  nrow()

p22g <- da |>
  dplyr::filter(recurso_ja_foi_julgado == "Sim") |>
  dplyr::mutate(
    alteracao = dplyr::case_when(
      !is.na(alteracao_regime) ~ "Sim",
      !is.na(alteracao_pena) ~ "Sim",
      TRUE ~ "Não"
    ),
    decisao_acordao = ifelse(decisao_acordao == "Parcialmente improvido", "Parcialmente provido", decisao_acordao)
  ) |>
  dplyr::filter(decisao_acordao == "Parcialmente provido") |>
  dplyr::mutate(alteracao = forcats::fct_infreq(alteracao)) |>
  grafico_base(alteracao) +
  ggplot2::labs(
    title = glue::glue("Acórdãos que tiveram alteração de sentença (N = {n22g})"),
    x = "Houve alteração de sentença?",
    y = "Quantidade de processos"
  )

ggplot2::ggsave(
  "data-raw/bruno-nassar-puc/img/p22g.png",
  p22g, width = 7, height = 6
)

# h) (coluna BM) e para qual patamar em cada caso -----------------------------------------------------------------------
n22h_pena <- da |>
  dplyr::filter(!is.na(alteracao_pena)) |>
  nrow()

n22h_regime <- da |>
  dplyr::filter(!is.na(alteracao_regime)) |>
  nrow()

p22h_pena <- da |>
  dplyr::mutate(
    decisao_acordao = ifelse(decisao_acordao == "Parcialmente improvido", "Parcialmente provido", decisao_acordao),
    tempo_pena = dplyr::case_when(
      tempo_pena == "Inferior a 2 anos" ~ "Pena inicial: Até 2 anos",
      tempo_pena == "Igual a 2 anos e que não exceda 4 anos" ~ "Pena inicial: Entre 2 e 4 anos",
      tempo_pena == "Superior a 4 anos e que não exceda 8 anos" ~ "Pena inicial: Entre 4 e 8 anos",
      tempo_pena == "Superior a 8 anos e inferior a 12 anos" ~ "Pena inicial: Entre 8 e 12 anos",
      tempo_pena == "Igual a 12 anos e inferior a 15 anos" ~ "Pena inicial: Entre 12 e 15 anos",
      tempo_pena == "Igual a 15 anos e inferior a 20 anos" ~ "Pena inicial: Entre 15 e 20 anos",
      tempo_pena == "Igual a 20 anos e inferior a 30 anos" ~ "Pena inicial: Entre 20 e 30 anos",
      tempo_pena == "Igual ou superior a 30 anos" ~ "Pena inicial: Igual ou superior a 30 anos"
    ),
    tempo_pena = factor(
      tempo_pena,
      levels = c(
        "Pena inicial: Até dois anos",
        "Pena inicial: Entre 2 e 4 anos",
        "Pena inicial: Entre 4 e 8 anos",
        "Pena inicial: Entre 8 e 12 anos",
        "Pena inicial: Entre 12 e 15 anos",
        "Pena inicial: Entre 15 e 20 anos",
        "Pena inicial: Entre 20 e 30 anos",
        "Pena inicial: Igual ou superior a 30 anos"
      )
    ),
    alteracao_pena = dplyr::case_when(
      alteracao_pena == "Inferior a 2 anos" ~ "Até 2 anos",
      alteracao_pena == "Igual a 2 anos e que não exceda 4 anos" ~ "Entre 2 e 4 anos",
      alteracao_pena == "Superior a 4 anos e que não exceda 8 anos" ~ "Entre 4 e 8 anos",
      alteracao_pena == "Superior a 8 anos e inferior a 12 anos" ~ "Entre 8 e 12 anos",
      alteracao_pena == "Igual a 12 anos e inferior a 15 anos" ~ "Entre 12 e 15 anos",
      alteracao_pena == "Igual a 15 anos e inferior a 20 anos" ~ "Entre 15 e 20 anos",
      alteracao_pena == "Igual a 20 anos e inferior a 30 anos" ~ "Entre 20 e 30 anos",
      alteracao_pena == "Igual ou superior a 30 anos" ~ "Igual ou superior a 30 anos"
    ),
    alteracao_pena = factor(
      alteracao_pena,
      levels = c(
        "Até dois anos",
        "Entre 2 e 4 anos",
        "Entre 4 e 8 anos",
        "Entre 8 e 12 anos",
        "Entre 12 e 15 anos",
        "Entre 15 e 20 anos",
        "Entre 20 e 30 anos",
        "Igual ou superior a 30 anos"
      )
    )
  ) |>
  dplyr::filter(recurso_ja_foi_julgado == "Sim") |>
  dplyr::filter(decisao_acordao == "Parcialmente provido") |>
  dplyr::filter(!is.na(alteracao_pena)) |>
  # dplyr::mutate(
  #   alteracao_pena = glue::glue("{tempo_pena} -> {alteracao_pena}")
  # ) |>
  dplyr::count(tempo_pena, alteracao_pena) |>
  dplyr::mutate(
    prop= n/sum(n),
    perc = formattable::percent(prop)
  ) |>
  ggplot2::ggplot() +
  ggplot2::aes(x = alteracao_pena, y = n, label = perc) +
  ggplot2::geom_col(fill = cores_abj[1]) +
  ggplot2::facet_wrap(~tempo_pena, scale = "free") +
  ggplot2::scale_x_discrete(labels=scales::label_wrap(20)) +
  ggplot2::scale_y_continuous(breaks = c(0,1)) +
  ggplot2::labs(
    title = glue::glue("Alteração do tempo de pena em relação ao tempo inicial (N = {n22h_pena})"),
    x = "Tempo de pena novo",
    y = "Quantidade de processos"
  )

p22h_regime <- da |>
  dplyr::mutate(
    decisao_acordao = ifelse(decisao_acordao == "Parcialmente improvido", "Parcialmente provido", decisao_acordao),
    regime_inicial = paste0("Regime inicial: ", regime_inicial),
    regime_inicial = forcats::fct_infreq(regime_inicial),
    aleracao_regime = forcats::fct_infreq(alteracao_regime)
  ) |>
  dplyr::filter(recurso_ja_foi_julgado == "Sim") |>
  dplyr::filter(decisao_acordao == "Parcialmente provido") |>
  dplyr::filter(!is.na(alteracao_regime)) |>
  dplyr::count(regime_inicial, alteracao_regime) |>
  dplyr::mutate(
    prop= n/sum(n),
    perc = formattable::percent(prop)
  ) |>
  ggplot2::ggplot() +
  ggplot2::aes(x = alteracao_regime, y = n, label = perc) +
  ggplot2::geom_col(fill = cores_abj[1]) +
  ggplot2::facet_wrap(~regime_inicial, scale = "free") +
  ggplot2::scale_x_discrete(labels=scales::label_wrap(20)) +
  ggplot2::scale_y_continuous(breaks = c(0,1)) +
  ggplot2::labs(
    title = glue::glue("Alteração do regime de pena em relação ao regime inicial (N = {n22h_pena})"),
    x = "Regime de cumprimento da pena novo",
    y = "Quantidade de processos"
  )

p22h <- gridExtra::grid.arrange(p22h_pena, p22h_regime)

ggplot2::ggsave(
  "data-raw/bruno-nassar-puc/img/p22h.png",
  p22h, width = 9, height = 6
)

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
