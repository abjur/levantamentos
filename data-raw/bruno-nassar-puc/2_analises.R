# preparacao --------------------------------------------------------------
da <- readr::read_rds("data-raw/bruno-nassar-puc/data/homicidios_tidy.rds")
cores_abj <-  viridis::viridis(2, 1, .2, .8)

# 1 – Coluna C: qual a distribuição de processos entre as varas? =========================================================================
da |>
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
    x = "Ano de distribuição",
    y = "Quantidade de processos"
  )
# 2 – Coluna D: qual a distribuição de processos eletrônicos e físicos? =========================================================================
da |>
  dplyr::count(digital) |>
  dplyr::mutate(
    prop = n/sum(n),
    perc = formattable::percent(prop)
  ) |>
  dplyr::select(-prop) |>
  knitr::kable(
    caption = "Distribuição de processos digitais",
    col.names = c(
      "Processo é digital?",
      "Quantidade de processos",
      "Porcentagem"
    )
  )

# 3 – Coluna E: qual a temática de cada processo. =========================================================================
da |>
  dplyr::glimpse()
# Como fazer isso?

# 4 – Coluna H: qual a distribuição de horários do fato? =========================================================================
da |>
  dplyr::filter(!is.na(parte_do_dia_fato)) |>
  dplyr::count(parte_do_dia_fato) |>
  dplyr::mutate(parte_do_dia_fato = factor(parte_do_dia_fato, levels = c("Madrugada (00:01 às 06:00)", "Manhã (06:01 às 12:00)", "Tarde (12:01 às 18:00)", "Noite (18:01 às 00:00)", "Não consta"))) |>
  ggplot2::ggplot() +
  ggplot2::aes(x = parte_do_dia_fato, y =n) +
  ggplot2::geom_col(fill = cores_abj[1]) +
  ggplot2::labs(
    x = "Parte do dia",
    y = "Quantidade de casos"
  )

# 5 – Coluna O: quantos processos foram suspensos com base no art. 366. =========================================================================

# 6 – Coluna U: quantos casos tiveram concurso com menor de idade? =========================================================================
da |>
  dplyr::count(concurso_menor)
# 7 – Coluna AA: qual a distribuição de número de réus julgados? =========================================================================

# 8 – Coluna AB: qual a distribuição no que diz respeito ao sexo dos réus? =========================================================================

# 9 – Coluna AC: qual a distribuição no que diz respeito à cor dos réus? =========================================================================

# 10 – Coluna AD: qual a distribuição no que diz respeito a se o réu é policial? =========================================================================

# 11 – Coluna AE: qual a distribuição no que diz respeito à quantidade de vítimas? =========================================================================

# 12 – Coluna AF: qual a distribuição no que diz respeito ao sexo das vítimas? =========================================================================

# 13 – Coluna AG - qual a distribuição no que diz respeito à delegacia que desenvolveu o IP? =========================================================================

# 14 – Coluna AH: qual a distribuição no que diz respeito à natureza da decisão? =========================================================================

# 15 – Coluna AI: qual a distribuição no que diz respeito a se o processo foi adiado pela pandemia? =========================================================================

# 16 – Coluna AJ (processos com resposta “absolvição sumária” na coluna AH): qual a distribuição das causas de absolvição sumária? =========================================================================

# 17 – Coluna AK (processos com resposta “extinção da punibilidade” na coluna AH): qual a distribuição das causas de extinção da punibilidade? =========================================================================

# 18 – Processos que tiveram pronúncia (resposta “sentença de pronúncia” ou “sentença após plenário do júri” na coluna AH): =========================================================================
# a) Coluna AL: quantos casos tiveram uma dessas decisões convertida em pronúncia? -----------------------------------------------------------------------
# b) Coluna AO: quantos casos tiveram RESE? -----------------------------------------------------------------------
# c) Coluna AP: para os casos que tiveram como resposta “sim” na coluna AO, medir o tempo entre a sentença de pronúncia (coluna M) e a decisão sobre o RESE (coluna AP). Para esses casos, medir também o tempo entre a sentença de pronúncia (coluna M) e o plenário (coluna N. Mas só se já houve plenário, ou seja, casos com resposta “sentença após plenário do júri” na coluna AH”). -----------------------------------------------------------------------

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
