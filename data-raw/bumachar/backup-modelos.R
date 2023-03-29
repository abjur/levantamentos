# III.B - sobrevivencia -----------------------------------------------------------

# 1) tempo da decretação e publicação da lista do AJ (artigo 7º)  -----------------------------------------------------------
# tempo_decretacao_lista_aj = dt_listcred_aj - dt_decisao

tempo_decretacao_lista_aj <- da |>
  dplyr::filter(info_fal_dec == "Sim") |>
  dplyr::transmute(
  id_processo,
  dt_inicio = dt_decisao,
  dt_listcred_aj,
  dt_fim = dplyr::coalesce(dt_listcred_aj, data_hora)
) |>
  dplyr::mutate(
    morreu = as.integer(!is.na(dt_listcred_aj)),
    tempo = dt_fim - dt_inicio,
  ) |>
  dplyr::filter(
    morreu == 1 | tempo < 365.25*5,
    tempo < 365*10,
    tempo > 0
  )

m_total <- survival::survfit(
  survival::Surv(tempo, morreu) ~ 1,
  data = da_tempo_decretacao_lista_aj
)

tempo_decretacao_lista_aj <- broom::glance(m_total) |>
  purrr::pluck("median")

# 2) tempo da decretação até avaliação  -----------------------------------------------------------
# tempo_decretacao_avaliacao = dt_avaliacao - dt_decisao

da_tempo_decretacao_avaliacao <- da |>
  dplyr::filter(info_fal_dec == "Sim") |>
  dplyr::transmute(
    id_processo,
    dt_inicio = dt_decisao,
    dt_avaliacao,
    dt_fim = dplyr::coalesce(dt_avaliacao, data_hora)
  ) |>
  dplyr::mutate(
    morreu = as.integer(!is.na(dt_avaliacao)),
    tempo = dt_fim - dt_inicio,
  ) |>
  dplyr::filter(
    morreu == 1 | tempo < 365.25*5,
    tempo < 365*10,
    tempo > 0
  )

m_total <- survival::survfit(
  survival::Surv(tempo, morreu) ~ 1,
  data = da_tempo_decretacao_avaliacao
)

tempo_decretacao_avaliacao <- broom::glance(m_total) |>
  purrr::pluck("median")

# 3) tempo da decretação até a liquidação do 1º ativo  -----------------------------------------------------------
# tempo_decretacao_liquidacao = dt_leilao - dt_decisao

da_tempo_decretacao_liquidacao <- da |>
  dplyr::filter(info_fal_dec == "Sim") |>
  dplyr::transmute(
    id_processo,
    dt_inicio = dt_decisao,
    dt_leilao,
    dt_fim = dplyr::coalesce(dt_leilao, data_hora)
  ) |>
  dplyr::mutate(
    morreu = as.integer(!is.dt_leilao(dt_listcred_aj)),
    tempo = dt_fim - dt_inicio,
  ) |>
  dplyr::filter(
    morreu == 1 | tempo < 365.25*5,
    tempo < 365*10,
    tempo > 0
  )

m_total <- survival::survfit(
  survival::Surv(tempo, morreu) ~ 1,
  data = da_tempo_decretacao_liquidacao
)

tempo_decretacao_liquidacao <- broom::glance(m_total) |>
  purrr::pluck("median")

# 4) tempo da decretação para o encerramento  -----------------------------------------------------------
# tempo_decretacao_encerramento = dt_extincao - dt_decisao,

da_tempo_decretacao_encerramento <-# 1) tempo da decretação e publicação da lista do AJ (artigo 7º)  -----------------------------------------------------------
# tempo_decretacao_lista_aj = dt_listcred_aj - dt_decisao

tempo_decretacao_lista_aj <- da |>
  dplyr::filter(
    info_fal_dec == "Sim",
    info_fal_extin_caucao == "Não"
  ) |>
  sobrevivencia(dt_listcred_aj)

# 2) tempo da decretação até avaliação  -----------------------------------------------------------
# tempo_decretacao_avaliacao = dt_avaliacao - dt_decisao

tempo_decretacao_avaliacao <- da |>
  dplyr::filter(
    info_fal_dec == "Sim",
    info_fal_extin_caucao == "Não"
  ) |>
  sobrevivencia(dt_avaliacao)

# 3) tempo da decretação até a liquidação do 1º ativo  -----------------------------------------------------------
# tempo_decretacao_liquidacao = dt_leilao - dt_decisao

tempo_decretacao_liquidacao <- da |>
  dplyr::filter(
    info_fal_dec == "Sim",
    info_fal_extin_caucao == "Não"
  ) |>
  sobrevivencia(dt_leilao)

# 4) tempo da decretação para o encerramento  -----------------------------------------------------------
# tempo_decretacao_encerramento = dt_extincao - dt_decisao,

tempo_decretacao_encerramento <- da |>
  dplyr::filter(
    info_fal_dec == "Sim",
    # info_fal_extin_caucao == "Não"
  ) |>
  dplyr::transmute(
    id_processo,
    dt_inicio = dt_decisao,
    dt_extincao,
    dt_fim = dplyr::coalesce(dt_extincao, data_hora)
  ) |>
  dplyr::mutate(
    morreu = as.integer(!is.na(dt_extincao)),
    tempo = dt_fim - dt_inicio,
  ) |>
  dplyr::filter(
    morreu == 1 | tempo < 365.25*5,
    tempo < 365*10,
    tempo > 0
  )

m_total <- survival::survfit(
  survival::Surv(tempo, morreu) ~ 1,
  data = da_tempo_decretacao_encerramento
)

tempo_decretacao_encerramento <- broom::glance(m_total) |>
  purrr::pluck("median")

