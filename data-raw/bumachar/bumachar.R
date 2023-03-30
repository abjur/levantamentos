# instruções
# - Dados de falência:
#   tempo da decretação e publicação da lista do AJ (artigo 7º);
#   tempo da decretação até avaliação;
#   tempo da decretação até a liquidação do 1º ativo;
#   tempo da decretação para o encerramento;
# Tentar tirar confissão (iniciativa do credor / convolação);
# eventualmente dividir iniciativa / convolação.


# I - preparacao --------------------------------------------------------------------
# base
da_avaliacao <- obsFase3::da_avaliacao_tidy |>
  dplyr::group_by(id_processo) |>
  dplyr::mutate(
    dt_avaliacao = dplyr::coalesce(data_laudo, data_auto)
  ) |>
  dplyr::summarise(
    dt_avaliacao = min(dt_avaliacao)
  ) |>
  dplyr::ungroup() |>
  dplyr::filter(!is.na(dt_avaliacao))

da_leilao <- obsFase3::da_leilao_tidy |>
  dplyr::group_by(id_processo) |>
  dplyr::summarise(
    dt_leilao = min(data_edital)
  ) |>
  dplyr::ungroup() |>
  dplyr::filter(!is.na(dt_leilao))

da <- obsFase3::da_processo_tidy |>
  dplyr::filter(info_origem_min != "Autofalência") |>
  dplyr::left_join(da_avaliacao) |> # dt de avaliacao
  dplyr::left_join(da_leilao) |> # dt de leilao
  dplyr::mutate(
    dt_extincao = dplyr::coalesce(dt_fal_fim, dt_extincao),
    tempo_decretacao_lista_aj = dt_listcred_aj - dt_decisao,
    tempo_decretacao_avaliacao = dt_avaliacao - dt_decisao,
    tempo_decretacao_liquidacao = dt_leilao - dt_decisao,
    tempo_decretacao_encerramento = dt_extincao - dt_decisao,
    dplyr::across(
      dplyr::contains("tempo_"),
      ~ifelse(.x < 0, NA_real_, .x)
    )
  )

# função
sobrevivencia <- function(da, var) {

  da_tempo <- da |>
    dplyr::filter(info_fal_dec == "Sim") |>
    dplyr::transmute(
      id_processo,
      dt_inicio = dt_decisao,
      {{var}},
      dt_fim = dplyr::coalesce({{var}}, data_hora)
    ) |>
    dplyr::mutate(
      morreu = as.integer(!is.na({{var}})),
      tempo = dt_fim - dt_inicio,
    ) |>
    dplyr::filter(
      morreu == 1 | tempo < 365.25*5,
      tempo < 365*10,
      tempo > 0
    )

  m_total <- survival::survfit(
    survival::Surv(tempo, morreu) ~ 1,
    data = da_tempo
  )

  tempo <- broom::glance(m_total) |>
    purrr::pluck("median")
}


# II - resumos (mediana) -------------------------------------------------------

da_resumo_todos <- da |>
  dplyr::summarise(
    info_origem_min = "Credor / Convolação",
    tempo_decretacao_lista_aj = median(tempo_decretacao_lista_aj, na.rm = TRUE),
    tempo_decretacao_avaliacao = median(tempo_decretacao_avaliacao, na.rm = TRUE),
    tempo_decretacao_liquidacao = median(tempo_decretacao_liquidacao, na.rm = TRUE),
    tempo_decretacao_encerramento = median(tempo_decretacao_encerramento, na.rm = TRUE)
)

da_mediana_resumo <- da |>
  dplyr::group_by(info_origem_min) |>
  dplyr::summarise(
    tempo_decretacao_lista_aj = median(tempo_decretacao_lista_aj, na.rm = TRUE),
    tempo_decretacao_avaliacao = median(tempo_decretacao_avaliacao, na.rm = TRUE),
    tempo_decretacao_liquidacao = median(tempo_decretacao_liquidacao, na.rm = TRUE),
    tempo_decretacao_encerramento = median(tempo_decretacao_encerramento, na.rm = TRUE)
  ) |>
  dplyr::ungroup() |>
  dplyr::bind_rows(da_resumo_todos) |>
  dplyr::mutate(
    dplyr::across(
      dplyr::contains("tempo_"),
      ~abjDash::tempo_lab(.x/365.25)
      # ~paste0(.x, " dias")
    )
  ) # |>
  # knitr::kable(
  #   col.names = c(
  #     "Tipo de falência",
  #     "Tempo decretação - lista mais recente do AJ",
  #     "Tempo decretação - 1a avaliação",
  #     "Tempo decretação - 1a liquidação",
  #     "Tempo decretação - encerramento"
  #   )
  # )

# III.A - sobrevivencia (credor) -----------------------------------------------------------

da_credor <- da |>
  dplyr::filter(
    info_origem_min == "Credor",
    info_fal_dec == "Sim",
    info_fal_extin_caucao == "Não",
    info_leilao_justif_min != "Arrecadação de bens negativa"
  )

da_sobrevivencia_credor <- tibble::tibble(
  info_origem_min = "Credor",
  n = nrow(da_credor),
  tempo_decretacao_lista_aj = sobrevivencia(da_credor, dt_listcred_aj),
  tempo_decretacao_avaliacao = sobrevivencia(da_credor, dt_avaliacao),
  tempo_decretacao_liquidacao = sobrevivencia(da_credor, dt_leilao),
  tempo_decretacao_encerramento = sobrevivencia(da_credor, dt_extincao)
) |>
  dplyr::mutate(dplyr::across(
    dplyr::contains("tempo_"),
    ~abjDash::tempo_lab(.x/365.25)
  ))

# III.B - sobrevivencia (convolação) -----------------------------------------------------------

da_conv <- da |>
  dplyr::filter(
    info_origem_min == "Convolação",
    info_leilao_justif_min != "Arrecadação de bens negativa"
  )

da_sobrevivencia_convolacao <- tibble::tibble(
  info_origem_min = "Convolação",
  n = nrow(da_conv),
  tempo_decretacao_lista_aj = sobrevivencia(da_conv, dt_listcred_aj),
  tempo_decretacao_avaliacao = sobrevivencia(da_conv, dt_avaliacao),
  tempo_decretacao_liquidacao = sobrevivencia(da_conv, dt_leilao),
  tempo_decretacao_encerramento = sobrevivencia(da_conv, dt_extincao)
) |>
  dplyr::mutate(dplyr::across(
    dplyr::contains("tempo_"),
    ~abjDash::tempo_lab(.x/365.25)
  ))

# III.C - sobrevivencia (todos) -----------------------------------------------------------
da_todos <- da |>
  dplyr::mutate(
    manter = dplyr::case_when(
      # condições do credor
      info_origem_min == "Credor" &
      info_fal_dec == "Sim" &
      info_fal_extin_caucao == "Não" &
      info_leilao_justif_min != "Arrecadação de bens negativa" ~ TRUE,
      # condições da conv
      info_origem_min == "Convolação" &
      info_leilao_justif_min != "Arrecadação de bens negativa" ~ TRUE,
      TRUE ~ FALSE
    )
  ) |>
  dplyr::filter(manter)

da_sobrevivencia_todos <- tibble::tibble(
  info_origem_min = "Credor / Convolação",
  n = nrow(da_todos),
  tempo_decretacao_lista_aj = sobrevivencia(da_todos, dt_listcred_aj),
  tempo_decretacao_avaliacao = sobrevivencia(da_todos, dt_avaliacao),
  tempo_decretacao_liquidacao = sobrevivencia(da_todos, dt_leilao),
  tempo_decretacao_encerramento = sobrevivencia(da_todos, dt_extincao)
) |>
  dplyr::mutate(dplyr::across(
    dplyr::contains("tempo_"),
    ~abjDash::tempo_lab(.x/365.25)
  ))



# III.D - sobrevivencia (geral) ------------------------------------------
da_sobrevivencia_resumo <- da_sobrevivencia_credor |>
  dplyr::bind_rows(da_sobrevivencia_convolacao) |>
  dplyr::bind_rows(da_sobrevivencia_todos)
