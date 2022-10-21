# Luiz Fonseca
# base_rjsp ---------------------------------------------------------------
da_rjsp <- obsFase2::da_relatorio

processos_sp <- da_rjsp |>
  dplyr::filter(cram_down == "Sim" | resultado_final == "Plano foi reprovado") |>
  dplyr::mutate(
    plano_desfecho = dplyr::case_when(
      cram_down == "Sim" ~ "Cram Down",
      TRUE ~ "O plano foi reprovado (a empresa/grupo faliu)"
    )
  ) |>
  dplyr::transmute(
    id_processo = n_processo,
    tj = "sp",
    dt_dist = data_dist2,
    comarca,
    tipo_vara = dplyr::case_when(
      capital == "Sim" ~ "Especializada",
      TRUE ~ "Comum"
    ),
    total_passivo,
    faturamento = faturamento_total,
    faixa_faturamento,
    plano_desfecho
  )

# base_rjsj ---------------------------------------------------------------
da_rjrj <- obsRJRJ::da_processo_tidy

filtros_rjrj <- c(
  unique(da_rjrj$plano_desfecho)[2],
  unique(da_rjrj$plano_desfecho)[5]
)

processos_rj <- da_rjrj |>
  dplyr::filter(plano_desfecho %in% filtros_rjrj) |>
  dplyr::transmute(
    id_processo,
    tj = "rj",
    dt_dist = data_dist,
    comarca,
    tipo_vara = dplyr::case_when(
      capital == "Sim" ~ "Especializada",
      TRUE ~ "Comum"
    ),
    faturamento,
    faixa_faturamento,
    total_passivo = passivos,
    plano_desfecho
  )

# base_rjrs ---------------------------------------------------------------
da_rjrs <- obsRJRS::da_processo_tidy

filtros_rjrs <- c(
  unique(da_rjrs$plano_desfecho)[4],
  unique(da_rjrs$plano_desfecho)[5]
)

processos_rs <- da_rjrs |>
  dplyr::filter(plano_desfecho %in% filtros_rjrs) |>
  dplyr::transmute(
    id_processo,
    tj = "rs",
    dt_dist = data_dist,
    comarca = stringr::str_to_title(comarca),
    comarca = stringr::str_replace(comarca, "Do", "do"),
    comarca = stringr::str_replace(comarca, "Da", "da"),
    comarca = stringr::str_replace(comarca, "De", "de"),
    comarca = stringr::str_replace(comarca, "Dos", "dos"),
    comarca = stringr::str_replace(comarca, "Das", "das"),
    tipo_vara = dplyr::case_when(
      capital == "Sim" ~ "Especializada",
      TRUE ~ "Comum"
    ),
    faturamento,
    faixa_faturamento,
    total_passivo = passivos,
    plano_desfecho
  )

# join --------------------------------------------------------------------

processos <- dplyr::bind_rows(processos_sp, processos_rj, processos_rs) |>
  dplyr::arrange(desc(tj), dt_dist)


# glossario ---------------------------------------------------------------
colunas <- processos |>
  colnames()

descricoes <- c(
  "Número do processo",
  "Tribunal de Justiça ('SP', 'RJ', 'RS')",
  "Data de distribuição",
  "Comarca",
  "Tipo de vara ('Especializada', 'Comum')",
  "Valor total do passivo da(s) empresa(s) em recuperação",
  "Faturamento total da(s) empresa(s)",
  "Faixa de faturamento",
  "Plano do desfecho ('Cram Down', 'O plano foi reprovado (a empresa/grupo faliu)')"
)

glossario <- tibble::tibble(
  colunas, descricoes
)
# sheets ------------------------------------------------------------------

link <- "https://docs.google.com/spreadsheets/d/1W5DG9DAfEoEIY7RPa9zeXytfVQx6GV4bQXZC6EjZeB4/edit#gid=0"

googlesheets4::gs4_auth("rfeliz@abj.org.br")

googlesheets4::write_sheet(
  processos,
  link,
  "processos"
)

googlesheets4::write_sheet(
  glossario,
  link,
  "glossario"
)
