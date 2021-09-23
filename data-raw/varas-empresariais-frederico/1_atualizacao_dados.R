library(magrittr)


# carregando os indices ---------------------------------------------------

path_2020 <- "/mnt/dados/abj/paginas/TJSP_dia/txt"
path_2021 <- "/mnt/dados/abj/paginas/gcp/txt"
arquivos_2020 <- fs::dir_ls(path_2020, recurse = TRUE, type = "file", regexp = "_12\\.txt")
arquivos_2021 <- fs::dir_ls(path_2021, recurse = TRUE, type = "file", regexp = "_12\\.txt")
arquivos <- c(arquivos_2020, arquivos_2021)

safe <- purrr::possibly(lex::tjsp_dj_parse, tibble::tibble(erro = "erro"))

progressr::with_progress({
  p <- progressr::progressor(length(arquivos))
  da_indice <- purrr::map_dfr(arquivos , ~{
    p()
    safe(.x)
  }, .id = "file")
})

readr::write_rds(
  da_indice,
  "data-raw/varas-empresariais-frederico/da_indice.rds",
  compress = "xz"
)

# obtendo as paginas potencialmente empresariais --------------------------

da_arquivos <- da_indice %>%
  dplyr::filter(is.na(erro)) %>%
  dplyr::select(-erro) %>%
  dplyr::group_by(file) %>%
  # vamos fazer o intervalo entre a pagina que começa aquela parte e a próxima
  dplyr::mutate(proxima_pag = dplyr::lead(pagina)) %>%
  dplyr::ungroup() %>%
  dplyr::filter(stringr::str_detect(titulo, "Empresa")) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(
    dplyr::across(c(pagina, proxima_pag), as.numeric),
    page = list(pagina:proxima_pag)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::select(id = file, page) %>%
  tidyr::unnest(page)

files_filtrado <- da_indice %>%
  with(unique(file))

pages <- files_filtrado %>%
  lex:::build_pages()

da_paginas_filtrado <- pages %>%
  dplyr::semi_join(da_arquivos, c("id", "page"))

da_processos_baixar <- da_paginas_filtrado %>%
  dplyr::transmute(
    processos = stringr::str_extract_all(txt, abjutils::pattern_cnj())
  ) %>%
  tidyr::unnest(processos) %>%
  dplyr::mutate(
    processos = abjutils::clean_cnj(processos),
    processos = stringr::str_pad(processos, 20, "left", "0")
  ) %>%
  dplyr::distinct(processos)


path_cpopg <- "data-raw/varas-empresariais-frederico/bases/cpopg_atualizacao"
ids <- da_processos_baixar$processos
progressr::with_progress({
  p <- progressr::progressor(length(ids))
  purrr::walk(ids , ~{
    p()
    lex::tjsp_cpopg_download(.x, path_cpopg)
  })
})

# parse dos processos -----------------------------------------------------

files_cpopg <- fs::dir_ls(path_cpopg)

progressr::with_progress({
  p <- progressr::progressor(length(files_cpopg))
  da_cpopg <- purrr::map_dfr(files_cpopg , ~{
    p()
    lex::tjsp_cpopg_parse(.x)
  }, .id = "file")
})

readr::write_rds(
  da_cpopg,
  "data-raw/varas-empresariais-frederico/da_cpopg_atualizacao_raw.rds",
  compress = "xz"
)

# filtro empresarial ------------------------------------------------------

da_cpopg_empresarial <- da_cpopg %>%
  dplyr::filter(stringr::str_detect(vara, "EMPR")) %>%
  dplyr::select(-erro)

readr::write_rds(
  da_cpopg_empresarial,
  "data-raw/varas-empresariais-frederico/da_cpopg_atualizacao_emp.rds",
  compress = "xz"
)

dim(processos_originais)
dim(da_cpopg_empresarial)

# bind_rows com base original ---------------------------------------------

processos_originais <- "data-raw/varas-empresariais-frederico/processos_empresariais.xlsx" %>%
  readxl::read_excel()

processos_originais %>%
  janitor::remove_empty("cols") %>%
  dplyr::glimpse()

da_cpopg_empresarial %>%
  dplyr::select(
    arq = file,
    id_processo,
    status,
    area,
    assunto,
    controle,
    processo = processo_principal,
    digital,
    classe,
    data = distribuicao,
    vara,
    juiz,
    local_fisico,
    valor_da_acao,
    outros_numeros,
    dados_da_precatoria,
    outros_assuntos,
    apensado_ao,
    entranhado_ao
  ) %>%
  dplyr::distinct(id_processo, .keep_all = TRUE)

da_completa <- dplyr::bind_rows(da_cpopg_empresarial, processos_originais) %>%
  dplyr::distinct(id_processo, .keep_all = TRUE)

readr::write_rds(
  da_completa,
  "data-raw/varas-empresariais-frederico/da_completa.rds",
  compress = "xz"
)

writexl::write_xlsx(
  da_completa,
  "data-raw/varas-empresariais-frederico/processos_empresariais_atualizado.xlsx"
)

# sanity checks -----------------------------------------------------------

da_completa %>%
  dplyr::mutate(
    dt_dist = lubridate::dmy(stringr::str_sub(distribuicao, 1L, 10L)),
    dt_dist = lubridate::floor_date(dt_dist, "week")
  ) %>%
  dplyr::count(dt_dist) %>%
  dplyr::filter(!is.na(dt_dist)) %>%
  ggplot2::ggplot() +
  ggplot2::aes(dt_dist, n) +
  ggplot2::geom_line() +
  ggplot2::theme_minimal()

