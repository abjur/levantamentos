# sabe a base de falencias, vc acha q é facil levantar:
#   as 100 maiores
# as 100 mais demoradas
# .A gente consegue as
# 10 falências mais antigas que não foram concluídas (e o principal credor),
# as 10 com maiores valores envolvidos (e o principal credor)


# analise valor -----------------------------------------------------------

valores <- obsFase3::da_processo_tidy  |>
  dplyr::filter(info_origem_min == "Credor") |>
  dplyr::transmute(
    id_processo,
    parte_reqte_cred,
    passivo = dplyr::coalesce(listcred_aj_val, listcred_devedor_val),
    info_fal_dec_min,
    info_fal_extin_caucao
  ) |>
  dplyr::filter(info_fal_dec_min %in% c("Sim", "Não")) |>
  dplyr::mutate(
    inco = parte_reqte_cred > 1e9,
    parte_reqte_cred = abs(parte_reqte_cred)
  ) |>
  dplyr::filter(is.na(inco) | !inco) |>
  dplyr::filter(!is.na(passivo)) |>
  dplyr::arrange(desc(passivo)) |>
  head(100) |>
  dplyr::transmute(
    id_processo,
    passivo = formattable::currency(passivo, symbol = "R$ ",
                                    big.mark = ".", decimal.mark = ",")
  )

writexl::write_xlsx(valores, "data-raw/valor/xlsx/valores.xlsx")

# analise tempo -----------------------------------------------------------
# as 100 mais demoradas

mais_demorados <- obsFase3::da_processo_tidy |>
  dplyr::filter(dt_dist >= lubridate::dmy("01/01/2010")) |>
  dplyr::mutate(
    dt_fim = dplyr::coalesce(dt_fal_fim, dt_extincao)
  ) |>
  dplyr::filter(!is.na(dt_fim)) |>
  dplyr::mutate(
    tempo = dt_fim - dt_dist
  ) |>
  dplyr::arrange(desc(tempo)) |>
  head(100) |>
  dplyr::select(
    id_processo,
    dt_dist,
    tempo
  )

writexl::write_xlsx(mais_demorados, "data-raw/valor/xlsx/mais_demorados.xlsx")

# as 10 com maiores valores envolvidos (e o principal credor)

mais_antigos <- obsFase3::da_processo_tidy |>
  dplyr::filter(dt_dist >= lubridate::dmy("01/01/2010")) |>
  dplyr::mutate(
    dt_fim = dplyr::coalesce(dt_fal_fim, dt_extincao)
  ) |>
  dplyr::filter(is.na(dt_fim)) |>
  dplyr::mutate(
    tempo = lubridate::today() - dt_dist
  ) |>
  dplyr::arrange(desc(tempo)) |>
  head(10) |>
  dplyr::select(
    id_processo,
    dt_dist,
    tempo
  )

writexl::write_xlsx(mais_antigos, "data-raw/valor/xlsx/mais_antigos.xlsx")

# analises AJ -------------------------------------------------------------
# 50 ajs mais nomeados
# os 10 AJs com mais casos em São Paulo?

da_aj <- obsFase3::da_leilao_tidy

ajs <- da_aj |>
  dplyr::mutate(
    leiloeiro = leiloeiro |>
      stringr::str_to_lower() |>
      stringr::str_remove_all("administrador judicial") |>
      stringr::str_remove_all("[:punct:]") |>
      stringr::str_squish() |>
      abjutils::rm_accent(),
    leiloeiro = dplyr::case_when(
      stringr::str_detect(leiloeiro, stringr::regex("juiz", TRUE)) ~ NA_character_, # juiz
      leiloeiro == "caio marcelo mendes de oliveira" ~ NA_character_, # juiz
      leiloeiro == "administrador judicial" ~ NA_character_,
      leiloeiro == "" ~ NA_character_,
      leiloeiro == "na" ~ NA_character_,
      leiloeiro == "aj" ~ NA_character_,
      stringr::str_detect(leiloeiro, "trustee") ~ "brasil trustee",
      leiloeiro == "danys pyerre de oliveira" ~ "denys pyerre de oliveira",
      stringr::str_detect(leiloeiro, "boyadjian") ~ "eduardo jordao boyadjian",
      stringr::str_detect(leiloeiro, "maraschi") ~ "euclides maraschi junior",
      stringr::str_detect(leiloeiro, "cerello") ~ "fernando jose cerello goncalves pereira",
      stringr::str_detect(leiloeiro, "alexandridis") ~ "georgios alexandridis",
      leiloeiro == "gustavo moretto guimaraes" ~ "gustavo moretto guimaraes de oliveira",
      leiloeiro == "gustavomoretto guimaraes de oliveira​" ~ "gustavo moretto guimaraes de oliveira",
      stringr::str_detect(leiloeiro, "hasta") ~ "hasta publica br",
      leiloeiro == "mega leiloes gestor judicial" ~ "mega leiloes",
      stringr::str_detect(leiloeiro, "moyse") ~ "renato schlobach moyses",
      stringr::str_detect(leiloeiro, "ronaldo|ronlado") & stringr::str_detect(leiloeiro, "faro") ~ "ronaldo sergio montenegro rodrigues faro",
      leiloeiro == "thais silva moreira de souza" ~ "thais silva moreira de sousa",
      leiloeiro == "thais silva moreira" ~ "thais silva moreira de sousa",
      TRUE ~ leiloeiro
    )
  ) |>
  dplyr::filter(!is.na(leiloeiro)) |>
  dplyr::mutate(
    leiloeiro = forcats::fct_lump(leiloeiro, n = 50, other_level = "outros"),
    leiloeiro = forcats::fct_infreq(leiloeiro),
    leiloeiro = forcats::fct_relevel(leiloeiro, "outros", after = Inf)
  ) |>
  dplyr::count(leiloeiro)

writexl::write_xlsx(ajs, "data-raw/valor/xlsx/ajs.xlsx")
