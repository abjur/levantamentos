# Bases Henrique

da_processos_henrique <- obsFase3::da_processo_tidy |>
  dplyr::filter(if_any(
   dplyr::contains("pgto"),
   ~stringr::str_detect(., "Pagou")
  )) |>
  dplyr::select(
    id_processo,
    ano_dist,
    info_classe,
    info_assunto,
    info_digital,
    info_foro,
    listcred_devedor_teve,
    listcred_devedor_val,
    dt_listcred_devedor,
    listcred_devedor_aj,
    listcred_aj_teve,
    listcred_aj_val,
    dt_listcred_aj,
    info_leilao,
    info_leilao_justif,
    info_ativo_val,
    info_fal_acabou,
    dt_fal_fim,
    dt_dist,
    info_origem,
    dt_extincao,
    dt_arrecadacao,
    dplyr::contains("pgto")
  )

processos_ja_pagou <- da_processos_mateus |>
  dplyr::pull(id_processo)

da_avaliacao_henrique <- obsFase3::da_avaliacao_tidy |>
  dplyr::filter(id_processo %in% processos_ja_pagou)
# não retornou nada

da_leilao_henrique <- obsFase3::da_leilao_tidy |>
  dplyr::filter(id_processo %in% processos_ja_pagou)
# não retornou nada

writexl::write_xlsx(da_processos_henrique, "data-raw/nepi_2022/xlsx/da_henrique.xlsx")
