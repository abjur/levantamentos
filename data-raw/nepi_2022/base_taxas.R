# taxa de recuperação

library(magrittr)

da <- obsFase3::da_processo_tidy

rx_lote <- "lote"

aux_listcred <- da %>%
  dplyr::transmute(
    id_processo,
    passivo = dplyr::coalesce(listcred_aj_val, listcred_devedor_val)
  )

aux_taxa_arrneg <- da %>%
  dplyr::filter(info_leilao_justif_min == "Arrecadação de bens negativa") %>%
  dplyr::transmute(id_processo, vendido = 0, taxa_ativo = 0)

aux_taxa_caucao <- da %>%
  dplyr::filter(info_fal_extin_caucao == "Sim") %>%
  dplyr::transmute(id_processo, vendido = 0, taxa_ativo = 0)

aux_taxa_leilao <- obsFase3::da_leilao_tidy %>%
  dplyr::semi_join(da, "id_processo") %>%
  dplyr::mutate(descricao = stringr::str_remove_all(descricao, rx_lote)) %>%
  dplyr::filter(tipo != "movel", !is.na(data_edital)) %>%
  dplyr::filter(
    # valor_avaliacao_inicial < 1e8,
    valor_avaliacao_inicial > 0,
    valor_total_arrematado > 0,
    # valor_total_arrematado < 1e8,
    valor_total_arrematado < 5 * valor_avaliacao_inicial
  ) %>%
  dplyr::arrange(dplyr::desc(data_edital)) %>%
  dplyr::group_by(id_processo, descricao) %>%
  dplyr::slice(1) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(id_processo) %>%
  dplyr::summarise(
    vendido = sum(valor_total_arrematado, na.rm = TRUE),
    ativo = sum(valor_avaliacao_inicial, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::mutate(taxa_ativo = vendido / ativo)

aux_taxa <- aux_taxa_arrneg %>%
  dplyr::bind_rows(aux_taxa_leilao) %>%
  dplyr::bind_rows(aux_taxa_caucao) %>%
  dplyr::left_join(aux_listcred, "id_processo") %>%
  dplyr::mutate(taxa_passivo = vendido / passivo) %>%
  dplyr::arrange(dplyr::desc(taxa_passivo)) %>%
  dplyr::distinct(id_processo, .keep_all = TRUE)

writexl::write_xlsx(aux_taxa, "data-raw/nepi_2022/xlsx/da_taxas.xlsx")

taxa_recup <- mean(aux_taxa$taxa_ativo)
taxa_recup_passivo <- mean(aux_taxa$taxa_passivo, na.rm = TRUE)

aux_taxa %>%
  dplyr::filter(taxa_ativo < 2) %>%
  ggplot2::ggplot() +
  ggplot2::aes(taxa_ativo) +
  ggplot2::geom_histogram(bins = 20, fill = blue_abj) +
  ggplot2::scale_x_continuous(labels = scales::percent, breaks = 0:10/5) +
  ggplot2::theme_minimal(12) +
  ggplot2::geom_vline(xintercept = taxa_recup, colour = "red", linetype = 2) +
  ggplot2::labs(
    x = "Taxa de recuperação",
    y = "Quantidade de processos"
  )
