# Pesquisa livre: "recuperação judicial" E "cláusula" E "homologação" E "plano" OU "plano de recuperação judicial" OU "PRJ"
# Data do julgamento:
#   2019 = 482
#   2020 = 557
#   2021 = 529
#   2022 = 538
#   TOTAL = 2.106 acórdãos
# Órgão julgador:
#   •	1ª Câmara Reservada de Direito Empresarial
#   •	2ª Câmara Reservada de Direito Empresarial
#   •	Câmara Reservada de Direito Empresarial
# Data do julgamento: 01/01/2019 até 31/12/2022

tjsp::autenticar()

oj <- c("1ª Câmara Reservada de Direito Empresarial",
        "2ª Câmara Reservada de Direito Empresarial",
        "Câmara Reservada de Direito Empresarial")

livre <- '"recuperação judicial" E "cláusula" E "homologação" E "plano" OU "plano de recuperação judicial" OU "PRJ"'

tjsp::tjsp_baixar_cjsg(livre = livre,
                       inicio = "01/01/2019",
                       fim = "31/12/2022",
                       # orgao_julgador = "1ª Câmara Reservada de Direito Empresarial",
                       diretorio = 'data-raw/igor/html')

da <- fs::dir_ls("data-raw/igor/html") |>
  tjsp::tjsp_ler_cjsg()

da |>
  dplyr::filter(orgao_julgador %in% oj) |>
  writexl::write_xlsx("data-raw/igor/xlsx/cjsg.xlsx")
