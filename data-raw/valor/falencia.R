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

# 10 falências mais antigas que não foram concluídas (e o principal credor)

mais_antigos <- obsFase3::da_processo_tidy |>
  dplyr::filter(dt_dist >= lubridate::dmy("01/01/2010")) |>
  dplyr::mutate(
    dt_fim = dplyr::coalesce(dt_fal_fim, dt_extincao)
  ) |>
  dplyr::filter(is.na(dt_fim)) |>
  dplyr::mutate(
    tempo = data_hora - dt_dist
  ) |>
  dplyr::arrange(desc(tempo)) |>
  head(10) |>
  dplyr::select(
    id_processo,
    dt_dist,
    dt_classificado = data_hora,
    tempo
  )

writexl::write_xlsx(mais_antigos, "data-raw/valor/xlsx/mais_antigos.xlsx")

# analises AJ -------------------------------------------------------------
# 50 ajs mais nomeados
# os 10 AJs com mais casos em São Paulo?
aux_aj <- tibble::tibble(
  aj_nm = c("Absalão de Souza Lima", "Adnan Abdel Kader Salem", "Adriana Rodrigues de Lucena", "Afonso Henrique Alves Braga", "Alexandre Arantes Ferreira", "Alexandre Shikishima", "Alfio Carlos Affonso Zalli Neto", "Alfredo Luiz Kugelmas", "Amador Bueno", "Antonio Carlos Seoanes", "Antonio Hissao Sato Junior", "Bruna Oliveira Santos ", "Caetano Bernardes Neubauer", "Cezar Augusto Badolato Silva", "Cristiane Chabaribery da Costa Telles", "Cristiane Borguetti Moraes Lopes", "Daniela Tapxure Severino", "Danilo Cardoso da Silva", "Dannae Vieira Avila", "Denys Pyerre de Oliveira", "Dino Boldrini Neto", "Dora Plat", "Douglas José Fidalgo", "Eduardo Garcia de Lima", "Eduardo Jordão Boyadjian", "Eduardo dos Reis", "Euclides Maraschi Junior", "Ernesto Volpe Filho", "Érica Passarelli do Vale", "Fabio Zukerman", "Fabio Souza Pinto", "Fernando Ferreira Castellani", "Fernando Celso de Aquino Chad", "Fernando José Cerello Gonçalves Pereira", "Frederico Antonio Oliveira de Rezende", "Georgios José Ilias Bernabé Alexandridis", "Gilson Keniti Inumaru", "Glaice Tommasiello", "Guilherme Soderi Neiva Camargo", "Gustavo Cristiano Samuel dos Reis", "Gustavo Moretto Guimarães de Oliveira", "Hugo Martins Abud", "Irio Jose da Silva", "Jorge Toshihiko Uwada", "José Carlos Kalil Filho", "José Menah Lourenço", "José Moretzsohn de Castro", "José Ricardo Bueno Zappa", "José Roberto Ossuna", "Josué Mastrodi Neto", "Luciana Ferreira da Costa Telles", "Manuel Antonio Angulo Lopez", "Marcelo Hajaj Merlino", "Marcelo Valland", "Maria Fabiana Seoane Dominguez Sant’Ana", "Marilaine Borges de Paula", "Marinaldo Muzy Villela", "Mauricio Dellova Campos", "Maurício Galvão de Andrade", "Nelson Alberto Carmona", "Nelson Garey", "Orestes Nestor de Souza Laspro", "Orival Salgado", "Orivaldo Figueiredo Lopes", "Orlando Geraldo Pampado", "Paulo Luvisari Furtado", "Paulo Roberto Bastos Pedro", "Pedro Sales", "Renata Franklin Simões", "Renato Schlobach Moysés", "Ricardo Augusto Requena", "Ricardo Siqueira Salles dos Santos", "Rodrigo Damásio de Oliveira", "Rodrigo Vieira Clara", "Ronaldo Milan", "Ronaldo Sérgio Montenegro Rodrigues Faro", "Rubens Machioni Silva", "Sadi Montenegro Duarte Neto", "Sérgio Villa Nova de Freitas", "Thais Kodama da Silva", "Thais Silva Moreira de Sousa", "Uilian Aparecido da Silva", "Yasmine Altimare da Silva", "Wendell Marcel Calixto Félix", "ACFB ADMINSITRAÇÃO JUDICIAL LTDA", "ADJUD ADMINISTRADORES JUDICIAIS LTDA", "ADNAN ABDEL KADER SALEM SOCIEDADE DE ADVOGADOS", "AJ RUIZ CONSULTORIA EMPRESARIAL LTDA", "AJ1 ADMINISTRAÇÃO JUDICIAL LTDA-ME", "ALA CONSULTORIA E ADMINISTRAÇÃO EIRELI - EPP", "ALTA ADMINISTRAÇÃO JUDICIAL LTDA.", "ALVAREZ & MARSAL ADMINISTRAÇÃO JUDICIAL LTDA.", "APPROBATO MACHADO ADVOGADOS", "BRASIL TRUSTEE ASSESSORIA E CONSULTORIA", "CABEZÓN ADMINISTRAÇÃO JUDICIAL EIRELI", "CAMIÑA, DEL PONTE E OSHIRO SOCIEDADE DE ADVOGADOS", "CAPITAL ADMINISTRADORA JUDICIAL LTDA", "CASA REIS LEILÕES", "CONCÓRDIA SERVIÇOS ADMINISTRATIVOS EMPRESARIAIS LTDA", "COMPASSO ADMINISTRAÇÃO JUDICIAL LTDA", "D1LANCE LEILÕES", "DAMÁSIO CONSULTORIA", "DELOITTE TOUCHE CONSULTORES LTDA", "ESCRITÓRIO DE ADVOCACIA ARNOLDO WALD", "EXM PARTNERS ASSESSORIA EMPRESARIAL LTDA", "F. REZENDE CONSULTORIA EM GESTÃO EMPRESARIAL LTDA", "Gold Leilões - Gold Intermediação de Ativos LTDA", "HastaPublicaBR Promotora de Eventos Ltda", "KPMG CORPORATE FINANCE LTDA", "LANCE ALIENACOES ELETRONICAS LTDA", "LASPRO CONSULTORES LTDA.", "LAURIA SOCIEDADE DE ADVOGADOS", "LINDOSO E ARAÚJO CONSULTORIA EMPRESARIAL LTDA.", "LUCON ADVOGADOS", "ONBEHALF AUDITORES E CONSULTORES LTDA", "PINHEIRO FRANCO E SALES ADVOGADOS ASSOCIADOS", "PRICEWATERHOUSECOOPERS ASSESSORIA EMPRESARIAL LTDA", "REAL BRASIL CONSULTORIA", "R4C ADMINISTRAÇÃO JUDICIAL LTDA", "RODRIGO DAMÁSIO DE OLIVEIRA - DAMÁSIO CONSULTORIA E RECUPERAÇÃO JUDICIAL", "RV3 CONSULTORES LTDA", "SAUER ARRUDA PINTO ADVOGADOS ASSOCIADOS SC LTDA", "TRUSTEE ADMINISTRADORES JUDICIAIS LTDA", "V FACCIO ADMINISTRAÇÕES", "VALDOR FACCIO", "VIVANTE GESTÃO E ADMINISTRAÇÃO JUDICIAL LTDA."),
  aj_id = c("6370", "46142", "1062", "3801", "19711", "2757", "21977", "1650", "13727", "7562", "858", "19864", "26093", "5393", "2808", "1542", "1922", "7794", "28025", "1748", "160", "5608", "5535", "3102", "924", "5450", "5665", "2799", "4695", "5508", "3012", "28103", "2434", "5406", "19583", "7633", "5699", "2116", "34408", "5596", "7979", "25812", "23467", "1649", "2609", "945", "8160", "2602", "2581", "1880", "13276", "3890", "13247", "6480", "1305", "5894", "3751", "10703", "925", "10456", "2047", "887", "4468", "13724", "2044", "1294", "12929", "3421", "669", "466", "4397", "3328", "575", "38519", "29112", "6331", "4764", "4250", "5604", "13244", "34064", "26542", "47230", "23601", "286", "485", "2376", "26544", "38327", "1572", "1055", "4183", "5118", "507", "635", "27869", "2520", "5448", "7576", "1821", "5366", "585", "3795", "4981", "1299", "1881", "620", "1057", "1836", "5937", "930", "13482", "4848", "19525", "3530", "3509", "13240", "5712", "1025", "575", "38067", "3994", "2597", "1727", "1752", "10445")
)

ajs <- obsFase3::da_processo_tidy |>
  dplyr::mutate(aj_id = dplyr::coalesce(aj_id, aj_id2)) |>
  dplyr::select(id_processo, aj_id) |>
  dplyr::filter(!is.na(aj_id)) |>
  dplyr::count(aj_id) |>
  dplyr::left_join(aux_aj) |>
  dplyr::select(aj_nm, n) |>
  dplyr::arrange(desc(n)) |>
  head(50)

writexl::write_xlsx(ajs, "data-raw/valor/xlsx/ajs.xlsx")
