library(magrittr)

cjpg <- readr::read_rds("/mnt/dados/abj/levantamentos/jurimetria_usp/cjpg.rds")
cpopg <- readr::read_rds("/mnt/dados/abj/levantamentos/jurimetria_usp/cpopg.rds")

assuntos <- "Decorrente de Violência Doméstica
    Lesão Corporal
    Ameaça
    Violência Doméstica Contra a Mulher
    Homicídio Qualificado
    Injúria
    Contra a Mulher" %>%
  stringr::str_split("\n") %>%
  unlist() %>%
  stringr::str_squish()

cjpg_filtrado <- cjpg %>% 
  dplyr::filter(assunto %in% assuntos)

cpopg_filtrado <- cjpg_filtrado %>% 
  dplyr::mutate(id_processo = abjutils::build_id(n_processo)) %>% 
  dplyr::inner_join(cpopg, "id_processo") %>% 
  dplyr::transmute(
    n_processo, id_processo, codigo, assunto = assunto.x, classe = classe.x,
    comarca, data_de_disponibilizacao, foro, magistrado, vara, resumo_sentenca = resumo,
    status, area, distribuicao, juiz, local_fisico, outros_assuntos, outros_numeros,
    digital, movimentacoes,partes, historico, audiencias, delegacia, valor_da_acao,
    processo_principal
  )

cjpg_filtrado %>% 
  readr::write_rds("/mnt/dados/abj/levantamentos/jurimetria_usp/cjpg_filtrado.rds")

cpopg_filtrado %>% 
  readr::write_rds("/mnt/dados/abj/levantamentos/jurimetria_usp/cpopg_filtrado.rds")

