# tidy

da_bruta <- readr::read_rds("data-raw/bruno-nassar-puc/data-raw/homicidios_bruto.rds") |>
  janitor::clean_names()

da_tidy <- da_bruta |>
  janitor::clean_names() |>
  dplyr::transmute(
    data_hora = carimbo_de_data_hora,
    id_processo = abjutils::clean_cnj(numero_do_processo_sem_ponto_e_traco),
    vara = numero_da_vara,
    digital = ifelse(natureza_do_processo == "Eletrônico", "Sim", "Não"),
    homicidio = dplyr::case_when(
      o_processo_versa_sobre_o_crime_de_homicidio == "Sim, sobre o art. 121" ~ "Sim",
      o_processo_versa_sobre_o_crime_de_homicidio == "Não, sobre outro crime doloso contra a vida" ~ "Não",
      o_processo_versa_sobre_o_crime_de_homicidio == "Processo sob sigilo" ~ "Sigilo"
    ),
    dt_fato = lubridate::mdy(data_do_fato),
    hr_fato = stringr::str_remove_all(horario_do_fato, stringr::regex("[a-z]+", TRUE)),
    hr_fato = lubridate::hms(hr_fato),
    parte_do_dia_fato = horario_do_fato_criterio_objetivo,
    dt_ip = lubridate::mdy(data_da_instauracao_do_ip),
    dt_dist = lubridate::mdy(data_da_distribuicao_do_processo),
    dt_denuncia = lubridate::mdy(data_da_denuncia),
    dt_recebimento_denuncia = lubridate::mdy(data_do_recebimento_da_denuncia),
    dt_pronuncia = lubridate::mdy(data_da_decisao_de_pronuncia_impronuncia_ou_desclassificacao_na_primeira_fase),
    dt_plenario = lubridate::mdy(data_da_sentenca_apos_plenario_absolvicao_sumaria_ou_extincao_da_punibilidade),
    suspensao = o_processo_chegou_a_ser_suspenso_com_base_no_art_366,
    dt_suspensao = lubridate::mdy(data_da_decisao_que_determinou_a_suspensao),
    dt_revogacao_suspensao = lubridate::mdy(data_da_decisao_que_revogou_a_suspensao_se_ha_decisao_expressa_nesse_sentido),
    denuncia_homicidio = homicidio_simples_denuncia,
    denuncia_qualificadoras = qualificadoras_denuncia,
    denuncia_org_crim = conexao_com_organizacao_criminosa_denuncia,
    concurso_menor = houve_concurso_com_menor_de_idade,
    flagrante = o_reu_foi_preso_em_flagrante,
    pris_prev = o_reu_foi_preso_preventivamente,
    dt_pris_prev = lubridate::mdy(data_da_decisao_que_decretou_a_preventiva),
    liberdade = se_preso_o_reu_foi_colocado_em_liberdade,
    dt_soltura = lubridate::mdy(data_da_decisao_que_determinou_a_soltura),
    n_reus_julgados = numero_de_reus_julgados,
    sexo_reus_julgados = sexo_dos_reus_julgados,
    cor_reus_julgados = cor_dos_reus_julgados_conforme_indicado_pelo_ip,
    reu_policial = algum_dos_reus_e_policial,
    n_vitimas = quantidade_de_vitimas,
    sexo_vitimas = sexo_das_vitimas,
    agente_ip = quem_desenvolveu_o_inquerito_policial,
    natureza_decisao = natureza_da_decisao,
    pandemia_juri = ainda_nao_foi_designado_o_plenario_do_juri_em_razao_da_pandemia,
    fundamento_36,
    causa_de_extincao_da_punibilidade,
    conversao_pronuncia = foi_inicialmente_proferida_impronuncia_absolvicao_sumaria_ou_desclassificacao_que_recorrida_foi_convertida_em_pronuncia,
    pronuncia_homicidio = homicidio_simples_pronuncia,
    pronuncia_qualificadoras = qualificadoras_pronuncia,
    pronuncia_rese = a_pronuncia_foi_alvo_de_rese,
    dt_acordao_rese = lubridate::mdy(data_do_acordao_do_tj_que_julgou_o_recurso),
    defesa_inicial = qual_a_natureza_da_defesa_que_acompanhou_o_plenario,
    defesa_mudanca = houve_mudanca_na_natureza_da_defesa_durante_o_curso_do_processo,
    assistente_de_acusacao = ha_assistente_da_acusacao,
    julgamento = resultado_do_julgamento,
    desclassificacao = resultado_da_sentenca_desclassificacao,
    oferecimento_sursis = coube_oferecimento_de_proposta_de_sursis,
    resultado_crime = resultado_do_crime,
    conexao_organizacao_criminos = de_acordo_com_a_sentenca_ha_conexao_do_crime_com_atividade_de_organizacao_criminosa,
    plenario_homicidio = homicidio_simples_plenario,
    plenario_qualificadoras = qualificadoras_plenario,
    plenario_causas_aumento = causas_de_aumento_plenario,
    reincidente = trata_se_de_reu_reincidente,
    homicidio_privilegido = homicidio_privilegiado,
    tempo_pena = qual_foi_o_tempo_de_pena_aplicado,
    regime_inicial = regime_inicial_de_cumprimento_de_pena,
    pode_liberdade = o_reu_pode_recorrer_em_liberdade,
    recurso = foi_interposto_recurso_contra_a_sentenca,
    quem_interpos_recurso = quem_interpos_o_recurso,
    recurso_ja_foi_julgado = se_interposto_o_recurso_de_apelacao_ja_foi_julgado,
    dt_acordao = lubridate::mdy(data_do_acordao),
    decisao_acordao = resultado_do_acordao,
    alteracao_pena = se_alterada_a_pena_para_outro_patamar_informar_qual,
    alteracao_regime = se_alterado_o_regime_de_cumprimento_inicial_informar_qual,
    sentenca_nulificada = houve_sentenca_nulificada_no_curso_dos_autos,
    dt_sentenca_nulificada = lubridate::mdy(data_da_sentenca_nulificada),
    fundamento_68,
    tribunal_sentenca_nulificada = qual_tribunal_nulificou_a_sentenca,
    dt_acordao_nulificada = lubridate::mdy(data_do_acordao_que_nulificou_a_sentenca)
  )

# fs::dir_create("data-raw/bruno-nassar-puc/data")

readr::write_rds(da_tidy, "data-raw/bruno-nassar-puc/data/homicidios_tidy.rds")
