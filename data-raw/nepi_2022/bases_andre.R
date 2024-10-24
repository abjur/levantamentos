# Base André

# rjsp --------------------------------------------------------------------

da_rjsp <- obsFase2::da_relatorio |>
  # dplyr::filter(epp_ou_me == "Empresa de Pequeno Porte (EPP)" | epp_ou_me == "Micro Empresa (ME)") |>
  dplyr::mutate(
    plano_desfecho = dplyr::case_when(
      resultado_final == "Aprovação do plano" ~ "O plano foi aprovado",
      resultado_final == "Ainda em negociação" ~ "Ainda não foi aprovado nem reprovado",
      cram_down == "Sim" ~ "Cram Down",
      is.na(resultado_final) ~ "(Vazio)",
      TRUE ~ "O plano foi reprovado (a empresa/grupo faliu)"
    ),
    desfecho_final = dplyr::case_when(
      is.na(desfecho_final) ~ "(Vazio)",
      desfecho_final == "Ainda em curso" ~ "Ainda não encerrou o cumprimento do plano",
      desfecho_final == "Encerramento sem falência" ~ "Cumprimento do plano encerrado",
      desfecho_final == "Falência" ~ "Faliu cumprindo o plano"
    ),
    plano_uno = dplyr::case_when(
      is.na(houve_decisao_plano_uno) ~ "(Vazio)",
    )
  ) |>
  dplyr::transmute(
    id_processo = n_processo,
    ano_dist = lubridate::year(data_dist),
    grupo_nome = NA_character_,
    comarca,
    capital,
    emenda,
    emenda_pedido_teve = "(Vazio)",
    plano_uno = houve_decisao_plano_uno,
    aj_listcred_valor = NA,
    requerente_listcred_valor = NA,
    faliu_antes_agc,
    plano_desfecho,
    resultado_final,
    data_falencia,
    tipo_societario = "(Vazio)",
    balanco_unificado = "(Vazio)",
    patrimonio_liquido = NA_real_,
    faturamento = faturamento_total,
    passivos = total_passivo,
    ativos = total_ativo,
    faixa_faturamento,
    faixa_passivo,
    faixa_ativo,
    aj_tipo = "(Vazio)",
    aj_nome = "(Vazio)",
    faliu_antes_agc,
    plano_desfecho,
    comite_credores = "(Vazio)",
    cram_down,
    legalidade_plano = "(Vazio)",
    legalidade_plano_momento = "(Vazio)",
    n_agc,
    pgto_prazo_classe1 = classe1_prazo,
    pgto_prazo_classe2 = classe2_prazo,
    pgto_prazo_classe3 = classe3_prazo,
    pgto_prazo_classe4 = NA_real_,
    desagio_valor_classe1 = classe1_desagio,
    desagio_valor_classe2 = classe2_desagio,
    desagio_valor_classe3 = classe3_desagio,
    desagio_valor_classe4 = NA_real_
  )

writexl::write_xlsx(da_rjsp, path = "data-raw/nepi_2022/xlsx/da_rjsp.xlsx")


# fsp ---------------------------------------------------------------------

falencia_empresas <- obsFase3::aux_rfb |>
  dplyr::select(cnpj, porte_empresa)

processos_f_epp_me <- obsFase3::aux_polo_ativo_cnpj |>
  dplyr::left_join(falencia_empresas) |>
  dplyr::filter(
    stringr::str_detect(nome, "EPP") |
      porte_empresa == "ME") |>
  dplyr::pull(id_processo)

da_fsp <- obsFase3::da_processo_tidy |>
  # dplyr::filter(id_processo %in% processos_f_epp_me) |>
  dplyr::select(
    id_processo,
    ano_dist,
    info_foro,
    info_conv,
    listcred_aj_teve,
    listcred_devedor_val,
    listcred_aj_val,
    aj_pfpj,
    info_ativo_val,
    dt_dist,
    info_origem,
    info_aj_crime
  )

writexl::write_xlsx(da_fsp, path = "data-raw/nepi_2022/xlsx/da_fsp.xlsx")


# rjrj --------------------------------------------------------------------

rjrj_empresas <- obsRJRJ::aux_rfb |>
  dplyr::select(cnpj, porte_empresa)

processos_rjrj_epp_me <- obsRJRJ::da_parte_tidy |>
  dplyr::left_join(rjrj_empresas) |>
  dplyr::filter(
    stringr::str_detect(nome, "EPP") |
      porte_empresa == "ME") |>
  dplyr::pull(id_processo)

da_rjrj <- obsRJRJ::da_processo_tidy |>
  # dplyr::filter(id_processo %in% processos_rjrj_epp_me) |>
  dplyr::transmute(
    id_processo,
    ano_dist,
    grupo_nome,
    comarca,
    capital,
    emenda,
    emenda_pedido_teve = "(Vazio)",
    plano_uno,
    aj_listcred_valor,
    requerente_listcred_valor = NA_real_,
    faliu_antes_agc,
    plano_desfecho,
    resultado_final,
    data_falencia,
    tipo_societario,
    balanco_unificado,
    patrimonio_liquido,
    faturamento,
    passivos,
    ativos,
    faixa_faturamento,
    faixa_passivo,
    faixa_ativo,
    aj_tipo,
    aj_nome,
    faliu_antes_agc,
    plano_desfecho,
    comite_credores = "(Vazio)",
    cram_down,
    legalidade_plano = "(Vazio)",
    legalidade_plano_momento = "(Vazio)",
    n_agc,
    pgto_prazo_classe1,
    pgto_prazo_classe2,
    pgto_prazo_classe3,
    pgto_prazo_classe4,
    desagio_valor_classe1,
    desagio_valor_classe2,
    desagio_valor_classe3,
    desagio_valor_classe4
  )

writexl::write_xlsx(da_rjrj, path = "data-raw/nepi_2022/xlsx/da_rjrj.xlsx")

# rjrs --------------------------------------------------------------------

processos_rjrs_epp_me <- c("00092282620188210028",
                   "50012943320208210004",
                   "50088512620208210019",
                   "00066933320188210026",
                   "00066933320188210026",
                   "50004430420208210130",
                   "50069493820208210019",
                   "50005293620208210045",
                   "50005293620208210045",
                   "50005293620208210045",
                   "50003513520208210127",
                   "50000022220128210124",
                   "50000281520158210027",
                   "50035729420208210072",
                   "50035729420208210072",
                   "50002452120208210112",
                   "50012264720208210016",
                   "50011080820208210037",
                   "00085435720198210004",
                   "00085435720198210004",
                   "00085435720198210004",
                   "00109505420208210019",
                   "50146392120208210019",
                   "50033933820198210027",
                   "50024308420198210009",
                   "50024308420198210009",
                   "00209995620178210021",
                   "00312063320168210027",
                   "00312063320168210027",
                   "00117482920128210008",
                   "50020109120198210005",
                   "00851595320178210001",
                   "50002021520198210114",
                   "50133423820178210001",
                   "50133423820178210001",
                   "50720411720208210001",
                   "50040870620208210016",
                   "50040131920208210026",
                   "50003853720208210118",
                   "50003853720208210118",
                   "50008792420208210045",
                   "00100308020208210019",
                   "00340481020168210019",
                   "00305840720188210019",
                   "00444083120168210010",
                   "00104987620178210010",
                   "50085273220168210001",
                   "50001450420198210144",
                   "50006968020198210015",
                   "00342074520198210019",
                   "00111735420178210005",
                   "00113276720138210052",
                   "00580983020168210010",
                   "00186409820198210010",
                   "50015281320208210037",
                   "00006850320198210124",
                   "50054051520208210019",
                   "50373870420208210001",
                   "50373870420208210001",
                   "00303812620108210019",
                   "00307423620118210010",
                   "00145130720168210016",
                   "00145130720168210016",
                   "51048688120208210001",
                   "02004861220188210001",
                   "00298015120108210033",
                   "00298015120108210033",
                   "00298015120108210033",
                   "00298015120108210033",
                   "00298015120108210033",
                   "00284719720158210015",
                   "00216638420178210022",
                   "00011661420158210024",
                   "00195237520158210013",
                   "00399829520118210027",
                   "00195246020158210013",
                   "00201750720118210022",
                   "00203192920128210027",
                   "00193184120188210013",
                   "00193184120188210013",
                   "00182416520168210013",
                   "50002833520178210113",
                   "50002833520178210113",
                   "50002833520178210113",
                   "00152352520178210010",
                   "00396366520158210008",
                   "00396366520158210008",
                   "00124663920168210023",
                   "00337894920158210019",
                   "00337894920158210019",
                   "00337894920158210019",
                   "00337894920158210019",
                   "00337894920158210019",
                   "00392326420188210022",
                   "00381316920168210019",
                   "00026807920158210063",
                   "01726429720128210001",
                   "00091648420168210028",
                   "00265202520178210039",
                   "00265202520178210039",
                   "00265202520178210039",
                   "00226483820178210027",
                   "00019757420148210012",
                   "00017984420158210055",
                   "00017571820168210128",
                   "00020782320168210041",
                   "00009995320188210133",
                   "00343817920108210048",
                   "00025384920178210049",
                   "00085113820138210109",
                   "00012343520138210120",
                   "00082817520138210018",
                   "00081905520148210048",
                   "00081711120108210009",
                   "00079908320128210156",
                   "00010042920168210074",
                   "00352832220158210027",
                   "00363265120158210008",
                   "00022721820158210151",
                   "00074886020188210019",
                   "00017371120198210067",
                   "00337075720168210027",
                   "00020836120118210060",
                   "00020836120118210060",
                   "00020836120118210060",
                   "00020836120118210060",
                   "14347910220108210001",
                   "00017301220138210008",
                   "00016005720178210145",
                   "00016005720178210145",
                   "00019418220178210113",
                   "00019418220178210113",
                   "00019418220178210113",
                   "00345555920168210022",
                   "00018875320168210113",
                   "00012729820168210166",
                   "00012643020168210164",
                   "00012541020178210080",
                   "00011860520128210058",
                   "00011668020198210086",
                   "00011523820178210128",
                   "00011437920178210127",
                   "00011139520168210089",
                   "00010320820128210051",
                   "00010846520168210050",
                   "00174853620138210086",
                   "00174853620138210086",
                   "00174853620138210086",
                   "00165562020178210035",
                   "00010462820178210047",
                   "00010462820178210047",
                   "50528403920208210001",
                   "50528403920208210001",
                   "00014091920168210057",
                   "00014091920168210057",
                   "00015545920178210148",
                   "00049812220198210010",
                   "00143793220188210073",
                   "00139808820158210014",
                   "00128156720158210026",
                   "00127870620148210036",
                   "50050498620168210010",
                   "00213166920188210037",
                   "00213166920188210037",
                   "50338621420208210001",
                   "50338621420208210001",
                   "00077770220198210037",
                   "00071501120188210044",
                   "00070213820198210022",
                   "00257701320138210023",
                   "00254027020148210022",
                   "00250789420168210027",
                   "00073086420188210077",
                   "00068969120148210007",
                   "00068330420168210005",
                   "00068204420178210013",
                   "00068204420178210013",
                   "00068204420178210013",
                   "12432117720108210001",
                   "00035227020168210048",
                   "04188915420148210001",
                   "03195081120118210001",
                   "50002980220178210049",
                   "00060595920168210009",
                   "00032641520188210008",
                   "00060341220178210009",
                   "02389080320118210001",
                   "00032395420168210078",
                   "00060221520198210013",
                   "00060221520198210013",
                   "00060221520198210013",
                   "00032178620158210124",
                   "00032079720168210159",
                   "00056844620138210047",
                   "00055156120188210022",
                   "01865522620148210001",
                   "00053224820198210010",
                   "00052722020168210077",
                   "00051854620178210007",
                   "00051477020188210016",
                   "00050690720188210039",
                   "01309066020168210001",
                   "01247497120168210001",
                   "00030228920158210031",
                   "00029083220158210135",
                   "00028898520178210028",
                   "00774116720178210001",
                   "00774116720178210001",
                   "00736831820178210001",
                   "00710131720118210001",
                   "00590014620108210052",
                   "00581912120108210004",
                   "00569013320178210001",
                   "00027075020198210054",
                   "00460123320168210008",
                   "00009019820158210060",
                   "00050151720178210026",
                   "00050039220158210019",
                   "00049852320198210022",
                   "00007097320178210068",
                   "00006606120168210102",
                   "00006606120168210102",
                   "00006606120168210102",
                   "00006606120168210102",
                   "00048994620168210058",
                   "00048912920178210157",
                   "00048833620188210054",
                   "00048772220158210058",
                   "00005922920158210076",
                   "00005922920158210076",
                   "00005922920158210076",
                   "00005922920158210076",
                   "00004922120118210139",
                   "00004591320168210153",
                   "00004553320178210058",
                   "00004212820128210060",
                   "00040535420188210027",
                   "00002860720128210160",
                   "00001691120138210021",
                   "00001691120138210021",
                   "00037720620168210048",
                   "00001084420198210053",
                   "00036496820138210159",
                   "00036106920158210040",
                   "00000998220148210045",
                   "00000910420178210077") |> unique()
# eu tirei esses números do servidor, mas eu não consegui subir no git certinho... Por isso eu copiei e colei pra cá

da_rjrs <- obsRJRS::da_processo_tidy |>
  # dplyr::filter(id_processo %in% processos_rjrs_epp_me) |>
  dplyr::transmute(
    id_processo,
    ano_dist,
    grupo_nome,
    comarca,
    capital,
    emenda,
    emenda_pedido_teve,
    plano_uno,
    aj_listcred_valor,
    requerente_listcred_valor,
    faliu_antes_agc,
    plano_desfecho,
    resultado_final,
    data_falencia,
    tipo_societario,
    balanco_unificado,
    patrimonio_liquido,
    faturamento,
    passivos,
    ativos,
    faixa_faturamento,
    faixa_passivo,
    faixa_ativo,
    aj_tipo,
    aj_nome,
    faliu_antes_agc,
    plano_desfecho,
    comite_credores,
    cram_down,
    legalidade_plano,
    legalidade_plano_momento,
    n_agc,
    pgto_prazo_classe1,
    pgto_prazo_classe2,
    pgto_prazo_classe3,
    pgto_prazo_classe4,
    desagio_valor_classe1,
    desagio_valor_classe2,
    desagio_valor_classe3,
    desagio_valor_classe4
  )

writexl::write_xlsx(da_rjrs, path = "data-raw/nepi_2022/xlsx/da_rjrs.xlsx")
