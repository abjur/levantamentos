acordaos <- lex::stj_cjsg_download(
  busca = "papiloscopica",
  dir = "data-raw/pericia-papiloscopica/stj_acordaos",
  tipo = "acordao"
)


fs::dir_create(dir)
tipo <- switch(tipo[1], acordao = "ACOR", monocratica = "DTXT")
busca <- busca %>% stringr::str_split("") %>% purrr::pluck(1) %>%
  purrr::map_if(~!stringr::str_detect(.x, "[0-9A-Za-z]"),
                lex:::to_win1252) %>% stringr::str_c(collapse = "")
file0 <- paste0(dir, "/pag_0001.html")
lex:::stj_cjsg_download_(1, file0, busca, tipo)


params <- list(acao = "pesquisar", novaConsulta = "true",
               i = 1, b = tipo, data = "", livre = I(busca), ref = "",
               indx = "", filtroPorOrgao = "", filtroPorMinistro = "",
               filtroPorNota = "", opAjuda = "NAO", operador = "mesmo",
               thesaurus = "JURIDICO", p = "true", processo = "", relator = "",
               data_inicial = "", data_final = "", tipo_data = "DTPB",
               orgao = "", ementa = "", siglajud = "", numero_leg = "",
               tipo1 = "ART", numero_art1 = "", tipo2 = "PAR", numero_art2 = "",
               tipo3 = "INC", numero_art3 = "", nota = "")

httr::handle_reset("https://scon.stj.jus.br/SCON/pesquisar.jsp")
httr::POST(
  "https://scon.stj.jus.br/SCON/pesquisar.jsp",
  body = params, encode = "form", httr::write_disk(file0, TRUE),
  lex:::esaj_ua()
  # httr::add_headers(c(`User-Agent` = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.15; rv:105.0) Gecko/20100101 Firefox/105.0"))
)






n_pags <- file0 %>% xml2::read_html() %>% xml2::xml_find_first("//div[@id='infopesquisa']/div/div[1]") %>%
  xml2::xml_text() %>% stringr::str_remove_all("[^0-9]") %>%
  base::as.numeric() %>% magrittr::divide_by(10) %>% base::ceiling()
starts <- 1:(n_pags - 1) * 10 + 1
files <- paste0(dir, "/pag_", stringr::str_pad(2:n_pags,
                                               4, pad = "0"), ".html")
purrr::walk2(starts, files, stj_cjsg_download_, busca, tipo)
return(c(file0, files))
