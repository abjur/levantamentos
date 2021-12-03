library(magrittr)

tjsp_cposg_cpf_download <- function (cpf, dir = ".", login = Sys.getenv("ESAJ_LOGIN"), senha = Sys.getenv("ESAJ_SENHA"))
  {
    stopifnot(length(id) == 1)
    cpf <- stringr::str_remove_all(cpf, "[^0-9]")
    dir <- paste0(dir, "/", cpf, "/")
    fs::dir_create(dir)
    dir <- stringr::str_c(normalizePath(dir), "/")
    lex:::login_esaj(login, senha)
    get_query <- list(
      conversationId = "",
      paginaConsulta = 1,
      localPesquisa.cdLocal = -1,
      cbPesquisa = "DOCPARTE",
      tipoNuProcesso = "UNIFICADO",
      dePesquisa = cpf,
      uuidCaptcha = ""
    )
    file <- stringr::str_c(dir, stringr::str_remove_all(id,
                                                        "[^0-9]"), ".html")
    f_search <-
      httr::GET(
        "https://esaj.tjsp.jus.br/cposg/search.do?gateway=true",
        query = get_query,
        httr::config(ssl_verifypeer = FALSE),
        httr::write_disk(file, TRUE)
      )
    links <-
      f_search %>% httr::content() %>% xml2::xml_find_all("//*[@class='nuProcesso']/a") %>%
      xml2::xml_attr("href")
    if (length(links) != 0) {
      descs <-
        f_search %>% httr::content() %>% xml2::xml_find_all("//*[@id='listagemDeProcessos']") %>%
        xml2::xml_text() %>% stringr::str_remove_all("[\\n\\t]") %>%
        stringr::str_split(" {35}") %>% purrr::pluck(1) %>%
        magrittr::extract(2:length(.)) %>% stringr::str_squish()
      file.remove(file)
      subjects <-
        descs %>% rm_accent() %>% stringr::str_to_lower() %>%
        stringr::str_extract("[a-z ]+./") %>% stringr::str_extract("[a-z ]+") %>%
        stringr::str_trim() %>% stringr::str_replace_all(" ",
                                                         "_")
      dates <-
        descs %>% stringr::str_extract("[0-9]{2}/[0-9]{2}/[0-9]{4}") %>%
        lubridate::dmy()
      links <- stringr::str_c("https://esaj.tjsp.jus.br",
                              links)
      files <- stringr::str_c(dir, "/", id, "_", subjects,
                              "_", dates, ".html")
      purrr::map2(links, files, ~ {
        if (!file.exists(.y)) {
          httr::GET(.x,
                    httr::config(ssl_verifypeer = FALSE),
                    httr::write_disk(.y, TRUE))
        }
      })
      return(files)
    }
    return(file)
  }




