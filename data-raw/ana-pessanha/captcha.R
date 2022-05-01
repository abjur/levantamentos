# preparacao --------------------------------------------------------------

library(magrittr)

## instalar os pacote captcha e captchaDownload
# remotes::install_github("decryptr/captcha@dataset", force = TRUE)
# remotes::install_github("jtrecenti/captchaDownload", force = TRUE)

# funcoes -----------------------------------------------------------------

#' verifica se o captcha foi resolvido corretamente.
#'
#' Função utilizada internamente.
#'
#' @param r_consulta request de consulta
#' @param token token de pesquisa
#' @param label chute do captcha
#'
#' @export
captcha_trt_test <- function(r_consulta, token, label) {
  acertou <- r_consulta %>%
    httr::content() %>%
    purrr::pluck("numero") %>%
    is.null() %>%
    magrittr::not()
  acertou
}

#' Baixa processo TRT2
#'
#' @param id_processo numero do processo
#' @param path caminho para salvar resultado
#' @param model modelo para resolver captcha
#'
#' @export
baixar_processo_trt2 <- function(id_processo, path, model = NULL) {

  id_processo <- stringr::str_remove_all(id_processo, "[^0-9]")
  path_json <- file.path(path, paste0(id_processo, ".json"))

  # obter o id do processo
  u_id <- paste0("https://pje.trt2.jus.br/pje-consulta-api/api/processos/dadosbasicos/", id_processo)
  r_id <- httr::GET(u_id, httr::add_headers("x-grau-instancia" = 1))
  id <- httr::content(r_id)[[1]]$id
  if (is.null(id)) return ("ID deu nulo, processo inválido.")

  # obter o captcha
  fs::dir_create(path)
  u_captcha <- paste0("https://pje.trt2.jus.br/pje-consulta-api/api/captcha?idProcesso=", id)
  f_captcha <- fs::file_temp(tmp_dir = path, ext = ".jpeg", pattern = "trt")

  if (!file.exists(f_captcha)) {
    captcha <- httr::GET(u_captcha)
    captcha %>%
      httr::content() %>%
      purrr::pluck("imagem") %>%
      base64enc::base64decode() %>%
      writeBin(f_captcha)
    token <- captcha %>%
      httr::content() %>%
      purrr::pluck("tokenDesafio")
  }

  # resolver o captcha
  if (is.null(model)) {
    label <- captchaDownload:::captcha_label(f_captcha)
  } else {
    label <- captcha::decrypt(f_captcha, model)
  }

  # acessar processo
  u_consulta <- paste0("https://pje.trt2.jus.br/pje-consulta-api/api/processos/", id)
  r_consulta <- httr::GET(
    u_consulta,
    query = list(tokenDesafio = token, resposta = label),
    httr::add_headers(`X-Grau-Instancia` = 1)
  )

  # salvar processo
  acertou <- captcha_trt_test(r_consulta, token, label)
  if (!acertou) {
    stop("Captcha estava errado. Tentar novamente.")
  } else {
    fs::file_delete(f_captcha)
    writeBin(r_consulta$content, path_json)
  }
  path_json

}



# aplicando funcoes -------------------------------------------------------

id_processo <- "1001273-64.2019.5.02.0611"
model <- luz::luz_load("~/Documents/ABJ/github/levantamentos/data-raw/ana-pessanha/trt_99.pt") # modificar este path
baixar_processo_trt2(id_processo, path = "~/Downloads", model) # escolher onde salvar os .json
