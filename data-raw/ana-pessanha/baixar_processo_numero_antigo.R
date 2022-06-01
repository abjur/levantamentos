segmentar_audio <- function(audio) {
  x <- tuneR::readWave(audio)@left |>
    abs()
  # plot(x, type = "l")
  # abline(h=10, col = 2)
  txt <- dplyr::if_else(x < 50, 0L, x) |>
    paste(collapse = ",")
  spl <- txt |>
    stringr::str_split("(,0){1000,}") |>
    dplyr::first()
  spl <- spl[!spl %in% c("", "0")]
  spl |>
    stringr::str_split(",") |>
    purrr::map_dbl(~sum(as.numeric(.x), na.rm = TRUE))
}

decrypt_audio <- function(audio) {

  modelo <- tibble::tribble(
    ~valores, ~labs,
    20391314,   "g",
    18723593,   "u",
    27624983,   "m",
    25318190,   "3",
    19940379,   "z",
    29460295,   "2",
    34659927,   "9",
    16126310,   "7",
    19560506,   "p",
    22197608,   "d",
    17919101,   "o",
    25963356,   "4",
    16113843,   "c",
    25348633,   "r",
    11995146,   "a",
    20922574,   "s",
    21235684,   "6",
    22058224,   "8",
    17853790,   "t",
    17563224,   "h",
    11556814,   "e",
    33987056,   "n",
    17134090,   "i",
    29553372,   "w",
    16086781,   "k",
    21276294,   "5",
    35745227,   "y",
    23199630,   "v",
    24551375,   "j",
    24912448,   "b",
    19289805,   "q",
    19254463,   "x",
    20767901,   "f"
  )

  segmentar_audio(audio) |>
    tibble::enframe("name", "valores") |>
    dplyr::inner_join(modelo, "valores") |>
    with(paste(labs, collapse = ""))
}

download_trt_pagina_antiga <- function(id, path) {
  f <- paste0(path, "/", id, ".html")

  if (!file.exists(f)) {
    # acessa pagina inicial
    u <- "https://aplicacoes5.trt2.jus.br/consultasphp/public/index.php/primeirainstancia"
    r0 <- httr::GET(u)

    # pega informacoes da requisicao
    captcha_id <- r0 |>
      xml2::read_html() |>
      xml2::xml_find_first("//input[@id='captcha-id']") |>
      xml2::xml_attr("value")

    # baixa o captcha
    src <- r0 |>
      xml2::read_html() |>
      xml2::xml_find_all("//source") |>
      xml2::xml_attr("src")
    f_captcha <- fs::file_temp(pattern = "trt1", ext = ".wav")
    r_captcha <- httr::GET(
      paste0("https://aplicacoes5.trt2.jus.br", src),
      httr::write_disk(f_captcha, TRUE)
    )

    captcha_value <- decrypt_audio(f_captcha)

    body <- list(
      processo = id,
      "captcha[id]" = captcha_id,
      "captcha[input]" = captcha_value
    )

    r <- httr::POST(
      u, body = body, encode = "form",
      httr::write_disk(f, TRUE)
    )
  }
  f
}


# testar ------------------------------------------------------------------

download_trt_pagina_antiga(
  "00994003320055020033",
  "data-raw/ana-pessanha/"
)

