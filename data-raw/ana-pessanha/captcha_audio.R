id <- "00994003320055020033"

u <- "https://aplicacoes5.trt2.jus.br/consultasphp/public/index.php/primeirainstancia"
r0 <- httr::GET(u)
id_captcha <- r0 |>
  xml2::read_html() |>
  xml2::xml_find_first("//input[@id='captcha-id']") |>
  xml2::xml_attr("value")

body <- list(
  processo = id,
  "captcha[id]" = captcha_id,
  "captcha[input]" = "xxxxx"
)

r <- httr::POST(
  u, body = body, encode = "form",
  httr::write_disk("data-raw/ana-pessanha/teste.html")
)

# baixar captcha ----------------------------------------------------------

for (i in 1:30) {
  u <- "https://aplicacoes5.trt2.jus.br/consultasphp/public/index.php/primeirainstancia"
  r0 <- httr::GET(u)
  src <- r0 |>
    xml2::read_html() |>
    xml2::xml_find_all("//source") |>
    xml2::xml_attr("src")

  img <- r0 |>
    xml2::read_html() |>
    xml2::xml_find_all("//img[@alt='']") |>
    xml2::xml_attr("src")

  f <- fs::file_temp(
    pattern = "trt1",
    tmp_dir = "data-raw/ana-pessanha/audios/",
    ext = ".wav"
  )
  f_img <- fs::path_ext_set(f, ".png")

  r <- httr::GET(
    paste0("https://aplicacoes5.trt2.jus.br", src),
    httr::write_disk(f, TRUE)
  )
  r_img <- httr::GET(
    paste0("https://aplicacoes5.trt2.jus.br", img),
    httr::write_disk(f_img, TRUE)
  )
}

# resolver captcha pelo audio ---------------------------------------------

## download dos dados

classify_audio <- function(cap, path, rm_old = TRUE) {

  img <- fs::path_ext_set(cap, ".png")
  plot(captcha::read_captcha(img))
  ans <- readline("Answer: ")

  name <- tools::file_path_sans_ext(basename(cap))
  ext <- tools::file_ext(basename(cap))
  path <- ifelse(is.null(path), dirname(cap), normalizePath(path))
  new_file <- stringr::str_glue("{path}/{name}_{ans}.{ext}")
  file.copy(cap, new_file, overwrite = TRUE)
  if (rm_old) {
    file.remove(cap)
  }
  return(new_file)

}

path <- "data-raw/ana-pessanha/audios/"
arqs <- fs::dir_ls(path, glob = "*.wav") |>
  stringr::str_subset("_", negate = TRUE)

for (a in arqs) {
  classify_audio(a, path)
}


## checando se temos todas as labels

arqs_audio <- fs::dir_ls(path, glob = "*.wav")
arqs_audio |>
  stringr::str_extract("(?<=_)[a-z0-9]+") |>
  stringr::str_split("") |>
  unlist() |>
  unique() |>
  sort()

# ok! só não tem 1 e l.

## segmentacao

arqs_audio

audio <- arqs_audio[1]
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

segmentado <- arqs_audio |>
  tibble::enframe() |>
  dplyr::mutate(valores = purrr::map(value, segmentar_audio))

modelo <- segmentado |>
  dplyr::mutate(
    labs = value |>
      stringr::str_extract("(?<=_)[a-z0-9]+") |>
      stringr::str_split("")
  ) |>
  tidyr::unnest(c(valores, labs)) |>
  dplyr::select(-value) |>
  dplyr::distinct(labs, .keep_all = TRUE) |>
  dplyr::select(-name)


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


# novo teste --------------------------------------------------------------


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

download_trt_pagina_antiga("00994003320055020033", "data-raw/ana-pessanha/")

