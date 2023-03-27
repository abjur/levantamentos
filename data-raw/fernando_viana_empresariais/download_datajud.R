get_empresariais <- function(parquet) {
  parquet |> 
    arrow::read_parquet() |> 
    dplyr::filter(
      stringr::str_detect(nome_orgao, stringr::regex(paste0("empr.+"), TRUE))
    )
}

get_parq_empresarial <- function(uf) {
  parquet <- fs::dir_ls("/mnt/dados/abj/datajud/parquet", regexp = paste0("TJ", uf))
  parquet |> 
    furrr::future_map(get_empresariais, .progress = TRUE) |> 
    dplyr::bind_rows()
}

future::plan("multisession")

sp <- get_parq_empresarial("SP")
sp <- dplyr::filter(sp, stringr::str_detect(nome_orgao, "CAPITAL"))
bh <- get_parq_empresarial("MG")
bh <- dplyr::filter(bh, stringr::str_detect(nome_orgao, "BELO HORIZONTE"))
rj <- get_parq_empresarial("RJ")

readr::write_rds(sp, "/mnt/dados/abj/levantamentos/data-raw/fernando_viana_empresariais/sp.rds")
readr::write_rds(bh, "/mnt/dados/abj/levantamentos/data-raw/fernando_viana_empresariais/bh.rds")
readr::write_rds(rj, "/mnt/dados/abj/levantamentos/data-raw/fernando_viana_empresariais/rj.rds")

