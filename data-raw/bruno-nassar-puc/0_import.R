# import

# da <- readr::read_csv("/home/abj/Downloads/RESPOSTAS AO FORMS (parte 1) - Processos.csv")
da <- readr::read_csv("/home/abj/Downloads/respostas_2.csv")
readr::write_rds(da, "data-raw/bruno-nassar-puc/data-raw/homicidios_bruto.rds")
