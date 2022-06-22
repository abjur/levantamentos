# import

da <- readxl::read_xlsx("/home/abj/Downloads/RESPOSTAS AO FORMS (parte 1).xlsx")

readr::write_rds(da, "data-raw/bruno-nassar-puc/data-raw/homicidios_bruto.rds")
