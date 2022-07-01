library(tidyverse)
library(tidyr)
library(dplyr)
library(summarytools)
library(gtsummary)

# Importar as bases separadas

apreensao <- readxl::read_xlsx("./data/bases_apreensoes.xlsx")
subtracao <- readxl::read_xlsx("./data/bases_subtração.xlsx", sheet = 3)
armas_match <- readxl::read_xlsx("./data/base_match.xlsx", sheet = 2)

apreendidas <- apreensao %>% 
  janitor::clean_names() %>% 
  mutate(
    id_delegacia = as.character(id_delegacia)
  )

subtraidas <- subtracao %>% 
  janitor::clean_names()

ambas <- armas_match %>% 
  janitor::clean_names() 


apreendidas$data_ocorrencia_bo <-as.numeric(apreendidas$data_ocorrencia_bo)

subtraidas$data_ocorrencia_bo <-as.numeric(subtraidas$data_ocorrencia_bo)


stringr::str_glue(
  "Tem na base de apreendidas e nao tem na base de subtraidas\n",
  "`{paste(setdiff(names(apreendidas), names(subtraidas)), collapse = ', ')}`",
  "\n\n",
  "Tem na base de subtraidas e nao tem na base de apreendidas\n",
  "`{paste(setdiff(names(subtraidas), names(apreendidas)), collapse = ', ')}`"
)

nomes_tem_nas_duas <- intersect(names(apreendidas), names(subtraidas))

da_raw <- list(apreendidas = apreendidas, subtraidas = subtraidas) %>% 
  dplyr::bind_rows(.id = "origem") %>% 
  dplyr::select(origem, dplyr::all_of(nomes_tem_nas_duas)) %>% 
  dplyr::mutate(y = as.integer(as.factor(origem)) - 1L) %>% 
  dplyr::relocate(y)

saveRDS(da_raw, "da_raw.rds")

# Tidy -------------------------------------------------------------------------


explicativas <- c(
  # "flag_flagrante",
  # "flag_status",
  # "flag_autoria_bo",
  # "flag_deficiencia",
  # "flag_vitima_fatal",
  # "sexo_pessoa",
  # "descr_exame",
  # "descricao_apresentacao",
  # "descr_periodo",
  # "cor_cutis",
  # "descr_grau_instrucao",
  # "descr_tipo_pessoa",
  # "nome_departamento_circ",
  # "descr_arma_fogo",
  # "nome_departamento",
  # "descr_conduta",
  "descr_arma_fogo",
  "marca_arma2",
  "calibre_arma2"
  # "hora_ocorrencia_bo"
)

da <- da_raw %>% 
  dplyr::mutate(
    dplyr::across(
      c(flag_autoria_bo), 
      ~{dplyr::case_when(is.na(.x) ~ "X", TRUE ~ .x)}
    ),
    dplyr::across(
      c(flag_vitima_fatal), 
      ~{dplyr::case_when(is.na(.x) ~ "NULL", TRUE ~ .x)}
    ),
    dplyr::across(
      c(sexo_pessoa), 
      ~{dplyr::case_when(is.na(.x) ~ "I", TRUE ~ .x)}
    ),
    dplyr::across(
      dplyr::all_of(explicativas),
      toupper
    ),
    nome_departamento_circ = dplyr::case_when(
      nome_departamento_circ == "OUTRAS DELEGACIAS" ~ "OUTROS",
      TRUE ~ nome_departamento_circ
    ),
    dplyr::across(
      dplyr::all_of(explicativas),
      forcats::fct_lump_min, min = 30, other_level = "OUTROS"
    )
  ) %>% 
  dplyr::select(y, dplyr::all_of(explicativas)) %>% 
  tidyr::drop_na(dplyr::everything())

#saveRDS(da, "da.rds")

da <- read_rds("da.rds")

base <- da %>% 
  group_by(y, descr_arma_fogo,marca_arma2,calibre_arma2 ) %>% 
  count()

base %>% 
  filter(y=="1") %>% 
  qdap::dist_tab(descr_arma_fogo) %>%
  knitr::kable()

library("gmodels")
CrossTable(base$y, base$descr_arma_fogo) 

base %>% 
  filter(descr_arma_fogo == "PISTOLA" & marca_arma2 == "TAURUS (FORJAS TAURUS S.A.)" 
         & calibre_arma2 == ".380") %>% 
  ggplot(aes(x = y, y = n, colour = y)) +
  geom_jitter(alpha = 0.1) +
  geom_boxplot(fill = NA) +
  theme_minimal()
  

# Visualize --------------------------------------------------------------------

da_raw %>% 
  dplyr::summarise(dplyr::across(.fns = dplyr::n_distinct)) %>% 
  tidyr::pivot_longer(dplyr::everything()) %>% 
  dplyr::arrange(value) %>% 
  print(n = 100)

desc <- purrr::map(explicativas, ~{
  da %>% 
    dplyr::count(y, .data[[.x]]) %>% 
    as.data.frame()
})


# Teste de cálculo de frequencies com o recorte de Pistolas --------------------

da_revolver_count <- da %>% 
  filter(descr_arma_fogo == "REVOLVER") %>% 
  group_by(y) %>% 
  count()

totalrevolver_0 <- as.double(da_revolver_count$n[1])
totalrevolver_1 <- as.double(da_revolver_count$n[2])

da_revolver <- da %>% 
  filter(descr_arma_fogo == "REVOLVER") %>% 
  group_by(y,descr_arma_fogo, marca_arma2, calibre_arma2) %>% 
  count()

freq_marca <- da %>% 
  filter(descr_arma_fogo == "REVOLVER") %>% 
  group_by(y) %>% 
  freq(marca_arma2, order = "freq", round.digits = 2, cumul = FALSE, headings = FALSE)

freq_marca_0 <- as.data.frame(freq_marca[["y = 0"]]) %>% 
  mutate(across(is.numeric, ~ round(., 2))) %>% 
  select(-2:3)

freq_marca_1 <- as.data.frame(freq_marca[["y = 1"]]) %>% 
  mutate(across(is.numeric, ~ round(., 2))) %>% 
  select(-2:3)

freq_calibre <- da %>% 
  filter(descr_arma_fogo == "REVOLVER") %>% 
  group_by(y) %>% 
  freq(calibre_arma2, order = "freq", round.digits = 2, cumul = FALSE, headings = FALSE)

freq_calibre_0 <- as.data.frame(freq_calibre[["y = 0"]]) %>% 
  mutate(across(is.numeric, ~ round(., 2))) %>% 
  select(-2:3)

freq_calibre_1 <- as.data.frame(freq_calibre[["y = 1"]]) %>% 
  mutate(across(is.numeric, ~ round(., 2))) %>% 
  select(-2:3)
  
  
revolver_freq_0 <- da_revolver %>% 
  filter(y==0) %>% 
  mutate(porc = n / totalrevolver_0 * 100) %>% 
  mutate("Porcentagem %" = round(porc, 2)) %>% 
  select(-porc) %>% 
  arrange(desc(n))

revolver_freq_1 <- da_revolver %>% 
  filter(y==1) %>% 
  mutate(porc = n / totalrevolver_1 * 100) %>% 
  mutate("Porcentagem %" = round(porc, 2)) %>% 
  select(-porc) %>% 
  arrange(desc(n))

regressao <- lm(y ~ marca_arma2 + calibre_arma2, data = da_revolver)

tbl_regression(regressao)

regressao_com_predicoes <- broom::augment(regressao)

teste <- regressao_com_predicoes %>% 
  select(Marca = marca_arma2, Calibre = calibre_arma2, predicao = .fitted) %>%
  arrange(desc(predicao))

(teste)
