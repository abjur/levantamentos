#' Author: Julio Trecenti
#' Subject: Verificando se análise de predizer origem a partir de características da base é possível

library(tidyverse)
library(magrittr)

# Import -----------------------------------------------------------------------

apreendidas <- readxl::read_xlsx("./data/bases_apreensoes.xlsx") %>% 
  janitor::clean_names() %>% 
  dplyr::mutate(
    id_delegacia = as.character(id_delegacia)
  )

subtraidas <- readxl::read_xlsx("./data/bases_subtração.xlsx", sheet = 3) %>% 
  janitor::clean_names() 

apreendidas$data_ocorrencia_bo <- as.numeric(apreendidas$data_ocorrencia_bo)
subtraidas$data_ocorrencia_bo <- as.numeric(subtraidas$data_ocorrencia_bo)


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

# saveRDS(da_raw, "da_raw.rds")

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
  tidyr::drop_na(dplyr::everything()) %>% 
  dplyr::mutate(
    tipo_arma = dplyr::case_when(
      descr_arma_fogo %in% c("REVOLVER") ~ "Revolver",
      descr_arma_fogo %in% c("PISTOLA", "PISTOLETE") ~ "Pistola",
      descr_arma_fogo %in% c("ESPINGARDA", "CARTUCHEIRA", "PISTOLÃO", "GARRUCHÃO") ~ "Espingarda",
      descr_arma_fogo %in% c("GARRUCHA") ~ "Garrucha",
      descr_arma_fogo %in% c("CARABINA", "FUZIL", "RIFLE", "MOSQUETÃO") ~ "Rifle",
      descr_arma_fogo %in% c("SUBMETRALHADORA", "METRALHADORA") ~ "Metralhadora",
      TRUE ~ "Outros"
    )
  )

# base utilizada no codigo modelagem_avancada.R
saveRDS(da, "da.rds")


# Visualize --------------------------------------------------------------------

da_raw %>% 
  dplyr::summarise(dplyr::across(.fns = dplyr::n_distinct)) %>% 
  tidyr::pivot_longer(dplyr::everything()) %>% 
  dplyr::arrange(value) %>% 
  print(n = 100)

purrr::map(explicativas, ~{
  da %>% 
    dplyr::count(y, .data[[.x]]) %>% 
    as.data.frame()
})


# Filtra para teste uma categorias de armas ------------------------------------

da <- da_all %>% 
  filter(descr_arma_fogo == "REVOLVER")

da %>% 
  group_by(y,descr_arma_fogo, marca_arma2, calibre_arma2) %>% 
  count()

# Model ------------------------------------------------------------------------

set.seed(1)
id_train <- sample(seq_len(nrow(da)), 80000L)
da_train <- da[id_train,]
da_valid <- da[-id_train,]

X <- model.matrix(~ marca_arma2 + descr_arma_fogo + calibre_arma2 - 1, data = da)
y <- da$y
X_train <- X[id_train,]
X_valid <- X[-id_train,]
y_train <- y[id_train]
y_valid <- y[-id_train]

model <- glm(y ~ ., data = da_train, family = "binomial")
model_lasso <- glmnet::cv.glmnet(X_train, y_train)
modelo_arvore <- rpart::rpart(
  y ~ ., data = da_train,
  control = rpart::rpart.control(minsplit = 10, cp = .001)
)

split.fun <- function(x, labs, digits, varlen, faclen) {
  # replace commas with spaces (needed for strwrap)
  labs <- gsub(",", " ", labs)
  for(i in 1:length(labs)) {
    # split labs[i] into multiple lines
    labs[i] <- paste(strwrap(labs[i], width=25), collapse="\n")
  }
  labs
}

rpart.plot::prp(modelo_arvore, split.fun=split.fun)

da_pred_valid <- da_valid %>% 
  dplyr::mutate(
    prob = as.numeric(predict(model, da_valid, type = "response")),
    pred = as.numeric(prob > .5),
    prob_lasso = predict(model_lasso, newx = X_valid, s = "lambda.1se")[,1],
    pred_lasso = as.numeric(prob_lasso > .5),
    prob_arvore = predict(modelo_arvore, da_valid),
    pred_arvore = as.numeric(prob_arvore > .5)
  ) %>% 
  dplyr::mutate(
    acertou = y == pred,
    acertou_lasso = y == pred_lasso,
    acertou_arvore = y == pred_arvore
  )

da_pred_valid %>% 
  tidyr::pivot_longer(dplyr::starts_with("acertou")) %>% 
  dplyr::count(name, value) %>% 
  dplyr::group_by(name) %>% 
  dplyr::mutate(prop = n/sum(n)) %>% 
  dplyr::ungroup() %>% 
  dplyr::filter(value)

da_pred_valid %>% 
  dplyr::group_by(descr_arma_fogo) %>% 
  dplyr::summarise(
    n = dplyr::n(),
    acuracia = mean(acertou_arvore),
    trivial = pmax(mean(y), mean(1-y)),
  ) %>% 
  dplyr::mutate(dplyr::across(c(acuracia, trivial), scales::percent)) %>% 
  dplyr::arrange(dplyr::desc(acuracia)) %>% 
  knitr::kable()

beta <- da_pred_valid %>%   # Aqui tentei consolidar os resultados por arma (tipo+calibre+marca). Faz sentido?               
  dplyr::group_by(descr_arma_fogo, marca_arma2, calibre_arma2) %>%  
  dplyr::summarise(
    n = dplyr::n(),
    acuracia = mean(acertou_arvore),
    trivial = pmax(mean(y), mean(1-y)),
  ) %>% 
  dplyr::mutate(dplyr::across(c(acuracia, trivial), scales::percent)) %>% 
  dplyr::arrange(dplyr::desc(acuracia)) 


broom::tidy(model) %>% 
  dplyr::mutate(
    term = forcats::fct_reorder(term, estimate),
    c1 = estimate + std.error * qnorm(.025),
    c2 = estimate + std.error * qnorm(.975)
  ) %>% 
  dplyr::filter(
    p.value < .05
  ) %>% 
  ggplot2::ggplot() +
  ggplot2::aes(estimate, term) +
  ggplot2::geom_point() +
  ggplot2::geom_errorbar(
    ggplot2::aes(xmin = c1, xmax = c2)
  ) +
  ggplot2::geom_vline(xintercept = 0, colour = 2, linetype = 2) +
  ggplot2::labs(
    x = "Efeito na probabilidade de ser 'apreendida'"
  )

coef(model_lasso) %>% 
  as.matrix() %>% 
  as.data.frame() %>% 
  tibble::rownames_to_column() %>% 
  tibble::as_tibble() %>% 
  janitor::clean_names() %>% 
  dplyr::filter(x1 != 0) %>% 
  dplyr::mutate(
    rowname = stringr::str_trunc(rowname, 30),
    rowname = forcats::fct_reorder(rowname, x1)
  ) %>% 
  ggplot2::ggplot() +
  ggplot2::aes(x1, rowname) +
  ggplot2::geom_point() +
  ggplot2::geom_vline(xintercept = 0, colour = 2, linetype = 2) +
  ggplot2::labs(
    x = "Efeito na probabilidade de ser 'apreendida'",
    y = "Variável"
  )

da_pred_valid %>% 
  tidyr::pivot_longer(dplyr::starts_with("prob")) %>% 
  ggplot2::ggplot(ggplot2::aes(d = y, m = value)) +
  ggplot2::facet_wrap(~name, ncol = 2) +
  plotROC::geom_roc() +
  plotROC::style_roc()


# Export -----------------------------------------------------------------------

# readr::write_rds(d, "")


