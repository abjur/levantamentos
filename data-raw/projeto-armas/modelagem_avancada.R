

# validação cruzada -------------------------------------------------------


#' Ajusta modelos de árvores
ajustar_modelos <- function (base_filtrada, .y = NULL) {
  
  ## para debugar
  # browser()
  
  if (!is.null(.y)) {
    message(stringr::str_glue("Ajustando modelo {.y}..."))
  }
  
  # Parte 1) Validação cruzada
  split <- rsample::initial_split(base_filtrada, .8, strata = y)
  treino <- rsample::training(split)
  # essa é a base que a gente esconde
  teste <- rsample::testing(split)
  
  # cria base de validação cruzada para ajustar modelos
  folds <- rsample::vfold_cv(treino)
  
  # pre processamento
  receita <- recipes::recipe(y ~ ., data = treino) %>%
    recipes::step_dummy(recipes::all_nominal_predictors()) %>%
    recipes::step_zv(recipes::all_predictors()) %>% 
    recipes::step_bin2factor(y)
  
  
  # especificacao do modelo de arvore
  tree_model <- parsnip::decision_tree(
    cost_complexity = tune::tune(),
    min_n = tune::tune(),
    tree_depth = tune::tune()
  ) %>% 
    parsnip::set_engine("rpart") %>%
    parsnip::set_mode("classification")
  
  wf <- workflows::workflow() %>% 
    workflows::add_recipe(receita) %>% 
    workflows::add_model(tree_model)
  
  # escolhi essa grid para que o modelo não ficasse muito complexo
  grid <- dials::grid_regular(
    dials::cost_complexity(c(-5, -1)),
    dials::min_n(c(20L, 20L)),
    dials::tree_depth(c(3L, 5L)), 
    levels = 5
  )
  
  modelos <- wf %>% 
    tune::tune_grid(
      resamples = folds,
      grid = grid,
      metrics = yardstick::metric_set(
        yardstick::roc_auc, 
        yardstick::accuracy
      ),
      control = tune::control_grid(
        verbose = is.null(.y), 
        save_pred = TRUE
      )
    )
  
  # decide qual modelo ficou melhor, segundo acurácia
  melhor_configuracao_modelo <- modelos %>% 
    tune::show_best("accuracy", n = 1)

    # Parte 2) Ajustamos na base de treino completa
  last_model <- parsnip::decision_tree(
    tree_depth = melhor_configuracao_modelo$tree_depth, 
    min_n = melhor_configuracao_modelo$min_n, 
    cost_complexity = melhor_configuracao_modelo$cost_complexity
  ) %>% 
    set_engine("rpart") %>% 
    set_mode("classification")
  
  last_wf <- workflows::workflow() %>% 
    workflows::add_recipe(receita) %>% 
    workflows::add_model(last_model)
  
  last_fit <- tune::last_fit(last_wf, split)
  last_fit
  
}



# ajuste dos modelos ------------------------------------------------------

# a base "da" é a obtida no script anterior.
# o algoritmo considerará como variáveis explicativas todas
# as variáveis que estiverem nessa base de dados

# modelo com todos os dados
modelo_completo <- da %>% 
  dplyr::select(-tipo_arma) %>% 
  ajustar_modelos()

# modelos para cada grupo de armas
modelos_por_tipo <- da %>% 
  dplyr::group_by(tipo_arma) %>% 
  tidyr::nest() %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(modelo = purrr::imap(data, ajustar_modelos))

## salvando ajustes
readr::write_rds(modelo_completo, "modelo_completo.rds", compress = "xz")
readr::write_rds(modelos_por_tipo, "modelos_por_tipo.rds", compress = "xz")

# comparação dos modelos

# vamos comparar as acurácias desses modelos, no geral e por grupo de armas

calcular_acuracia <- function(dados) {
  dados %>% 
    dplyr::mutate(
      ypred = ifelse(.pred_yes > .5, "yes", "no"),
      ypred = factor(ypred, c("yes", "no"))
    ) %>% 
    yardstick::accuracy(y, ypred)
}

# acuracia geral do modelo completo
modelo_completo$.predictions[[1]] %>% 
  calcular_acuracia()

# acuracia geral do modelo por grupo
modelos_por_tipo %>% 
  tidyr::unnest(modelo) %>% 
  dplyr::select(tipo_arma, .predictions) %>% 
  tidyr::unnest(.predictions) %>% 
  calcular_acuracia()


# acuracia por grupo do modelo completo
modelo_completo$.predictions[[1]] %>% 
  dplyr::mutate(tipo_arma = da$tipo_arma[.row]) %>% 
  dplyr::group_by(tipo_arma) %>% 
  calcular_acuracia()

# acuracia por grupo do modelo por grupo
modelos_por_tipo %>% 
  tidyr::unnest(modelo) %>% 
  dplyr::select(tipo_arma, .predictions) %>% 
  tidyr::unnest(.predictions) %>% 
  dplyr::group_by(tipo_arma) %>% 
  calcular_acuracia()

# desenho das arvores -----------------------------------------------------

desenhar_arvore_modelo <- function(modelo, titulo = NULL) {
  fit <- modelo$.workflow[[1]]$fit$fit$fit
  # TODO melhore essa função para que a árvore fique bonitinha
  # considerar a função rpart.plot::rpart.plot() como opção
  suppressWarnings(rpart.plot::prp(fit)) 
  if (!is.null(titulo)) title(titulo)
}

par(mfrow = c(1,1))
desenhar_arvore_modelo(modelo_completo, "Árvore completa")

par(mfrow = c(2, 2))
desenhar_arvore_modelo(modelos_por_tipo$modelo[[2]], "Espingarda\n")
desenhar_arvore_modelo(modelos_por_tipo$modelo[[3]], "Revolver\n")
desenhar_arvore_modelo(modelos_por_tipo$modelo[[5]], "Rifle\n")
desenhar_arvore_modelo(modelos_por_tipo$modelo[[6]], "Outros\n")

# esses aqui ficaram só o "toco". O modelo não identificou nenhuma variável
# que ajudasse a predizer o resultado
par(mfrow = c(2, 2))
desenhar_arvore_modelo(modelos_por_tipo$modelo[[1]], "Pistola")
desenhar_arvore_modelo(modelos_por_tipo$modelo[[4]], "Garrucha")
desenhar_arvore_modelo(modelos_por_tipo$modelo[[7]], "Metralhadora")

