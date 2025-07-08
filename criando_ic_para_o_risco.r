library(tidymodels)

tidymodels::tidymodels_prefer()

# Função para gerar dados (mantida igual)
gerando_dados <- function(n = 350L, ...) {
  f <- function(x, sd = 0.5, ...) {
    45 * tanh(x / 1.9 - 7) + 57 + rnorm(n, sd = sd)
    #45 * tanh(x)^7 + 4 + rnorm(n = n, ...)
  }

  tibble(x = runif(n = n, min = 0, max = 20)) |>
    mutate(y = f(x, ...)) |>
    relocate(y, .before = x)
}

set.seed(123)
dados <- gerando_dados(n = 250, mean = 0, sd = 3.5)

# Dividindo entre treino e teste
dados_split <- rsample::initial_split(dados, prop = 0.8, strata = y)
teste <- rsample::testing(dados_split)
treino <- rsample::training(dados_split)

# Criando receita
receita <- recipe(y ~ ., data = treino) |>
  step_poly(all_numeric_predictors(), degree = tune("p")) |>
  step_normalize(all_numeric_predictors())

# Preparando a receita do modelo
# receita <- receita |>
#   prep()

# # Visualizando a receita preparada com bake
# bake_receita <- bake(receita, new_data = data.frame(y = 3.4, x = 5.6))

# Especificando o modelo linear_reg com o parnip
modelo <- parsnip::linear_reg() |>
  parsnip::set_mode("regression") |>
  parsnip::set_engine("lm")

# Unir receita + modelo (workflow)

wf <- workflow() |>
  add_recipe(receita) |>
  add_model(modelo)

# Criando um conjunto de validação cruzada
cv <- rsample::vfold_cv(treino, v = 5)


# Tunando o modelo
tune_res <- tune_grid(
  wf,
  resamples = cv,
  grid = 25,
  control = control_grid(save_pred = TRUE)
)
tune_res |> show_best(metric = "rmse")

# Criando untervalo de confiança para o risco preditivo
add_ci_t <- function(metrics_tbl, level = 0.95) {
  alpha <- 1 - level
  metrics_tbl %>%
    mutate(
      t_crit = qt(1 - alpha / 2, df = n - 1),
      lower = mean - t_crit * std_err,
      upper = mean + t_crit * std_err
    )
}
add_ci_t(collect_metrics(tune_res))
tune::int_pctl(tune_res) # via bootstrap
