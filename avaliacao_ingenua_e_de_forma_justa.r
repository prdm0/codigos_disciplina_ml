library(tidymodels)
library(dplyr)
library(future)
library(patchwork)

rm(list = ls(all = TRUE))

tidymodels::tidymodels_prefer(quiet = TRUE)

future::plan(future::multicore)

# Abordagem ingênua ------------------------------------------------------
gerando_dados <- function(n = 500L, ...) {
  f <- function(x, sd = 0.5) {
    10 * tanh(x)^3 + 4 + rnorm(n = n, mean = 0, ...)
  }

  tibble(x = runif(n = n, min = -3, max = 3)) |>
    mutate(y = f(x, ...))
}

set.seed(123)
dados <- gerando_dados(n = 100L, sd = 0.5)

avaliacao_ingenua <- function(dados, p) {
  receita <-
    recipe(y ~ ., data = dados) |>
    step_poly(all_predictors(), degree = p, options = list(raw = TRUE))

  modelo <- linear_reg() |>
    set_engine("lm") |>
    set_mode("regression")

  wf <- workflow() |>
    add_recipe(receita) |>
    add_model(modelo) |>
    fit(data = dados)

  ajuste <- augment(wf, new_data = dados)
  rmse <- yardstick::rmse(ajuste, truth = y, estimate = .pred)$.estimate

  tibble(
    p = p,
    rmse = rmse
  )
}

futures <- sapply(X = 1:20, FUN = \(i) {
  future(avaliacao_ingenua(dados, p = i), seed = TRUE)
})

tbl_rmse <- value(futures) |> purrr::list_rbind()

p1 <- tbl_rmse |>
  ggplot(aes(x = p, y = rmse)) +
  geom_line()

set.seed(123)
dados_split <- rsample::initial_split(dados, prop = 0.8, strata = y)
treino <- rsample::training(dados_split)
teste <- rsample::testing(dados_split)

avaliacao_mais_justa <- function(treino, teste, p) {
  receita <-
    recipe(y ~ ., data = treino) |>
    step_poly(all_predictors(), degree = p, options = list(raw = TRUE))

  modelo <- linear_reg() |>
    set_engine("lm") |>
    set_mode("regression")

  wf <- workflow() |>
    add_recipe(receita) |>
    add_model(modelo) |>
    fit(data = treino)

  ajuste <- augment(wf, new_data = teste)
  rmse <- yardstick::rmse(ajuste, truth = y, estimate = .pred)$.estimate

  tibble(
    p = p,
    rmse = rmse
  )
}

futures <- sapply(X = 1:20, FUN = \(i) {
  future(avaliacao_mais_justa(treino, teste, p = i), seed = TRUE)
})

tbl_rmse <- value(futures) |> purrr::list_rbind()

p2 <- tbl_rmse |>
  ggplot(aes(x = p, y = rmse)) +
  geom_line()

# Realizando uma validação cruzada ---------------------------------------
avaliacao_mais_justa_com_cv <- function(dados, p) {
  cv <- rsample::vfold_cv(dados, v = 5, repeats = 10)

  receita <-
    recipe(y ~ ., data = dados) |>
    step_poly(all_predictors(), degree = p, options = list(raw = TRUE))

  modelo <- linear_reg() |>
    set_engine("lm") |>
    set_mode("regression")

  wf <- workflow() |>
    add_recipe(receita) |>
    add_model(modelo)

  r <- fit_resamples(
    wf,
    resamples = cv,
    metrics = yardstick::metric_set(rmse),
    control = control_resamples(
      save_pred = TRUE,
      parallel_over = "everything"
    )
  ) |>
    collect_metrics()

  tibble(
    p = p,
    rmse = r$mean
  )
}

set.seed(0)
r <- purrr::map_dfr(
  .x = 1:20,
  .f = ~ avaliacao_mais_justa_com_cv(dados, p = .x)
)

p3 <- r |>
  ggplot(aes(x = p, y = rmse)) +
  geom_line()

(p1 | p2) / p3
