library(tidymodels)
library(ggplot2)
library(dplyr)
library(patchwork)

tidymodels::tidymodels_prefer(quiet = TRUE)

#! Função de regressão real
gerando_dados <- function(n = 350L, ...) {
  f <- function(x, sd = 0.5, ...) {
    45 * tanh(x / 1.9 - 7) + 57 + rnorm(n, sd = sd)
    #45 * tanh(x)^3 + 4 + rnorm(n = n, ...)
  }

  tibble(x = runif(n = n, min = 0, max = 35)) |>
    mutate(y = f(x, ...)) |>
    relocate(y, .before = x)
}

# Gerando dados ----------------------------------------------------------
set.seed(123)
dados <- gerando_dados(n = 550L, sd = 3)
dados$id <- 1:nrow(dados)

p1 <- dados |>
  ggplot(aes(x = x, y = y)) +
  geom_point()

# Holdout ----------------------------------------------------------------
dados_split <- rsample::initial_split(dados, prop = 0.8, strata = y)
treino <- training(dados_split)
teste <- testing(dados_split)
cv <- rsample::vfold_cv(treino, v = 5, strata = y, repeats = 1)

cv_tidy <- tidy(cv)

p2 <-
  cv_tidy |>
  ggplot(aes(x = Fold, y = Row, fill = Data)) +
  geom_tile() +
  scale_fill_brewer() +
  theme_classic() +
  guides(fill = guide_legend(title = "Validação"))

f <- function(x) {
  (x - mean(x)) / sd(x)
}

receita <- recipe(y ~ ., data = treino) |>
  recipes::update_role(id, new_role = "id") |>
  recipes::step_poly(degree = 10) |>
  recipes::step_mutate(across(.cols = x, .fns = f))

# a <- prep(receita) |> bake(new_data = NULL)

# Definindo o modelo -----------------------------------------------------
modelo <- parsnip::linear_reg() |>
  parsnip::set_mode("regression") |>
  parsnip::set_engine("lm")

# Criando workflow -------------------------------------------------------
wf <- workflow() |>
  add_recipe(receita) |>
  add_model(modelo) |>
  fit(data = treino)

d <- parameters(trees(), mixture())

d1 <- grid_space_filling(d, size = 30, type = "max_entropy")
d2 <- grid_space_filling(d, size = 30, type = "uniform")
d3 <- grid_space_filling(d, size = 30, type = "audze_eglais")
d4 <- grid_space_filling(d, size = 30, type = "latin_hypercube")

plot_grid <- function(d) {
  d |>
    ggplot(aes(x = trees, y = mixture)) +
    geom_point(size = 5)
}

(plot_grid(d1) | plot_grid(d2)) / (plot_grid(d3) | plot_grid(d4))
