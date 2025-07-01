library(ggplot2)
library(dplyr)
library(purrr)
library(patchwork)
library(rsample)

rm(list = ls())

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

# Avaliação ingênua (ajustada)
avaliacao_ingenua <- function(dados, p_max = 25) {
  avaliacao <- function(p) {
    regressao <- lm(y ~ poly(x, degree = p), data = dados)

    r <- dados |>
      mutate(y_chapeu = predict(regressao)) |>
      summarise(eqm = mean((y - y_chapeu)^2))

    tibble(p = p, eqm = r$eqm)
  }
  purrr::map_dfr(1:p_max, avaliacao)
}

# Avaliação com holdout (ajustada)
avaliacao_holdout <- function(dados, p_max = 25, seed = 123, ...) {
  split <- initial_split(dados, prop = 0.8, strata = y, ...)
  treino <- training(split)
  teste <- testing(split)
  avaliacao <- function(p) {
    regressao <- lm(y ~ poly(x, degree = p), data = treino)

    r <- teste |>
      mutate(y_chapeu = predict(regressao, newdata = teste)) |>
      summarise(eqm = mean((y - y_chapeu)^2))

    tibble(p = p, eqm = r$eqm)
  }
  purrr::map_dfr(1:p_max, avaliacao)
}

# Gerar dados
set.seed(123)
dados <- gerando_dados(n = 250, mean = 0, sd = 5.5)

# Avaliações
tbl_ingenua <- avaliacao_ingenua(dados, p_max = 25)
tbl_holdout <- avaliacao_holdout(dados, p_max = 25)

# Gráficos
p1 <- tbl_ingenua |>
  ggplot(aes(x = p, y = eqm)) +
  geom_line() +
  labs(title = "Avaliação Ingênua (treino)")

p2 <- tbl_holdout |>
  ggplot(aes(x = p, y = log(eqm))) +
  geom_line() +
  geom_point(data = slice_min(tbl_holdout, eqm), color = "red", size = 3) +
  labs(title = "Avaliação Holdout (teste)", y = "log(EQM)") +
  scale_x_continuous(breaks = seq(1, 25, 2))

# Visualização dos dados
p_dados <- dados |>
  ggplot(aes(x = x, y = y)) +
  geom_point(alpha = 0.5) +
  labs(title = "Dados Observados")

# Layout final
(p_dados) / (p1 | p2)

# Plotando sobre p_dados o polinômio de melhor grau, polinômio de grau 6
melhor_grau <- which.min(tbl_holdout$eqm)
dados |>
  mutate(
    y_chapeu = predict(lm(y ~ poly(x, degree = melhor_grau), data = dados))
  ) |>
  ggplot(aes(x = x, y = y)) +
  geom_point(alpha = 0.5) +
  geom_line(aes(y = y_chapeu), color = "blue", size = 1) +
  labs(title = paste("Polinômio de grau", melhor_grau, "ajustado aos dados")) +
  theme_minimal()
