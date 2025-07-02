library(dplyr)
library(ggplot2)

rm(list = ls())

validacao_cruzada <- function(x, k = 3L) {
  # x: data frame que será dividido em k partes aproximadamente iguais
  # k: número de partes (folds) para a validação cruzada
  n <- nrow(x)
  x <- x[sample(nrow(x)), ]
  folds <- cut(1L:n, breaks = k, labels = FALSE)
  x$folds <- folds
  treino_validacao <- function(v_fold, split) {
    validacao <- x[x$folds == v_fold, ]
    treino <- x[x$folds != v_fold, ]
    validacao$info <- "validacao"
    treino$info <- "treino"
    d <- rbind(treino, validacao)
    d$split <- split
    d
  }

  d <- purrr::map_dfr(
    .x = 1L:k,
    .f = \(i) treino_validacao(i, split = i)
  )
  d$folds <- NULL
  d
}

#! Função de regressão real
gerando_dados <- function(n = 350L, ...) {
  f <- function(x, sd = 0.5, ...) {
    45 * tanh(x / 1.9 - 7) + 57 + rnorm(n, sd = sd)
    #45 * tanh(x)^3 + 4 + rnorm(n = n, ...)
    #x^7 + rnorm(n = n, ...)
  }

  tibble(x = runif(n = n, min = .Machine$double.xmin, max = 35)) |>
    mutate(y = f(x, ...)) |>
    relocate(y, .before = x)
}

tunagem <- function(cv, p_max = 25L) {
  # cv: data frame com os dados de treino e validação cruzada
  # p_max: grau máximo do polinômio a ser ajustado

  k <- max(cv$split)

  regressao <- function(p, i) {
    treino <- cv |>
      dplyr::filter(split == i, info == "treino")
    lm(y ~ poly(x, degree = p), data = treino)
  }

  avaliacao <- function(p) {
    one_step <- function(i) {
      r <- regressao(p, i)
      validacao <- cv |>
        dplyr::filter(split == i, info == "validacao")
      pred <- predict(r, newdata = validacao)
      mse <- mean((pred - validacao$y)^2)
      tibble(p = p, split = i, mse = mse)
    }
    purrr::map_dfr(.x = 1L:k, .f = one_step)
  }
  resultados <- purrr::map_dfr(.x = 1L:p_max, .f = avaliacao)
  resultados |>
    group_by(p) |>
    summarise(mse = mean(mse))
}

set.seed(123)
dados <- gerando_dados(n = 150, mean = 0, sd = 5.5)

#! Conjunto de validação cruzada
cv <- validacao_cruzada(dados, k = 5L)

r <- tunagem(cv, p_max = 25)
r |>
  ggplot(aes(x = p, y = mse)) +
  geom_line()


# Plotando a regressao polinomial com o melhor grau
melhor_grau <- r |>
  filter(mse == min(mse)) |>
  pull(p) |>
  as.integer()

modelo_final <- lm(y ~ poly(x, degree = melhor_grau), data = dados)
dados |>
  mutate(y_chapeu = predict(modelo_final)) |>
  ggplot(aes(x = x, y = y)) +
  geom_point(alpha = 0.5) +
  geom_line(aes(y = y_chapeu), color = "blue", size = 1) +
  labs(title = paste("Polinômio de grau", melhor_grau, "ajustado aos dados")) +
  theme_minimal()
