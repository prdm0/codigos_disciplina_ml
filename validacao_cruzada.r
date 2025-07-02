library(dplyr)

validacao_cruzada <- function(x, k = 3L) {
  # x: data frame que será dividido em k partes aproximadamente iguais
  # k: número de partes (folds) para a validação cruzada
  n <- nrow(x)
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

d <- validacao_cruzada(mtcars, k = 5)
View(d)
