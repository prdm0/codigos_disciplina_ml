library(tidymodels)
library(visdat)
library(ggplot2)

tidymodels::tidymodels_prefer()

load("~/Downloads/dados_expectativa_renda.RData")

dados <- dados_expectativa_renda

#! Visualizando os dados
p1 <- visdat::vis_dat(dados)

set.seed(123)
dados_split <- rsample::initial_split(
  dados,
  prop = 0.8,
  strata = LifeExpectancy
)
treino <- rsample::training(dados_split)
teste <- rsample::testing(dados_split)

# dados_split <- rsample::initial_validation_split(dados, prop = c(0.6, 0.2), strata = LifeExpectancy)
# treino <- rsample::training(dados_split)
# validacao <- rsample::validation(dados_split)
# teste <- rsample::testing(dados_split)

cv <- rsample::vfold_cv(treino, v = 3, repeats = 1)
loocv <- rsample::loo_cv(treino)

cv_tidy <- tidy(cv)

#* Visualizando o comjunto de validação cruzada (3-fold cross-validation)
p2 <-
  cv_tidy |>
  ggplot(aes(x = Fold, y = Row, fill = Data)) +
  geom_tile() +
  scale_fill_brewer() +
  theme_classic() +
  ggtitle("Visualizando o conjunto de validação cruzada")

receita <-
  recipes::recipe(LifeExpectancy ~ GDPercapita, data = treino) |>
  recipes::step_poly(GDPercapita, degree = tune("p"))

# receita_prep <- prep(receita)
# receita_bake <- bake(receita_prep, new_data = NULL)

#* Definindo o modelo
modelo <- linear_reg() |>
  set_mode("regression") |>
  set_engine("lm")

#* Criando workflow
wf <- workflow() |>
  add_recipe(receita) |>
  add_model(modelo)

# Vamos tunar o grau do polinomio
tunagem <- tune::tune_grid(
  wf,
  resamples = cv,
  metrics = yardstick::metric_set(rmse),
  control = control_grid(save_pred = TRUE)
)

int_pctl(tunagem)


# Só retornará o \hat{y}
# predict(wf, new_data = teste)
previsoes <- augment(wf, new_data = teste)
yardstick::rmse(previsoes, truth = LifeExpectancy, estimate = .pred)

# Você pode implementar suas abordagens usando o tidymodels.
# minha_tunagem <- function(p, treino, teste){
#   receita <-
#     recipes::recipe(LifeExpectancy ~ GDPercapita, data = treino) |>
#     recipes::step_poly(GDPercapita, degree = p)
#   #* Definindo o modelo
#   modelo <- linear_reg() |>
#     set_mode("regression") |>
#     set_engine("lm")

#   wf <- workflow() |>
#     add_recipe(receita) |>
#     add_model(modelo) |>
#     fit(treino)
#   previsoes <- augment(wf, new_data = teste)
#   yardstick::rmse(previsoes, truth = LifeExpectancy, estimate = .pred)
# }

# minha_tunagem(p = 6, treino = treino, teste = teste)
