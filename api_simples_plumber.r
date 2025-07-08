library(plumber)

#* Criando um histograma
#* @get /hist
#* @serializer svg
histograma <- function() {
  dados <- rnorm(100)
  hist(dados)
}
