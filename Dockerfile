# Start with a base R image
FROM rocker/r-ver:4.5.0

RUN apt-get update -qq && apt-get install -y --no-install-recommends \
  libcurl4-openssl-dev \
  libicu-dev \
  libsodium-dev \
  libssl-dev \
  libx11-dev \
  make \
  zlib1g-dev \
  && apt-get clean


# Define o diretório de trabalho. Todos os comandos seguintes
# serão executados a partir daqui.
WORKDIR /app

# Instala as dependências R. Esta camada só será reconstruída
# se este comando mudar, aproveitando o cache do Docker.
RUN R -e "install.packages('plumber')"

# Copia o script da API para o diretório de trabalho atual (/app).
# Usar "." como destino é uma prática comum que se refere ao WORKDIR.
COPY api_simples_plumber.r /opt/api/api_simples_plumber.r

# Expõe a porta em que o plumber irá escutar
EXPOSE 8000

# Comando para rodar a API. Como estamos em /app,
# podemos nos referir ao script pelo seu nome (caminho relativo).
CMD ["Rscript", "-e", "pr <- plumber::pr('/opt/api/api_simples_plumber.r'); pr$run(host='0.0.0.0', port=8000)"]