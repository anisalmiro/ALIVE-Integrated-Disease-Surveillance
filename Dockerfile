# ============================================================
# Dockerfile para o Painel ALIVE com Auto-Update
# ============================================================
#
# Build:
# docker build -t painel-alive:latest .
#
# Run:
# docker run -e DHIS2_USERNAME=abule -e DHIS2_PASSWORD="pass" \
#            -p 3838:3838 painel-alive:latest
#
# ============================================================

FROM rocker/shiny:4.3.0

# Metadados
LABEL maintainer="Projecto ALIVE <info@alive.org>"
LABEL description="Painel de Monitoria e Qualidade de Dados - Projecto ALIVE WP2"

# Definir timezone
ENV TZ=Africa/Maputo

# Instalar dependências do sistema
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    && rm -rf /var/lib/apt/lists/*

# Copiar o código da aplicação
COPY . /srv/shiny-server/painel-alive/

# Definir working directory
WORKDIR /srv/shiny-server/painel-alive

# Instalar pacotes R (sem salvar output verboso)
RUN R --vanilla --quiet -e "
  packages <- c(
    'shiny', 'shinydashboard', 'readr', 'dplyr', 'tidyr', 
    'tibble', 'stringr', 'lubridate', 'purrr', 'ggplot2', 
    'plotly', 'DT', 'scales', 'openxlsx', 'httr', 'jsonlite'
  )
  install.packages(packages, repos='https://cloud.r-project.org')
"

# Configurar permissões
RUN chmod +x docker-entrypoint.sh && \
    chown -R shiny:shiny /srv/shiny-server/painel-alive

# Expor porta
EXPOSE 3838

# Entrypoint
ENTRYPOINT ["/srv/shiny-server/painel-alive/docker-entrypoint.sh"]
