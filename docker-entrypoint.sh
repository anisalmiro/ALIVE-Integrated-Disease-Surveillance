#!/bin/bash
# ============================================================
# SCRIPT DE INICIALIZAÇÃO EM DOCKER
# ============================================================
# 
# Use este script para iniciar o painel em Docker com
# auto-update de dados ativado.
#
# Exemplo de uso:
# docker run -e DHIS2_USERNAME=abule -e DHIS2_PASSWORD="An1s1@1990$" \
#            -p 3838:3838 painel-alive:latest
#
# ============================================================

set -e

echo ""
echo "═════════════════════════════════════════════════════════════"
echo "  PAINEL AVANÇADO DE MONITORIA E QUALIDADE DE DADOS"
echo "  Projecto ALIVE — WP2 / DHIS2 Tracker (Docker)"
echo "═════════════════════════════════════════════════════════════"
echo ""

# Verificar credenciais
if [ -z "$DHIS2_USERNAME" ] || [ -z "$DHIS2_PASSWORD" ]; then
    echo "⚠️  AVISO: Credenciais DHIS2 não configuradas"
    echo "   Auto-update será DESACTIVADO"
    echo ""
    echo "   Para ativar, execute com:"
    echo "   docker run -e DHIS2_USERNAME=user -e DHIS2_PASSWORD=pass ..."
    echo ""
else
    echo "✓ Credenciais DHIS2 detectadas"
    echo "  Auto-update será ACTIVADO"
    echo "  Intervalo: 1 hora"
    echo ""
fi

# Instalar pacotes se necessário
echo "Verificando pacotes R necessários..."
R --vanilla --quiet -e "
  required <- c('shiny', 'shinydashboard', 'readr', 'dplyr', 'tidyr', 
                'tibble', 'stringr', 'lubridate', 'purrr', 'ggplot2', 
                'plotly', 'DT', 'scales', 'openxlsx', 'httr', 'jsonlite')
  missing <- required[!sapply(required, requireNamespace, quietly=TRUE)]
  if(length(missing) > 0) {
    cat('Instalando:', paste(missing, collapse=', '), '\n')
    install.packages(missing, repos='https://cloud.r-project.org')
  }
"

echo ""
echo "Iniciando painel..."
echo "Acesso em: http://localhost:3838"
echo "Pressione Ctrl+C para encerrar"
echo ""

# Iniciar o painel
R --vanilla --quiet -e "
  Sys.setenv(
    DHIS2_USERNAME = Sys.getenv('DHIS2_USERNAME'),
    DHIS2_PASSWORD = Sys.getenv('DHIS2_PASSWORD')
  )
  shiny::runApp(
    appDir = '.',
    host = '0.0.0.0',
    port = 3838,
    launch.browser = FALSE
  )
"
