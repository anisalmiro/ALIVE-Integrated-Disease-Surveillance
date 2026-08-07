#!/usr/bin/env Rscript
# ============================================================
# SCRIPT DE INICIALIZAÇÃO DO PAINEL COM AUTO-UPDATE
# ============================================================
#
# Este script inicia o painel com a funcionalidade de 
# atualização automática de dados a cada 1 hora.
#
# Uso: Rscript iniciar_painel_com_auto_update.R
#
# ANTES DE USAR:
# 1. Configure os valores de utilizador e palavra-passe abaixo
# 2. OU defina variáveis de ambiente no seu sistema
# 3. OU crie um ficheiro .Renviron no diretório raiz
#
# ============================================================

# Configurar credenciais do DHIS2
# ⚠️  NÃO RECOMENDADO para produção - use variáveis de ambiente!
Sys.setenv(
  DHIS2_USERNAME = Sys.getenv("DHIS2_USERNAME", unset = "abule"),
  DHIS2_PASSWORD = Sys.getenv("DHIS2_PASSWORD", unset = "An1s1@1990$")
)

cat("\n")
cat("═══════════════════════════════════════════════════════════════\n")
cat("  PAINEL AVANÇADO DE MONITORIA E QUALIDADE DE DADOS\n")
cat("  Projecto ALIVE — WP2 / DHIS2 Tracker\n")
cat("═══════════════════════════════════════════════════════════════\n")
cat("\n")

# Verificar se as credenciais foram configuradas
username <- Sys.getenv("DHIS2_USERNAME")
password <- Sys.getenv("DHIS2_PASSWORD")

if (nchar(username) > 0 && nchar(password) > 0) {
  cat("✓ Auto-update ACTIVADO\n")
  cat("  - Credenciais DHIS2 detectadas\n")
  cat("  - Dados serão actualizados a cada 1 hora\n")
} else {
  cat("⚠ Auto-update DESACTIVADO\n")
  cat("  - Credenciais DHIS2 não configuradas\n")
  cat("  - O painel funcionará, mas sem atualizar automaticamente\n")
}

cat("\n")
cat("Iniciando painel...\n")
cat("Abra o browser em: http://localhost:3838\n")
cat("Pressione Ctrl+C para encerrar\n")
cat("\n")

# Iniciar o painel
shiny::runApp(
  appDir = ".",
  host = "127.0.0.1",
  port = 3838,
  launch.browser = FALSE
)
