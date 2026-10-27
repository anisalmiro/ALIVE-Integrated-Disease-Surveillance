# ============================================================
# PAINEL AVANÇADO DE MONITORIA E QUALIDADE DE DADOS
# Projecto ALIVE — WP2 / DHIS2 Tracker
# ============================================================


#deploy this app
if(FALSE){


library(rsconnect)

# Garante que instala uma versão do terra compatível com o GDAL 3.4.1 do servidor
if (!requireNamespace("terra", quietly = TRUE) || packageVersion("terra") > "1.7-71") {
  install.packages("terra", repos = "https://packagemanager.posit.co/cran/2024-05-01")
}

  
  # 2. (Opcional) Se a aplicação usa geoprocessamento em R
  # library(terra) 
  
  # 3. Executar o Deploy
  rsconnect::deployApp(
    appDir = ".",
    appName = "painel_monitoria_dados_shiny",
    appFiles = c(
      "app.R",
      "R",
      "config",
      "www",
      "data-raw/dados_processados.csv",
      "data-raw/AGREGADOS.csv",
      "data-raw/Coordenadas_Geograficas_SARA.csv",
      "gerar_relatorio_sem_painel.R"
    ),
    dependencyResolution = "library",
    forceUpdate = TRUE
  )

}
#_____________________________
# ============================================================
# PAINEL AVANÇADO DE MONITORIA E QUALIDADE DE DADOS
# Projecto ALIVE — WP2 / DHIS2 Tracker
# ============================================================

# 1. Create www folder if missing
if (!dir.exists("www")) {
  dir.create("www")
}

# 2. Create data-raw directory & placeholder CSV if missing
if (!dir.exists("data-raw")) {
  dir.create("data-raw", recursive = TRUE)
}

if (!file.exists("data-raw/dados_processados.csv")) {
  
  file.create("data-raw/dados_processados.csv")
}

library(readr)     # <--- Garante a função read_csv
library(dplyr)
library(plotly)
library(dplyr)
library(readr)
library(lubridate)
library(ggplot2)
library(plotly)
library(DT)
library(shinydashboard)
library(leaflet)
required_packages <- c(
  "shiny",
  "shinydashboard",
  "readr",
  "dplyr",
  "tidyr",
  "tibble",
  "stringr",
  "lubridate",
  "purrr",
  "ggplot2",
  "plotly",
  "DT",
  "scales",
  "openxlsx",
  "httr",
  "jsonlite"
)


missing_packages <- required_packages[
  !vapply(
    required_packages,
    requireNamespace,
    logical(1),
    quietly = TRUE
  )
]


if(length(missing_packages) > 0){
  
  stop(
    paste(
      "Missing packages:",
      paste(missing_packages, collapse=", "),
      "\nInstall them locally before deployment."
    ),
    call.=FALSE
  )
  
}


source("R/config.R", local = TRUE)
source("R/functions.R", local = TRUE)
source("R/auto_update.R", local = TRUE)
source("R/ui.R", local = TRUE)
source("R/server.R", local = TRUE)


shinyApp(
  ui = ui,
  server = server
)