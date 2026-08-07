# Execute este ficheiro apenas uma vez antes de iniciar o painel.

pacotes <- c(
  "shiny", "shinydashboard", "readr", "dplyr", "tidyr", "tibble",
  "stringr", "lubridate", "purrr", "ggplot2", "plotly", "DT",
  "scales", "openxlsx"
)

instalar <- pacotes[
  !vapply(pacotes, requireNamespace, logical(1), quietly = TRUE)
]

if (length(instalar) > 0) {
  install.packages(instalar, dependencies = TRUE)
} else {
  message("Todos os pacotes necessários já estão instalados.")
}

