# ============================================================
# GERA UM RELATÓRIO EXCEL SEM ABRIR O SHINY
# ============================================================

pacotes <- c(
  "readr", "dplyr", "tidyr", "tibble", "stringr", "lubridate",
  "purrr", "scales", "openxlsx"
)
faltam <- pacotes[!vapply(pacotes, requireNamespace, logical(1), quietly = TRUE)]
if (length(faltam)) {
  stop("Instale os pacotes em falta: ", paste(faltam, collapse = ", "), call. = FALSE)
}

source(file.path("R", "config.R"))
source(file.path("R", "functions.R"))

# ALTERE AQUI se o ficheiro tiver outro nome ou estiver noutra pasta.
caminho_csv <- DATA_PATH

# ALTERE AQUI o prazo considerado aceitável para a digitação.
prazo_dias <- 2

# ALTERE AQUI a periodicidade: "day", "week" ou "month".
periodicidade <- "week"

# ALTERE AQUI o mínimo de eventos esperado por unidade em cada período.
minimo_eventos <- 1

dados <- read_monitor_data(caminho_csv) |>
  prepare_monitor_data(late_days = prazo_dias)

datas_validas <- dados$event_date[!is.na(dados$event_date)]
if (!length(datas_validas)) stop("Não foi possível reconhecer a data dos eventos.", call. = FALSE)

inicio <- min(datas_validas)
fim <- max(datas_validas)
saida <- paste0("relatorio_monitoria_qualidade_", Sys.Date(), ".xlsx")

write_quality_workbook(
  dados, saida, inicio, fim,
  unit = periodicidade,
  minimum_events = minimo_eventos,
  late_days = prazo_dias
)

message("Relatório criado: ", normalizePath(saida))
