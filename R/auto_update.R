# ============================================================
# ATUALIZAÇÃO AUTOMÁTICA DE DADOS DO SERVIDOR DHIS2
# ============================================================

library(httr)
library(jsonlite)
library(dplyr)
library(later)

Sys.setenv(DHIS2_USERNAME = "abule")
Sys.setenv(DHIS2_PASSWORD = "An1s1@1990$")


get_dhis2_events <- function(
    username = Sys.getenv("DHIS2_USERNAME"),
    password = Sys.getenv("DHIS2_PASSWORD"),
    page_size = 100) {
  
  # Validar credenciais
  if (nchar(username) == 0 || nchar(password) == 0) {
    stop("Credenciais DHIS2 não configuradas. Defina DHIS2_USERNAME e DHIS2_PASSWORD.")
  }
  
  base_url <- "https://dados.clima.org.mz/sive/api/29/analytics/events/query/o4fkQbIZgUV.json"
  
  dimensions <- c(
    "pe:2025;2026",
    "ou:USER_ORGUNIT",
    "sB1IHYu2xQT",
    "ENRjVGxVL6l",
    "CklPZdOd6H1",
    "MZeTF32uOUD",
    "Q1NM5BSIxl0",
    "VCtm2pySeEV",
    "oNN1FVNaV4r.M9GNJpUsAeT",
    "ANXuo6saopx",
    "oNN1FVNaV4r.eMVytXtuMWh",
    "oNN1FVNaV4r.teO9EjrYeF6",
    "oNN1FVNaV4r.mtkN9C1BJX4",
    "oNN1FVNaV4r.rLDcMERXNrS",
    "oNN1FVNaV4r.vrJV8duHFUy",
    "oNN1FVNaV4r.BsKcBDC0kSq",
    "oNN1FVNaV4r.kP29shPSHl8",
    "oNN1FVNaV4r.sh6WSMuKbof",
    "oNN1FVNaV4r.F4XfAlCCrf2",
    "oNN1FVNaV4r.nRdPCxj7d9z",
    "oNN1FVNaV4r.exTgdok33ph",
    "oNN1FVNaV4r.HG5ltiCtn1D",
    "oNN1FVNaV4r.vK9PPhCrA6Q"
  )
  
  dim_string <- paste0("dimension=", dimensions, collapse = "&")
  
  pagina <- 1
  todos_dados <- list()
  
  repeat {
    other_params <- paste(
      "stage=oNN1FVNaV4r",
      "displayProperty=NAME",
      "totalPages=true", 
      "outputType=EVENT",
      "desc=eventdate",
      paste0("pageSize=", page_size),
      paste0("page=", pagina),
      sep = "&"
    )
    
    full_url <- paste0(base_url, "?", dim_string, "&", other_params)
    
    cat("  [Download] Buscando página:", pagina, "\n")
    
    response <- httr::GET(
      full_url,
      httr::authenticate(username, password, type = "basic"),
      httr::timeout(60)
    )
    
    if (httr::status_code(response) != 200) {
      stop(paste("Erro HTTP:", httr::status_code(response)))
    }
    
    content_json <- httr::content(response, as = "text", encoding = "UTF-8")
    data_list <- jsonlite::fromJSON(content_json, simplifyVector = FALSE)
    
    num_rows <- length(data_list$rows)
    total_pages <- data_list$metaData$pager$pageCount
    
    # PARADA: Nenhuma linha retornada
    if (is.null(data_list$rows) || num_rows == 0) {
      cat("  [Download] Sem mais registros. Finalizando paginação.\n")
      break
    }
    
    # Converter rows para dataframe
    dados_temp <- as.data.frame(do.call(rbind, data_list$rows),
                                stringsAsFactors = FALSE)
    
    # Extrair nomes das colunas
    headers_names <- sapply(data_list$headers, function(h) h$name)
    colnames(dados_temp) <- headers_names
    
    todos_dados[[pagina]] <- dados_temp
    
    # PARADA: Chegou ao limite total de páginas da API
    if (!is.null(total_pages) && pagina >= total_pages) {
      cat("  [Download] Última página alcançada (", num_rows, "registos ).\n")
      break
    }
    
    # PARADA: Página incompleta indica última página
    if (num_rows < page_size) {
      cat("  [Download] Última página alcançada (", num_rows, "registos ).\n")
      break
    }
    
    pagina <- pagina + 1
  }
  
  if (length(todos_dados) == 0) {
    return(data.frame())
  }
  
  dados_final <- dplyr::bind_rows(todos_dados)
  return(dados_final)
}


download_and_save_dhis2_data <- function(
    output_path = file.path("data-raw", "dados_processados.csv"),
    username = Sys.getenv("DHIS2_USERNAME"),
    password = Sys.getenv("DHIS2_PASSWORD")) {
  
  tryCatch({
    cat("[Auto-update]", format(Sys.time(), "%d/%m/%Y %H:%M:%S"), "- Iniciando download de dados...\n")
    
    # Executa download até a última página
    dados <- get_dhis2_events(username, password)
    
    if (nrow(dados) == 0) {
      cat("[Auto-update] AVISO: Nenhum dado foi retornado do servidor.\n")
      return(FALSE)
    }
    
    # Limpar dados - converter listas em strings
    dados_limpo <- dados
    dados_limpo[] <- lapply(dados_limpo, function(col) {
      if (is.list(col)) {
        sapply(col, function(x) paste(unlist(x), collapse = ";"))
      } else {
        col
      }
    })
    
    # Garantir diretório de saída
    dir_path <- dirname(output_path)
    if (!dir.exists(dir_path)) {
      dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
    }
    
    # Salvar CSV
    write.csv(dados_limpo,
              output_path,
              row.names = FALSE,
              na = "")
    
    # MENSAGEM DE SUCESSO QUE SINALIZA A PARADA DA EXECUÇÃO ATUAL
    success_msg <- sprintf("[Auto-update] %s - Sucesso! Guardados %d registos em %s\n",
                           format(Sys.time(), "%d/%m/%Y %H:%M:%S"),
                           nrow(dados_limpo),
                           output_path)
    
    cat(success_msg)
    
    # Interrompe o processo de download da rodada atual
    return(TRUE)
    
  }, error = function(e) {
    cat("[Auto-update] ERRO:", conditionMessage(e), "\n")
    return(FALSE)
  })
}

# ============================================================
# AGENDAMENTO PARA RE-EXECUTAR APENAS NA PRÓXIMA HORA (3600s)
# ============================================================

schedule_auto_update <- function() {
  status <- download_and_save_dhis2_data()
  
  if (status) {
    cat("--> Download concluído com sucesso. Aguardando 1 hora para o próximo ciclo...\n")
  } else {
    cat("--> Falha no download. Tentando novamente em 1 hora...\n")
  }
  
  # Re-agenda para rodar daqui a 1 hora (3600 segundos) sem bloquear runApp()
  later::later(schedule_auto_update, 3600)
}

# Iniciar o ciclo
schedule_auto_update()