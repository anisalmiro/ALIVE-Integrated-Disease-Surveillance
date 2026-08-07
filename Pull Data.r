library(httr)
library(jsonlite)
library(dplyr)



# Executar
#username <- "alive"
#password <- "Alive2025#"

username <- "abule"
password <- "An1s1@1990$"

get_dhis2_events <- function(username, password, page_size = 100) {
  
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
      "totalPages=false",
      "outputType=EVENT",
      "desc=eventdate",
      paste0("pageSize=", page_size),
      paste0("page=", pagina),
      sep = "&"
    )
    
    full_url <- paste0(base_url, "?", dim_string, "&", other_params)
    
    cat("Buscando página:", pagina, "\n")
    
    response <- GET(
      full_url,
      authenticate(username, password, type = "basic"),
      timeout(60)
    )
    
    if (status_code(response) != 200) {
      stop(paste("Erro HTTP:", status_code(response)))
    }
    
    content_json <- content(response, as = "text", encoding = "UTF-8")
    
    # IMPORTANTE: desativar simplificação automática
    data_list <- fromJSON(content_json, simplifyVector = FALSE)
    
    if (length(data_list$rows) == 0) break
    
    # Converter rows para dataframe
    dados_temp <- as.data.frame(do.call(rbind, data_list$rows),
                                stringsAsFactors = FALSE)
    
    # Extrair nomes das colunas corretamente
    headers_names <- sapply(data_list$headers, function(h) h$name)
    colnames(dados_temp) <- headers_names
    
    todos_dados[[pagina]] <- dados_temp
    
    pagina <- pagina + 1
  }
  
  dados_final <- bind_rows(todos_dados)
  
  return(dados_final)
}



dados <- get_dhis2_events(username, password)

names(dados)





#save to data-raw folder csv file with the data frame "dados_processados"
dados_limpo <- dados

dados_limpo[] <- lapply(dados_limpo, function(col) {
  if (is.list(col)) {
    sapply(col, function(x) paste(unlist(x), collapse = ";"))
  } else {
    col
  }
})

cli::cli_alert_success("Salvando dados processados em 'data-raw/dados_processados.csv'")
write.csv(dados_limpo,
          "data-raw/dados_processados.csv",
          row.names = FALSE,
          na = "")



