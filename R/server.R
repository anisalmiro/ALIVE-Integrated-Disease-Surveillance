# ============================================================
# SERVIDOR DO PAINEL
# ============================================================

server <- function(input, output, session) {

  library(readr)     # <--- Garante a função read_csv
  library(dplyr)

  lang <- shiny::reactive({
    if (is.null(input$language_selector) || identical(input$language_selector, "")) "pt" else input$language_selector
  })

  txt <- function(key, fallback = NULL) {
    translations <- list(
      base_not_loaded = c(pt = "Base não carregada", en = "Dataset not loaded"),
      use_csv = c(pt = "Use ‘Carregar novo CSV’ na barra lateral.", en = "Use ‘Upload new CSV’ in the sidebar."),
      records = c(pt = "Registos do Alive Form", en = "Alive Form records"),
      updated_at = c(pt = "Atualizado em", en = "Updated at"),
      event_period = c(pt = "Período do evento", en = "Event period"),
      province = c(pt = "Província", en = "Province"),
      consultation_type = c(pt = "Tipo de Consulta", en = "Consultation type"),
      district = c(pt = "Distrito", en = "District"),
      facility = c(pt = "Unidade sanitária", en = "Health facility"),
      disease = c(pt = "Doença/diagnóstico", en = "Disease/diagnosis"),
      age_group = c(pt = "Faixa etária", en = "Age group"),
      sex = c(pt = "Sexo", en = "Sex"),
      periodicity = c(pt = "Periodicidade", en = "Periodicity"),
      clear_filters = c(pt = "Limpar filtros", en = "Clear filters"),
      no_data = c(pt = "Sem dados para os filtros seleccionados", en = "No data for the selected filters"),
      app_title = c(pt = "ALIVE — Vigilância Integrada de Doenças", en = "ALIVE — Integrated Disease Surveillance"),
      app_subtitle = c(pt = "Qualidade dos dados, sinais de surtos, acesso e continuidade dos serviços, vulnerabilidade e variação de desempenho das unidades de saúde.", en = "Data quality, outbreak signals, service access and continuity, vulnerability, and facility performance variation."),
      global_filters = c(pt = "Filtros globais", en = "Global filters"),
      overview = c(pt = "Visão geral", en = "Overview"),
      alerts_analysis = c(pt = "Alertas e análise", en = "Alerts & analysis"),
      diseases = c(pt = "Doenças", en = "Diseases"),
      quality = c(pt = "Qualidade & dados", en = "Quality & data"),
      completeness = c(pt = "Completude", en = "Completeness"),
      entry = c(pt = "Registo", en = "Data entry"),
      data_method = c(pt = "Dados e método", en = "Data & method"),
      to = c(pt = "até", en = "to"),
      period_unit_day = c(pt = "Diária", en = "Daily"),
      period_unit_week = c(pt = "Semanal", en = "Weekly"),
      period_unit_month = c(pt = "Mensal", en = "Monthly")
    )

    val <- translations[[key]]
    if (is.null(val)) return(fallback %||% key)
    if (identical(lang(), "en")) val[["en"]] else val[["pt"]]
  }

  output$main_sidebar_menu <- shinydashboard::renderMenu({
    shinydashboard::sidebarMenu(
      id = "main_tab",
      shinydashboard::menuItem(txt("overview"), tabName = "overview", icon = shiny::icon("gauge-high")),
      shinydashboard::menuItem(txt("alerts_analysis"), tabName = "analysis", icon = shiny::icon("triangle-exclamation")),
      shinydashboard::menuItem(txt("diseases"), tabName = "diseases", icon = shiny::icon("virus")),
      shinydashboard::menuItem(
        txt("quality"),
        icon = shiny::icon("chart-line"),
        startExpanded = FALSE,
        shinydashboard::menuItem(txt("completeness"), tabName = "completeness", icon = shiny::icon("list-check")),
        shinydashboard::menuItem(txt("entry"), tabName = "entry", icon = shiny::icon("keyboard"))
      ),
      shinydashboard::menuItem(txt("data_method"), tabName = "data", icon = shiny::icon("database"))
    )
  })

  output$global_filters_title <- shiny::renderUI({
    shiny::tags$div(class = "sidebar-section-title", txt("global_filters"))
  })

  output$reset_filters_button <- shiny::renderUI({
    shiny::actionButton("reset_filters", txt("clear_filters"), icon = shiny::icon("rotate-left"))
  })

  output$app_header_texts <- shiny::renderUI({
    shiny::tagList(
      shiny::h2(class = "dashboard-title", txt("app_title")),
      shiny::p(class = "dashboard-subtitle", txt("app_subtitle"))
    )
  })

  shiny::observe({
    if (!is.null(input$language_selector)) {
      choice_values <- c("day", "week", "month")
      choice_names <- c(txt("period_unit_day"), txt("period_unit_week"), txt("period_unit_month"))
      shiny::updateSelectInput(
        session,
        "period_unit",
        label = txt("periodicity"),
        choices = stats::setNames(choice_values, choice_names)
      )
    }
  })

  # Centraliza a leitura dos ficheiros CSV para reduzir dependência de caminhos
  # fixos espalhados pela aplicação e facilitar futuras correções de manutenção.
  dados <- shiny::reactive({
    read_agregados_data(AGREGADOS_DATA_PATH)
  })
  


  
  
  
  # ---------- Utilitários visuais ----------
  make_kpi <- function(title, value, note, icon, accent = "#0b5d4b") {
    shiny::tags$div(
      class = "kpi-card", style = paste0("--accent:", accent, ";"),
      shiny::tags$div(
        shiny::tags$div(class = "kpi-title", title),
        shiny::tags$div(class = "kpi-value", value),
        shiny::tags$div(class = "kpi-note", note)
      ),
      shiny::tags$div(class = "kpi-icon", shiny::icon(icon))
    )
  }

  no_plot <- function(message = "Sem dados para os filtros seleccionados") {
    plotly::plot_ly() |>
      plotly::layout(
        xaxis = list(visible = FALSE), yaxis = list(visible = FALSE),
        annotations = list(list(
          text = message, x = 0.5, y = 0.5, xref = "paper", yref = "paper",
          showarrow = FALSE, font = list(color = "#64748b", size = 14)
        )),
        margin = list(l = 20, r = 20, t = 20, b = 20)
      ) |>
      plotly::config(displayModeBar = FALSE)
  }

  monitor_theme <- function(base_size = 11) {
    ggplot2::theme_minimal(base_size = base_size) +
      ggplot2::theme(
        panel.grid.minor = ggplot2::element_blank(),
        panel.grid.major.x = ggplot2::element_line(colour = "#e7eef0"),
        panel.grid.major.y = ggplot2::element_blank(),
        plot.title = ggplot2::element_text(colour = "#17324d", face = "bold"),
        axis.title = ggplot2::element_text(colour = "#475569"),
        axis.text = ggplot2::element_text(colour = "#475569"),
        legend.position = "bottom",
        legend.title = ggplot2::element_blank()
      )
  }

  to_plotly <- function(p, tooltip = "text") {
    plotly::ggplotly(p, tooltip = tooltip) |>
      plotly::config(displayModeBar = FALSE, responsive = TRUE)
  }

  datatable_pt <- function(data, page_length = 10, scroll_x = TRUE, buttons = FALSE) {
    extensions <- if (buttons) "Buttons" else character(0)
    dom <- if (buttons) "Bfrtip" else "tip"
    button_options <- if (buttons) c("copy", "csv", "excel") else NULL

    DT::datatable(
      data,
      rownames = FALSE,
      filter = "top",
      escape = TRUE,
      extensions = extensions,
      options = list(
        pageLength = page_length,
        scrollX = scroll_x,
        autoWidth = TRUE,
        dom = dom,
        buttons = button_options,
        order = list(),
        language = list(
          search = "Pesquisar:", lengthMenu = "Mostrar _MENU_ registos",
          info = "_START_–_END_ de _TOTAL_", infoEmpty = "Sem registos",
          zeroRecords = "Nenhum registo encontrado",
          paginate = list(first = "Primeira", previous = "Anterior", `next` = "Seguinte", last = "Última")
        )
      )
    )
  }

  # ---------- Atualização automática de dados (a cada 1 hora) ----------
  # Timer que dispara a cada 3600000 milissegundos (1 hora)
  #auto_update_timer <- shiny::reactiveTimer(3600000)
  

  # ---------- Leitura e preparação ----------
  loaded <- shiny::reactive({
    path <- if (!is.null(input$data_file) && has_value(input$data_file$datapath)) {
      input$data_file$datapath
    } else {
      DATA_PATH
    }
    label <- if (!is.null(input$data_file) && has_value(input$data_file$name)) {
      input$data_file$name
    } else {
      basename(DATA_PATH)
    }

    tryCatch(
      {
        list(data = read_monitor_data(path), path = path, label = label, error = NULL)
      },
      error = function(e) {
        list(data = NULL, path = path, label = label, error = conditionMessage(e))
      }
    )
  })

  base_data <- shiny::reactive({
    source <- loaded()
    shiny::validate(shiny::need(is.null(source$error), source$error))
    map_to_canonical(source$data)
  })

  monitor_data <- shiny::reactive({
    evaluate_quality(base_data(), late_days = input$late_days %||% 2)
  })

  output$data_banner <- shiny::renderUI({
    source <- loaded()
    if (!is.null(source$error)) {
      return(shiny::tags$div(
        class = "data-banner", style = "background:#fff0f0;border-color:#f1c2c2;color:#8f2525;",
        shiny::tags$div(shiny::icon("circle-exclamation"), shiny::strong(paste0(" ", txt("base_not_loaded"), ": ")), source$error),
        shiny::tags$span(class = "small-muted", txt("use_csv"))
      ))
    }

    x <- base_data()
    valid_dates <- x$event_date[!is.na(x$event_date)]
    range_text <- if (length(valid_dates)) {
      paste(format(min(valid_dates), "%d/%m/%Y"), txt("to"), format(max(valid_dates), "%d/%m/%Y"))
    } else {
      "datas não reconhecidas"
    }
    shiny::tags$div(
      class = "data-banner",
      shiny::tags$div(
        shiny::icon("database"), shiny::strong(paste0(txt("records"), ": ")),
        paste0(" · ", fmt_number(nrow(x)), " registos · ", range_text)
      ),
      shiny::tags$span(class = "small-muted", paste0(txt("updated_at"), ": ", format(Sys.time(), "%d/%m/%Y %H:%M")))
    )
  })

  # ---------- Filtros dinâmicos ----------
  output$date_filter <- shiny::renderUI({
    x <- base_data()
    valid_dates <- x$event_date[!is.na(x$event_date)]
    shiny::req(length(valid_dates) > 0)
    min_date <- min(valid_dates)
    max_date <- max(valid_dates)
    shiny::dateRangeInput(
      "date_range", txt("event_period"),
      start = min_date, end = max_date,
      min = min_date, max = max(Sys.Date(), max_date),
      format = "dd/mm/yyyy", separator = paste0(" ", txt("to"), " "), language = if (identical(lang(), "en")) "en" else "pt"
    )
  })

  output$province_filter <- shiny::renderUI({
    choices <- sort(unique(base_data()$province[has_value(base_data()$province)]))
    shiny::selectizeInput("province", txt("province"), choices = choices, multiple = TRUE)
  })
  
  output$consultation_type_filter <- shiny::renderUI({
    choices <- sort(unique(base_data()$consultation_type[has_value(base_data()$consultation_type)]))
    shiny::selectizeInput("consultation_type", txt("consultation_type"), choices = choices, multiple = TRUE)
  })

  output$district_filter <- shiny::renderUI({
    x <- base_data()
    if (!is.null(input$province) && length(input$province)) x <- x[x$province %in% input$province, ]
    choices <- sort(unique(x$district[has_value(x$district)]))
    shiny::selectizeInput("district", txt("district"), choices = choices, multiple = TRUE)
  })

  output$facility_filter <- shiny::renderUI({
    x <- base_data()
    if (!is.null(input$province) && length(input$province)) x <- x[x$province %in% input$province, ]
    if (!is.null(input$district) && length(input$district)) x <- x[x$district %in% input$district, ]
    choices <- sort(unique(x$facility[has_value(x$facility)]))
    shiny::selectizeInput("facility", txt("facility"), choices = choices, multiple = TRUE)
  })

  output$disease_filter <- shiny::renderUI({
    choices <- sort(unique(base_data()$diagnosis_final[has_value(base_data()$diagnosis_final)]))
    shiny::selectizeInput("disease", txt("disease"), choices = choices, multiple = TRUE)
  })

  output$age_filter <- shiny::renderUI({
    choices <- VALID_AGE_GROUPS[VALID_AGE_GROUPS %in% base_data()$age_group]
    shiny::selectizeInput("age_group", txt("age_group"), choices = choices, multiple = TRUE)
  })

  output$sex_filter <- shiny::renderUI({
    choices <- VALID_SEX[VALID_SEX %in% base_data()$sex]
    shiny::selectizeInput("sex", txt("sex"), choices = choices, multiple = TRUE)
  })

  shiny::observeEvent(input$reset_filters, {
    x <- base_data()
    valid_dates <- x$event_date[!is.na(x$event_date)]
    if (length(valid_dates)) {
      shiny::updateDateRangeInput(
        session, "date_range", start = min(valid_dates), end = max(valid_dates)
      )
    }
    
    # Adicionado "consultation_type" à lista de selectize inputs a reiniciar
    filter_ids <- c("province", "district", "facility", "disease", "age_group", "sex", "consultation_type")
    invisible(lapply(filter_ids, function(id) {
      shiny::updateSelectizeInput(session, id, selected = character(0))
    }))
    
    shiny::updateSelectInput(session, "period_unit", selected = "week")
    shiny::updateNumericInput(session, "minimum_events", value = 1)
    shiny::updateNumericInput(session, "late_days", value = 2)
  })
  
  filtered_data <- shiny::reactive({
    x <- monitor_data()
    shiny::req(input$date_range)
    start_date <- as.Date(input$date_range[1])
    end_date <- as.Date(input$date_range[2])
    x <- x[!is.na(x$event_date) & x$event_date >= start_date & x$event_date <= end_date, ]
    
    filter_if_selected <- function(data, field, selected) {
      if (!is.null(selected) && length(selected)) data[data[[field]] %in% selected, ] else data
    }
    x <- filter_if_selected(x, "province", input$province)
    x <- filter_if_selected(x, "district", input$district)
    x <- filter_if_selected(x, "facility", input$facility)
    x <- filter_if_selected(x, "diagnosis_final", input$disease)
    x <- filter_if_selected(x, "age_group", input$age_group)
    x <- filter_if_selected(x, "sex", input$sex)
    
    # Aplicação do filtro de tipo de consulta
    x <- filter_if_selected(x, "consultation_type", input$consultation_type)
    
    x
  })

  # Universo do módulo ALIVE: respeita período, geografia, idade e sexo dos
  # filtros globais, mas usa selectores próprios para US e diagnósticos.
  analysis_facility_scope <- shiny::reactive({
    x <- monitor_data()
    if (!is.null(input$province) && length(input$province)) x <- x[x$province %in% input$province, ]
    if (!is.null(input$district) && length(input$district)) x <- x[x$district %in% input$district, ]
    x |>
      dplyr::distinct(province, district, facility, facility_code) |>
      dplyr::filter(has_value(facility))
  })

  analysis_universe_data <- shiny::reactive({
    x <- monitor_data()
    shiny::req(input$date_range)
    start_date <- as.Date(input$date_range[1])
    end_date <- as.Date(input$date_range[2])
    x <- x[!is.na(x$event_date) & x$event_date >= start_date & x$event_date <= end_date, ]
    filter_if_selected <- function(data, field, selected) {
      if (!is.null(selected) && length(selected)) data[data[[field]] %in% selected, ] else data
    }
    x <- filter_if_selected(x, "province", input$province)
    x <- filter_if_selected(x, "district", input$district)
    x <- filter_if_selected(x, "age_group", input$age_group)
    x <- filter_if_selected(x, "sex", input$sex)
    x
  })

  analysis_selectors_ready <- shiny::reactiveVal(FALSE)
  shiny::observeEvent(
    list(analysis_universe_data(), analysis_facility_scope()),
    {
    x <- analysis_universe_data()
    facilities <- sort(unique(analysis_facility_scope()$facility))
    disease_counts <- x |>
      dplyr::filter(has_value(diagnosis_final)) |>
      dplyr::count(diagnosis_final, sort = TRUE)
    diseases <- sort(disease_counts$diagnosis_final)
    priority <- disease_counts |>
      dplyr::filter(stringr::str_detect(normalise_text(diagnosis_final), "sarampo|colera|malaria")) |>
      dplyr::pull(diagnosis_final)
    default_diseases <- unique(c(priority, utils::head(disease_counts$diagnosis_final, 5)))

    if (!analysis_selectors_ready()) {
      selected_facilities <- facilities
      selected_diseases <- default_diseases
      analysis_selectors_ready(TRUE)
    } else {
      # O handler de observeEvent é isolado: alterar os próprios selectores não
      # volta a executar este bloco e evita o ciclo visual de actualizações.
      selected_facilities <- intersect(
        shiny::isolate(input$analysis_facilities) %||% character(), facilities
      )
      selected_diseases <- intersect(
        shiny::isolate(input$analysis_diseases) %||% character(), diseases
      )
      if (!length(selected_facilities)) selected_facilities <- facilities
      if (!length(selected_diseases)) selected_diseases <- default_diseases
    }
    shiny::freezeReactiveValue(input, "analysis_facilities")
    shiny::freezeReactiveValue(input, "analysis_diseases")
    shiny::updateSelectizeInput(
      session, "analysis_facilities", choices = facilities,
      selected = selected_facilities, server = TRUE
    )
    shiny::updateSelectizeInput(
      session, "analysis_diseases", choices = diseases,
      selected = selected_diseases, server = TRUE
    )
    },
    ignoreInit = FALSE
  )

  output$alert_parameter_ui <- shiny::renderUI({
    method <- input$alert_method %||% "mean_sd"
    switch(
      method,
      mean_sd = shiny::numericInput("alert_parameter", "Número de desvios-padrão", value = 2, min = .5, max = 6, step = .5),
      median_mad = shiny::numericInput("alert_parameter", "Multiplicador do MAD", value = 3, min = 1, max = 10, step = .5),
      percentile = shiny::sliderInput("alert_parameter", "Percentil histórico", min = 80, max = 100, value = 95, step = 1, post = "%"),
      growth = shiny::numericInput("alert_parameter", "Crescimento mínimo (%)", value = 50, min = 1, max = 1000, step = 5)
    )
  })

  analysis_facilities_value <- shiny::reactive({
    selected <- input$analysis_facilities
    if (!is.null(selected) && length(selected)) return(selected)
    sort(unique(analysis_facility_scope()$facility))
  })

  denominator_data <- shiny::reactive({
    if (is.null(input$denominator_file) || !has_value(input$denominator_file$datapath)) {
      return(tibble::tibble())
    }
    tryCatch(
      read_denominators(input$denominator_file$datapath),
      error = function(e) {
        shiny::showNotification(
          paste("Não foi possível ler os denominadores:", conditionMessage(e)), type = "error"
        )
        tibble::tibble()
      }
    )
  })

  analysis_service_data <- shiny::reactive({
    analysis_scope_data(
      analysis_universe_data(),
      facilities = analysis_facilities_value(),
      combine_facilities = FALSE,
      exclude_duplicates = isTRUE(input$exclude_analysis_duplicates)
    )
  })

  analysis_alert_data <- shiny::reactive({
    parameter <- input$alert_parameter %||% 2
    range <- current_range()
    alert_method <- input$alert_method %||% "mean_sd"

    standard_deviations <- if (alert_method == "mean_sd") parameter else 2
    mad_multiplier <- if (alert_method == "median_mad") parameter else 3
    percentile <- if (alert_method == "percentile") parameter / 100 else .95
    growth_percent <- if (alert_method == "growth") parameter else 50

    outbreak_alert_series(
      analysis_universe_data(), range[1], range[2],
      unit = input$analysis_period_unit %||% "week",
      facilities = analysis_facilities_value(),
      diseases = input$analysis_diseases,
      combine_facilities = isTRUE(input$combine_facilities),
      group_name = input$analysis_group_name %||% "Grupo ALIVE seleccionado",
      exclude_duplicates = isTRUE(input$exclude_analysis_duplicates),
      method = alert_method,
      baseline_periods = input$baseline_periods %||% 8,
      minimum_history = min(input$minimum_history %||% 4, input$baseline_periods %||% 8),
      minimum_cases = input$minimum_cases_alert %||% 2,
      standard_deviations = standard_deviations,
      mad_multiplier = mad_multiplier,
      percentile = percentile,
      growth_percent = growth_percent
    )
  })

  measles_alert_data <- shiny::reactive({
    who_measles_alerts(
      analysis_universe_data(), facilities = analysis_facilities_value(),
      combine_facilities = isTRUE(input$combine_facilities),
      group_name = input$analysis_group_name %||% "Grupo ALIVE seleccionado",
      exclude_duplicates = isTRUE(input$exclude_analysis_duplicates)
    )
  })

  cholera_alert_data <- shiny::reactive({
    who_cholera_alerts(
      analysis_universe_data(), facilities = analysis_facilities_value(),
      combine_facilities = isTRUE(input$combine_facilities),
      group_name = input$analysis_group_name %||% "Grupo ALIVE seleccionado",
      exclude_duplicates = isTRUE(input$exclude_analysis_duplicates)
    )
  })

  service_access_data <- shiny::reactive({
    range <- current_range()
    selected_universe <- analysis_facility_scope() |>
      dplyr::filter(facility %in% analysis_facilities_value())
    service_access_summary(
      analysis_service_data(), input$continuity_days %||% 60, denominator_data(),
      start_date = range[1], end_date = range[2],
      unit = input$analysis_period_unit %||% "week",
      facility_universe = selected_universe
    )
  })

  vulnerability_data <- shiny::reactive({
    vulnerability_summary(analysis_service_data(), input$continuity_days %||% 60)
  })

  facility_variation_data <- shiny::reactive({
    facility_variation(analysis_service_data(), input$analysis_diseases, denominator_data())
  })

  # Universo geográfico esperado. Mantém no painel as unidades conhecidas que
  # não introduziram qualquer evento no período ou doença seleccionados.
  facility_scope <- shiny::reactive({
    x <- monitor_data()
    if (!is.null(input$province) && length(input$province)) x <- x[x$province %in% input$province, ]
    if (!is.null(input$district) && length(input$district)) x <- x[x$district %in% input$district, ]
    if (!is.null(input$facility) && length(input$facility)) x <- x[x$facility %in% input$facility, ]
    x |>
      dplyr::distinct(province, district, facility, facility_code) |>
      dplyr::filter(has_value(facility))
  })

  current_range <- shiny::reactive({
    shiny::req(input$date_range)
    as.Date(input$date_range)
  })

  facility_stats <- shiny::reactive({
    range <- current_range()
    facility_monitoring(
      filtered_data(), range[1], range[2], input$period_unit %||% "week",
      input$minimum_events %||% 1, input$late_days %||% 2,
      facility_universe = facility_scope()
    )
  })

  outlier_data <- shiny::reactive({
    range <- current_range()
    period_outliers(
      filtered_data(), range[1], range[2], input$period_unit %||% "week",
      facilities = facility_scope()$facility
    )
  })

  metrics <- shiny::reactive({
    x <- filtered_data()
    facilities <- facility_stats()
    silent_facilities_count <- if (nrow(facilities) > 0L) {
      sum(facilities$silent_periods > 0, na.rm = TRUE)
    } else {
      0L
    }

    list(
      records = nrow(x),
      facilities = dplyr::n_distinct(x$facility[has_value(x$facility)]),
      diseases = dplyr::n_distinct(x$diagnosis_final[has_value(x$diagnosis_final)]),
      quality = overall_quality_score(x, input$late_days %||% 2),
      completeness = safe_mean(x$record_completeness),
      timeliness = safe_pct(!is.na(x$delay_days) & x$delay_days <= (input$late_days %||% 2)),
      duplicate_rows = sum(x$duplicate_id, na.rm = TRUE),
      alert_rows = sum(x$issue_count > 0, na.rm = TRUE),
      high_alerts = sum(x$severity == "Alta", na.rm = TRUE),
      medium_alerts = sum(x$severity == "Média", na.rm = TRUE),
      silent_facilities = silent_facilities_count,
      anomalies = sum(outlier_data()$anomaly, na.rm = TRUE)
    )
  })

  # ---------- KPIs ----------
  output$kpi_records <- shiny::renderUI({
    m <- metrics(); make_kpi("Registos", fmt_number(m$records), "", "file-lines", "#1769aa")
  })
  output$kpi_facilities <- shiny::renderUI({
    m <- metrics(); make_kpi("Unidades sanitárias", fmt_number(m$facilities), "", "hospital", "#0f766e")
  })
  
  # Altere a definição da função para incluir color (com um valor padrão, ex: "#0277bd")
  make_kpi <- function(title, value, note = NULL, icon = NULL, color = "#0277bd", ...) {
    icon_tag <- if (!is.null(icon)) shiny::icon(icon) else NULL
    note_tag <- if (!is.null(note)) shiny::div(class = "kpi-note", note) else NULL

    shiny::div(
      class = "kpi-card",
      style = paste0("border-left: 4px solid ", color, ";"),
      shiny::div(
        class = "kpi-icon",
        style = paste0("color: ", color, ";"),
        icon_tag
      ),
      shiny::div(class = "kpi-title", title),
      shiny::div(class = "kpi-value", value),
      note_tag
    )
  }
  
  # ============================================================
  # KPI: Primeiras Consultas
  # ============================================================
  output$kpi_diseases <- shiny::renderUI({
    # Atribuição direta do dataframe
    m <- hpt_data
    
    # Contagem dos registos de primeira consulta na coluna consultation_type
    first_consult_count <- if (!is.null(m) && "consultation_type" %in% names(m)) {
      # Converte para caracter para garantir leitura correta de fatores ou textos
      consult_vec <- as.character(m$consultation_type)
      
      # Soma as ocorrências dos termos "primeira", "1ª", "1a", "inicial" ou "novo"
      sum(grepl("primeira|1ª|1a|inicial|novo", consult_vec, ignore.case = TRUE), na.rm = TRUE)
    } else {
      0
    }
    
    # Construção do KPI
    make_kpi(
      title = "Primeiras Consultas", 
      value = fmt_number(first_consult_count), 
      note = "1ª consulta", 
      icon = "user-plus"
    )
  })
  
  # ============================================================
  # KPI: Consultas de Seguimento
  # ============================================================
  output$kpi_quality <- shiny::renderUI({
    # Atribuição direta do dataframe
    m <- hpt_data
    
    # Contagem dos registos de consulta de seguimento/seguinte na coluna consultation_type
    followup_consult_count <- if (!is.null(m) && "consultation_type" %in% names(m)) {
      # Converte para texto para evitar falhas com fatores ou tipos numéricos
      consult_vec <- as.character(m$consultation_type)
      
      # Procura por "seguinte", "seguimento", "continuidade" ou "retorno"
      sum(grepl("seguinte|seguimento|continuidade|reconsulta|retorno", consult_vec, ignore.case = TRUE), na.rm = TRUE)
    } else {
      0
    }
    
    # Construção do KPI
    make_kpi(
      title = "Consultas de Seguimento", 
      value = fmt_number(followup_consult_count), 
      note = "", 
      icon = "user-check"
    )
  })



  output$kpi_high_alerts <- shiny::renderUI({
    m <- metrics(); make_kpi("Prioridade alta", fmt_number(m$high_alerts), "Registos para revisão imediata", "circle-exclamation", "#c93c3c")
  })
  output$kpi_medium_alerts <- shiny::renderUI({
    m <- metrics(); make_kpi("Prioridade média", fmt_number(m$medium_alerts), "Inconsistências e duplicações", "triangle-exclamation", "#d99a00")
  })
  output$kpi_silent_facilities <- shiny::renderUI({
    m <- metrics(); make_kpi("US com silêncio", fmt_number(m$silent_facilities), "Pelo menos um período abaixo do mínimo", "volume-xmark", "#7c3aed")
  })
  output$kpi_anomalies <- shiny::renderUI({
    m <- metrics(); make_kpi("Anomalias de volume", fmt_number(m$anomalies), "Método robusto mediana/MAD", "chart-line", "#1769aa")
  })

  # ---------- Visão executiva ----------
  output$automatic_summary <- shiny::renderUI({
    x <- filtered_data()
    if (nrow(x) == 0) return(shiny::p("Não há registos para os filtros seleccionados."))

    facilities <- facility_stats()
    fields <- field_completeness(x)
    diseases <- disease_summary(x)
    worst_facility <- if (nrow(facilities)) facilities$facility[which.min(facilities$quality_score)] else "—"
    worst_score <- if (nrow(facilities)) min(facilities$quality_score, na.rm = TRUE) else NA_real_
    weakest_field <- if (nrow(fields)) fields$label[1] else "—"
    weakest_value <- if (nrow(fields)) fields$completeness[1] else NA_real_
    top_disease <- if (nrow(diseases)) diseases$diagnosis_final[1] else "—"
    top_count <- if (nrow(diseases)) diseases$notifications[1] else 0
    m <- metrics()

    shiny::tags$div(
      class = "automatic-summary",
      shiny::p(
        "Foram analisados ", shiny::strong(fmt_number(nrow(x))), " registos de ",
        shiny::strong(fmt_number(m$facilities)), " unidades sanitárias"
      ),
      shiny::p(
        "O diagnóstico mais registado é ", shiny::strong(top_disease), " com ",
        shiny::strong(fmt_number(top_count)), " notificações."
      )
    )
  })

  output$overall_trend <- plotly::renderPlotly({
    x <- filtered_data()
    range <- current_range()
    
    if (nrow(x) == 0) return(no_plot())
    
    unit <- input$period_unit %||% "week"
    periods <- tibble::tibble(period_start = period_sequence(range[1], range[2], unit))
    
    trend <- x |>
      dplyr::mutate(period_start = period_start_date(event_date, unit)) |>
      dplyr::count(period_start, name = "records") |>
      dplyr::right_join(periods, by = "period_start") |>
      dplyr::mutate(
        records = tidyr::replace_na(records, 0L),
        text = paste0(period_label(period_start, unit), "<br>Registos: ", fmt_number(records))
      ) |>
      dplyr::arrange(period_start)
    
    # Construção do gráfico de linhas
    p <- ggplot2::ggplot(trend, ggplot2::aes(x = period_start, y = records, group = 1, text = text)) +
      ggplot2::geom_line(colour = "#0b5d4b", linewidth = 1) +
      ggplot2::geom_point(colour = "#0b5d4b", size = 2.5, fill = "#7bc8b3", shape = 21, stroke = 1) +
      ggplot2::labs(x = NULL, y = "Registos") + 
      monitor_theme()
    
    to_plotly(p)
  })


  output$disease_pareto <- plotly::renderPlotly({
    
    
    # 1. Filtra dados válidos
    df <- filtered_data() |> 
      dplyr::filter(!is.na(facility), !is.na(diagnosis_final))
    
    if (nrow(df) == 0) return(no_plot("Não há dados de diagnósticos ou unidades sanitárias"))
    
    # 2. Agrupa por US e Diagnóstico, selecionando as principais Unidades Sanitárias
    top_facilities <- df |> 
      dplyr::count(facility, sort = TRUE) |> 
      dplyr::slice_head(n = 10) |> 
      dplyr::pull(facility)
    
    x <- df |> 
      dplyr::filter(facility %in% top_facilities) |> 
      dplyr::group_by(facility, diagnosis_final) |> 
      dplyr::summarise(notificacoes = dplyr::n(), .groups = "drop") |> 
      dplyr::mutate(
        text = paste0("Unidade Sanitária: ", facility,
                      "<br>Diagnóstico: ", diagnosis_final,
                      "<br>Notificações: ", fmt_number(notificacoes))
      )
    
    # 3. Gera o gráfico de barras empilhadas
    p <- ggplot2::ggplot(x, ggplot2::aes(
      x = stats::reorder(facility, notificacoes, sum), 
      y = notificacoes, 
      fill = diagnosis_final,
      text = text
    )) +
      ggplot2::geom_col(width = 0.7) +
      ggplot2::coord_flip() +
      ggplot2::labs(
        x = "Unidade Sanitária",
        y = "Notificações",
        fill = "Diagnóstico"
      ) + 
      monitor_theme()
    
    to_plotly(p, tooltip = "text")
  })

  # ---------- Introdução de dados ----------
  output$entry_heatmap <- plotly::renderPlotly({
    range <- current_range()
    grid <- facility_period_grid(
      filtered_data(), range[1], range[2], input$period_unit %||% "week",
      facilities = facility_scope()$facility
    )
    if (nrow(grid) == 0) return(no_plot())
    grid <- grid |>
      dplyr::mutate(
        facility = factor(facility, levels = rev(sort(unique(facility)))),
        text = paste0(facility, "<br>", period_label, "<br>Registos: ", events)
      )
    p <- ggplot2::ggplot(grid, ggplot2::aes(period_start, facility, fill = events, text = text)) +
      ggplot2::geom_tile(colour = "white", linewidth = .25) +
      ggplot2::scale_fill_gradient(low = "#f1f5f5", high = "#0b5d4b", trans = "sqrt") +
      ggplot2::labs(x = NULL, y = NULL, fill = "Registos") + monitor_theme(10)
    to_plotly(p)
  })
  
  
  
  #Map cases
  output$disease_map <- leaflet::renderLeaflet({
    sara_geo_data <- read_sara_geo_data(SARA_GEO_DATA_PATH)
    if (nrow(sara_geo_data) == 0) {
      return(NULL)
    }

    df_raw <- filtered_data()

    if (is.null(df_raw) || nrow(df_raw) == 0) {
      return(NULL)
    }

    disease_col <- intersect(c("diagnosis_final", "disease", "doenca", "diagnostico", "agravo", "patologia", "disease_name"), names(df_raw))[1]
    facility_col <- if ("facility" %in% names(df_raw)) "facility" else "facility_name"
    facility_code_col <- if ("facility_code" %in% names(df_raw)) "facility_code" else NULL

    df_raw <- df_raw %>%
      dplyr::mutate(
        disease = if (!is.null(disease_col) && nzchar(disease_col)) as.character(.data[[disease_col]]) else "Não Especificado",
        facility = if (facility_col %in% names(df_raw)) as.character(.data[[facility_col]]) else "N/A",
        facility_code = if (!is.null(facility_code_col) && facility_code_col %in% names(df_raw)) as.character(.data[[facility_code_col]]) else NA_character_
      ) %>%
      dplyr::mutate(
        disease = dplyr::coalesce(disease, "Não Especificado"),
        facility_code = trimws(facility_code)
      ) %>%
      dplyr::filter(!is.na(facility_code) & nzchar(facility_code))

    if (nrow(df_raw) == 0) {
      return(NULL)
    }

    cases_by_facility_disease <- df_raw %>%
      dplyr::group_by(facility_code, disease) %>%
      dplyr::summarise(
        total_cases = dplyr::n(),
        facility_name = dplyr::first(facility),
        .groups = "drop"
      )

    map_data <- cases_by_facility_disease %>%
      dplyr::mutate(facility_code = as.character(facility_code)) %>%
      dplyr::left_join(
        sara_geo_data %>% dplyr::mutate(facility_code = as.character(SIS_CODE)),
        by = "facility_code"
      ) %>%
      dplyr::filter(!is.na(latitude) & !is.na(longitude))

    if (nrow(map_data) == 0) {
      return(NULL)
    }

    unique_diseases <- unique(map_data$disease)
    pal_disease <- leaflet::colorFactor(
      palette = c("#4E79A7", "#F28E2B", "#E15759", "#76B7B2", "#59A14F", "#EDC948", "#B07AA1", "#FF9DA7"),
      domain = unique_diseases
    )

    map_data <- map_data %>%
      dplyr::mutate(
        popup_html = paste0(
          "<div style='font-family: sans-serif; font-size: 13px;'>",
          "<b>Unidade Sanitária: </b>", dplyr::coalesce(facility_name, "N/A"), "<br>",
          "<b>Código US: </b>", facility_code, "<br>",
          "<b>Doença / Agravo: </b><span style='color:", pal_disease(disease), ";font-weight:bold;'>", disease, "</span><br>",
          "<b>Total de Casos: </b>", format(total_cases, big.mark = " "),
          "</div>"
        )
      )

    leaflet::leaflet(map_data) %>%
      leaflet::addProviderTiles(leaflet::providers$OpenStreetMap, group = "OpenStreetMap") %>%
      leaflet::addProviderTiles(leaflet::providers$CartoDB.DarkMatter, group = "Dark (CartoDB)") %>%
      leaflet::addProviderTiles(leaflet::providers$Esri.WorldImagery, group = "Satélite (Esri)") %>%
      leaflet::addCircleMarkers(
        lng = ~longitude,
        lat = ~latitude,
        radius = ~scales::rescale(total_cases, to = c(6, 22)),
        color = ~pal_disease(disease),
        fillColor = ~pal_disease(disease),
        fillOpacity = 0.75,
        stroke = TRUE,
        weight = 1.5,
        popup = ~popup_html
      ) %>%
      leaflet::addLegend(
        pal = pal_disease,
        values = unique_diseases,
        title = "Doença / Agravo",
        position = "bottomright",
        opacity = 0.9
      ) %>%
      leaflet::addLayersControl(
        baseGroups = c("OpenStreetMap", "Dark (CartoDB)", "Satélite (Esri)"),
        options = leaflet::layersControlOptions(collapsed = FALSE),
        position = "topright"
      )
  })
  
  
  #fim Mapa


  output$facility_monitor_table <- DT::renderDT({
    x <- facility_stats()
    if (nrow(x) == 0) return(datatable_pt(data.frame(Mensagem = "Sem dados")))
    display <- x |>
      dplyr::transmute(
        Província = province, Distrito = district, `Unidade sanitária` = facility,
        Registos = events, `Períodos esperados` = expected_periods,
        `Períodos reportados` = reported_periods, `Períodos silenciosos` = silent_periods,
        `Cobertura de reporte` = reporting_coverage,
        `Completude dos campos` = field_completeness,
        Pontualidade = timely, `Índice de qualidade` = quality_score,
        `Último evento` = last_event, `Dias sem evento` = days_silent,
        Situação = quality_status
      )
    datatable_pt(display, page_length = 15, buttons = TRUE) |>
      DT::formatRound(c("Cobertura de reporte", "Completude dos campos", "Pontualidade", "Índice de qualidade"), 1) |>
      DT::formatStyle(
        "Índice de qualidade", backgroundColor = DT::styleInterval(
          c(60, 80, 90), c("#f8d7da", "#fff0c2", "#d9f2df", "#c9eee4")
        )
      )
  })

  # ---------- Completude ----------
  output$field_completeness_plot <- plotly::renderPlotly({
    x <- field_completeness(filtered_data())
    if (nrow(x) == 0) return(no_plot())
    x <- x |>
      dplyr::arrange(completeness) |>
      dplyr::mutate(
        label = factor(label, levels = label),
        status = quality_band(completeness),
        text = paste0(label, "<br>Completude: ", fmt_pct(completeness), "<br>Ausentes: ", missing_n)
      )
    label_pct <- function(v) paste0(v, "%")
    p <- ggplot2::ggplot(x, ggplot2::aes(completeness, label, fill = status, text = text)) +
      ggplot2::geom_col(width = .7) +
      ggplot2::geom_vline(xintercept = 90, linetype = "dashed", colour = "#64748b") +
      ggplot2::scale_fill_manual(values = c("Crítico" = "#c93c3c", "Atenção" = "#d99a00", "Bom" = "#38a169", "Excelente" = "#0f766e")) +
      ggplot2::scale_x_continuous(
        limits = c(0, 100),
        labels = label_pct
      ) +
      ggplot2::labs(x = "Completude", y = NULL) + monitor_theme(10)
    to_plotly(p)
  })

  output$completeness_heatmap <- plotly::renderPlotly({
    x <- facility_field_matrix(filtered_data())
    if (nrow(x) == 0) return(no_plot())
    x <- x |>
      dplyr::mutate(
        facility = factor(facility, levels = rev(sort(unique(facility)))),
        label = factor(label, levels = rev(unique(label))),
        text = paste0(facility, "<br>", label, "<br>Completude: ", fmt_pct(completeness))
      )
    p <- ggplot2::ggplot(x, ggplot2::aes(label, facility, fill = completeness, text = text)) +
      ggplot2::geom_tile(colour = "white", linewidth = .3) +
      ggplot2::scale_fill_gradientn(colours = c("#c93c3c", "#f0c94d", "#0f9d72"), limits = c(0, 100)) +
      ggplot2::labs(x = NULL, y = NULL, fill = "%") +
      monitor_theme(9) + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
    to_plotly(p)
  })

  output$field_completeness_table <- DT::renderDT({
    x <- field_completeness(filtered_data()) |>
      dplyr::transmute(
        Campo = label, Preenchidos = complete_n, Ausentes = missing_n,
        `Completude (%)` = round(completeness, 1), Situação = quality_band(completeness)
      )
    datatable_pt(x, page_length = 25, buttons = TRUE) |>
      DT::formatStyle(
        "Completude (%)", backgroundColor = DT::styleInterval(
          c(60, 80, 90), c("#f8d7da", "#fff0c2", "#d9f2df", "#c9eee4")
        )
      )
  })

  # ---------- Qualidade ----------
  output$quality_radar <- plotly::renderPlotly({
    x <- quality_dimensions(filtered_data(), input$late_days %||% 2)
    if (nrow(filtered_data()) == 0) return(no_plot())
    closed <- dplyr::bind_rows(x, x[1, ])
    plotly::plot_ly(
      closed, type = "scatterpolar", r = ~score, theta = ~dimension,
      fill = "toself", mode = "lines+markers",
      line = list(color = "#0b5d4b", width = 3),
      marker = list(color = "#0b5d4b", size = 7),
      fillcolor = "rgba(11,93,75,.20)",
      text = ~paste0(dimension, ": ", fmt_pct(score)), hoverinfo = "text"
    ) |>
      plotly::layout(
        polar = list(radialaxis = list(visible = TRUE, range = c(0, 100), ticksuffix = "%")),
        showlegend = FALSE, margin = list(l = 60, r = 60, t = 35, b = 35)
      ) |>
      plotly::config(displayModeBar = FALSE)
  })

  output$issue_plot <- plotly::renderPlotly({
    x <- issue_summary(filtered_data())
    if (nrow(x) == 0) return(no_plot())
    x <- x |>
      dplyr::arrange(records) |>
      dplyr::mutate(
        issue = factor(issue, levels = issue),
        text = paste0(issue, "<br>Registos: ", records, "<br>Percentagem: ", fmt_pct(percent))
      )
    p <- ggplot2::ggplot(x, ggplot2::aes(records, issue, text = text)) +
      ggplot2::geom_col(fill = "#c05621", width = .7) +
      ggplot2::labs(x = "Registos afectados", y = NULL) + monitor_theme()
    to_plotly(p)
  })

  # ---------- Doenças ----------
  output$disease_trend <- plotly::renderPlotly({
    x <- filtered_data() |> dplyr::filter(has_value(diagnosis_final))
    if (nrow(x) == 0) return(no_plot("Não há diagnósticos preenchidos"))
    unit <- input$period_unit %||% "week"
    top <- x |> dplyr::count(diagnosis_final, sort = TRUE) |> dplyr::slice_head(n = 8) |> dplyr::pull(diagnosis_final)
    trend <- x |>
      dplyr::mutate(
        disease_group = ifelse(diagnosis_final %in% top, diagnosis_final, "Outros"),
        period_start = period_start_date(event_date, unit)
      ) |>
      dplyr::count(period_start, disease_group, name = "notifications") |>
      dplyr::mutate(text = paste0(disease_group, "<br>", period_label(period_start, unit),
                                  "<br>Notificações: ", notifications))
    p <- ggplot2::ggplot(trend, ggplot2::aes(period_start, notifications, colour = disease_group,
                                             group = disease_group, text = text)) +
      ggplot2::geom_line(linewidth = .8) + ggplot2::geom_point(size = 1.3) +
      ggplot2::labs(x = NULL, y = "Notificações", colour = NULL) + monitor_theme(10)
    to_plotly(p)
  })

  output$disease_age_sex <- plotly::renderPlotly({
    x <- filtered_data() |>
      dplyr::filter(has_value(age_group), has_value(sex)) |>
      dplyr::count(age_group, sex, name = "records") |>
      dplyr::mutate(
        age_group = factor(age_group, levels = VALID_AGE_GROUPS),
        text = paste0(age_group, "<br>", sex, "<br>Registos: ", records)
      )
    if (nrow(x) == 0) return(no_plot())
    p <- ggplot2::ggplot(x, ggplot2::aes(age_group, records, fill = sex, text = text)) +
      ggplot2::geom_col(position = "dodge") +
      ggplot2::scale_fill_manual(values = c("Feminino" = "#7c3aed", "Masculino" = "#1769aa")) +
      ggplot2::labs(x = NULL, y = "Registos") + monitor_theme(10) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 35, hjust = 1))
    to_plotly(p)
  })


  output$disease_table <- DT::renderDT({
    x <- disease_summary(filtered_data())
    if (nrow(x) == 0) return(datatable_pt(data.frame(Mensagem = "Não há diagnósticos preenchidos")))
    x <- x |>
      dplyr::transmute(
        Diagnóstico = diagnosis_final, Notificações = notifications,
        `Unidades sanitárias` = facilities, Positivos = positive,
        `Positividade (%)` = round(positivity, 1), Óbitos = deaths,
        `Completude (%)` = round(field_completeness, 1)
      )
    datatable_pt(x, page_length = 15, buttons = TRUE)
  })

  
  #HIpertencao analisys
  
  # 1. TRATAMENTO E FILTRAGEM DOS DADOS DE HIPERTENSÃO
  # ==============================================================================
  # 1. PACOTES NECESSÁRIOS
  # ==============================================================================

  
  # ==============================================================================
  # 2. CARREGAMENTO E RENOMEAMENTO DA BASE DE DADOS ESTÁTICA
  # ==============================================================================
  field_map <- c(
    record_id              = "psi",
    program_stage_id       = "ps",
    event_date             = "eventdate",
    stored_by              = "storedby",
    created_by             = "createdbydisplayname",
    updated_by             = "lastupdatedbydisplayname",
    last_updated           = "lastupdated",
    scheduled_date         = "scheduleddate",
    enrollment_date        = "enrollmentdate",
    incident_date          = "incidentdate",
    tracked_entity_id      = "tei",
    enrollment_id          = "pi",
    geometry               = "geometry",
    longitude              = "longitude",
    latitude               = "latitude",
    facility               = "ouname",
    organisation_hierarchy  = "ounamehierarchy",
    facility_code          = "oucode",
    program_status         = "programstatus",
    event_status           = "eventstatus",
    facility_id            = "ou",
    fever                  = "oNN1FVNaV4r.eMVytXtuMWh",
    requested_test         = "oNN1FVNaV4r.mtkN9C1BJX4",
    first_name             = "ENRjVGxVL6l",
    age_group              = "MZeTF32uOUD",
    diagnosis              = "oNN1FVNaV4r.nRdPCxj7d9z",
    occupation             = "Q1NM5BSIxl0",
    surname                = "sB1IHYu2xQT",
    group_code             = "ANXuo6saopx",
    care_action            = "oNN1FVNaV4r.F4XfAlCCrf2",
    consultation_type      = "oNN1FVNaV4r.kP29shPSHl8",
    community_actor_group  = "VCtm2pySeEV",
    vulnerable_group       = "oNN1FVNaV4r.BsKcBDC0kSq",
    sex                    = "CklPZdOd6H1",
    other_test             = "oNN1FVNaV4r.rLDcMERXNrS",
    received_treatment     = "oNN1FVNaV4r.sh6WSMuKbof",
    test_result            = "oNN1FVNaV4r.exTgdok33ph",
    test_status            = "oNN1FVNaV4r.teO9EjrYeF6",
    outcome                = "oNN1FVNaV4r.HG5ltiCtn1D",
    vulnerable_population  = "oNN1FVNaV4r.vK9PPhCrA6Q",
    community_referral     = "oNN1FVNaV4r.M9GNJpUsAeT",
    other_diagnosis        = "oNN1FVNaV4r.vrJV8duHFUy"
  )
  
  # Ler os dados e aplicar os renomeamentos (any_of ignora colunas inexistentes)
  hpt_data <- read_monitor_data(DATA_PATH) %>%
    rename(any_of(field_map))
  
  # ==============================================================================
  # 3. OBJETOS REATIVOS (DENTRO DO SERVER)
  # ==============================================================================
  
  # Nota: Se preferires utilizar a base global do Shiny com filtros de data/província, 
  # substitui 'hpt_data' por 'filtered_data()' abaixo.
  htn_data <- reactive({
    hpt_data %>%
      filter(
        diagnosis == "Hipertensão" | stringr::str_detect(diagnosis, "(?i)hipertens")
      ) %>%
      mutate(
        data = as.Date(event_date),
        ano = lubridate::isoyear(data),
        semana = lubridate::isoweek(data),
        mes = lubridate::floor_date(data, "month"),
        epi_semana = paste0(ano, "-W", sprintf("%02d", semana)),
        
        eh_obito = ifelse(outcome == "Óbito" | care_action == "Óbito", 1, 0),
        eh_internamento = ifelse(consultation_type == "Internamento" | care_action == "Internamento", 1, 0)
      )
  })
  
  # 3.2 Agrupamento Semanal (Para Limiar de Alerta)
  htn_weekly <- reactive({
    htn_data() %>%
      group_by(epi_semana, ano, semana) %>%
      summarise(
        total_obitos = sum(eh_obito, na.rm = TRUE),
        total_internamentos = sum(eh_internamento, na.rm = TRUE),
        total_eventos = total_obitos + total_internamentos,
        em_alerta = total_eventos >= 1,
        .groups = "drop"
      ) %>%
      arrange(ano, semana)
  })
  
  # ==============================================================================
  # 4. RENDERIZAÇÃO DOS KPIS (VALUE BOXES)
  # ==============================================================================
  
  # KPI 1: Óbitos na Semana Atual
  output$kpi_htn_deaths_week <- shiny::renderUI({
    dados_semana <- htn_weekly() %>% tail(1)
    val <- if(nrow(dados_semana) > 0) dados_semana$total_obitos else 0
    
    # Alerta vermelho (#c0392b) se houver óbitos, caso contrário verde (#27ae60)
    cor_kpi <- if(val >= 1) "#c0392b" else "#27ae60"
    
    make_kpi(
      "Óbitos por Hipertensão",
      fmt_number(val),
      "Semana epidemiológica atual",
      "skull",
      cor_kpi
    )
  })
  
  # KPI 2: Internamentos na Semana Atual
  output$kpi_htn_admissions_week <- shiny::renderUI({
    dados_semana <- htn_weekly() %>% tail(1)
    val <- if(nrow(dados_semana) > 0) dados_semana$total_internamentos else 0
    
    # Alerta laranja/vermelho (#e67e22) se houver internamentos, caso contrário verde (#27ae60)
    cor_kpi <- if(val >= 1) "#e67e22" else "#27ae60"
    
    make_kpi(
      "Internamentos por Hipertensão",
      fmt_number(val),
      "Semana epidemiológica atual",
      "procedures",
      cor_kpi
    )
  })
  
  # KPI 3: Totais por Mês (Óbitos no Mês Atual)
  output$kpi_htn_deaths_month <- shiny::renderUI({
    hoje <- Sys.Date()
    val <- htn_data() %>%
      filter(mes == lubridate::floor_date(hoje, "month")) %>%
      summarise(total = sum(eh_obito, na.rm = TRUE)) %>%
      pull(total)
    
    val <- if(length(val) == 0 || is.na(val)) 0 else val
    
    make_kpi(
      "Óbitos no Mês Atual",
      fmt_number(val),
      "Total acumulado no mês vigente",
      "calendar-alt",
      "#2980b9" # Azul
    )
  })
  
  # KPI 4: Totais nos Últimos 90 Dias (Óbitos)
  output$kpi_htn_deaths_3months <- shiny::renderUI({
    hoje <- Sys.Date()
    limite_3m <- hoje - lubridate::days(90)
    
    val <- htn_data() %>%
      filter(data >= limite_3m) %>%
      summarise(total = sum(eh_obito, na.rm = TRUE)) %>%
      pull(total)
    
    val <- if(length(val) == 0 || is.na(val)) 0 else val
    
    make_kpi(
      "Óbitos (Últimos 90 Dias)",
      fmt_number(val),
      "Total acumulado nos últimos 3 meses",
      "chart-line",
      "#8e44ad" # Roxo
    )
  })
  
  # ==============================================================================
  # 5. GRÁFICO DE ALERTAS COM LIMIAR (≥ 1 Evento/Semana)
  # ==============================================================================
  output$htn_alert_plot <- plotly::renderPlotly({
    df_plot <- htn_weekly()
    
    p <- ggplot(df_plot, aes(x = epi_semana, y = total_eventos, group = 1)) +
      geom_line(color = "#34495e", linewidth = 0.8) +
      geom_point(aes(color = em_alerta, 
                     text = paste0("Epi-Semana: ", epi_semana,
                                   "<br>Óbitos: ", total_obitos,
                                   "<br>Internamentos: ", total_internamentos,
                                   "<br>Total Eventos: ", total_eventos)), 
                 size = 3) +
      geom_hline(yintercept = 1, linetype = "dashed", color = "#e74c3c", linewidth = 1) +
      scale_color_manual(
        values = c("FALSE" = "#2ecc71", "TRUE" = "#e74c3c"),
        labels = c("Normal (0)", "Alerta (≥ 1)")
      ) +
      labs(
        title = "Curva Epidémica de Eventos Graves por Hipertensão",
        x = "Semana Epidemiológica",
        y = "Total (Óbitos + Internamentos)",
        color = "Estado"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    plotly::ggplotly(p, tooltip = "text")
  })
  
  # ==============================================================================
  # 6. TABELAS DE RESUMO E ALERTAS (DT)
  # ==============================================================================
  
  # Tabela de Semanas em Alerta
  output$htn_alert_table <- DT::renderDT({
    htn_weekly() %>%
      filter(em_alerta == TRUE) %>%
      select(
        `Semana Epidemiológica` = epi_semana,
        `Óbitos` = total_obitos,
        `Internamentos` = total_internamentos,
        `Total Eventos` = total_eventos
      ) %>%
      arrange(desc(`Semana Epidemiológica`)) %>%
      DT::datatable(
        options = list(pageLength = 5, dom = 'tip'),
        rownames = FALSE
      )
  })
  
  # Tabela Resumo Mensal
  output$htn_summary_table <- DT::renderDT({
    htn_data() %>%
      group_by(Mês = format(mes, "%Y-%m")) %>%
      summarise(
        `Total Casos` = n(),
        `Internamentos` = sum(eh_internamento, na.rm = TRUE),
        `Óbitos` = sum(eh_obito, na.rm = TRUE),
        `Taxa de Letalidade (%)` = round((sum(eh_obito, na.rm = TRUE) / n()) * 100, 1),
        .groups = "drop"
      ) %>%
      arrange(desc(Mês)) %>%
      DT::datatable(rownames = FALSE)
  })
  
  # fIM DE ANALISE DE HIPERTENCAO
  
  
  # ---------- Análises epidemiológicas e de serviços — ALIVE ----------





  output$outbreak_alert_plot <- plotly::renderPlotly({
    x <- analysis_alert_data()
    if (nrow(x) == 0) return(no_plot("Seleccione US e diagnósticos com dados"))
    top_diseases <- x |>
      dplyr::group_by(diagnosis_final) |>
      dplyr::summarise(total = sum(cases), alerts = sum(alert), .groups = "drop") |>
      dplyr::arrange(dplyr::desc(alerts), dplyr::desc(total)) |>
      dplyr::slice_head(n = 8) |>
      dplyr::pull(diagnosis_final)
    x <- x |>
      dplyr::filter(diagnosis_final %in% top_diseases) |>
      dplyr::mutate(
        text = paste0(
          analysis_unit, "<br>", diagnosis_final, "<br>", period_label,
          "<br>Casos: ", cases,
          "<br>Limiar: ", ifelse(is.na(threshold), "linha de base insuficiente", round(threshold, 1)),
          "<br>", alert_level
        )
      )
    p <- ggplot2::ggplot(
      x, ggplot2::aes(period_start, cases, colour = analysis_unit, group = analysis_unit, text = text)
    ) +
      ggplot2::geom_line(linewidth = .8) +
      ggplot2::geom_line(ggplot2::aes(y = threshold), linetype = "dashed", linewidth = .55, na.rm = TRUE) +
      ggplot2::geom_point(
        data = x |> dplyr::filter(alert), colour = "#c93c3c", fill = "#ffffff",
        shape = 21, size = 3, stroke = 1.1
      ) +
      ggplot2::facet_wrap(~diagnosis_final, scales = "free_y", ncol = 2) +
      ggplot2::labs(x = NULL, y = "Casos", colour = "US/grupo") + monitor_theme(9)
    to_plotly(p)
  })

  output$outbreak_alert_table <- DT::renderDT({
    x <- analysis_alert_data() |> dplyr::filter(alert)
    if (nrow(x) == 0) {
      return(datatable_pt(data.frame(Mensagem = "Nenhum período ultrapassa o limiar seleccionado")))
    }
    x <- x |>
      dplyr::transmute(
        `US/grupo` = analysis_unit, Diagnóstico = diagnosis_final, Período = period_label,
        Casos = cases, `Média histórica` = round(baseline, 1),
        Limiar = round(threshold, 1), Excesso = round(excess_cases, 1),
        Intensidade = alert_level, Método = method_label
      )
    datatable_pt(x, page_length = 15, buttons = TRUE) |>
      DT::formatStyle("Intensidade", backgroundColor = DT::styleEqual(
        c("Elevado", "Muito elevado"), c("#fff0c2", "#f8d7da")
      ))
  })

  output$who_definitions_ui <- shiny::renderUI({
    shiny::tagList(
      shiny::div(
        class = "method-card",
        shiny::h4("Sarampo — definição operacional da OMS"),
        shiny::p(
          "Caso suspeito: febre e erupção maculopapular generalizada não vesicular, ou qualquer pessoa em quem o profissional de saúde suspeite de sarampo. A definição clínica acrescenta tosse, coriza ou conjuntivite."
        ),
        shiny::p(
          "Sinal de surto suspeito: pelo menos 5 casos epidemiologicamente ligados, com inícios de erupção separados por 7–21 dias. Sinal confirmado: pelo menos 2 casos confirmados laboratorialmente, temporalmente relacionados (7–21 dias) e ligados epidemiológica ou virologicamente."
        ),
        shiny::tags$a(href = WHO_REFERENCE_URLS[["measles"]], target = "_blank", "Abrir referência oficial da OMS")
      ),
      shiny::div(
        class = "method-card",
        shiny::h4("Cólera — limiares operacionais da OMS"),
        shiny::p(
          "Na ausência de surto, o painel procura 2 ou mais casos suspeitos, ou 1 teste rápido positivo, na mesma unidade de vigilância em 7 dias. A classificação provável usa a tabela OMS de positivos entre os testados em 14 dias; a confirmação requer pelo menos um caso local confirmado por laboratório."
        ),
        shiny::tags$a(href = WHO_REFERENCE_URLS[["cholera"]], target = "_blank", "Abrir referência oficial da OMS")
      ),
      shiny::div(
        class = "privacy-note",
        shiny::strong("Importante: "),
        "os resultados abaixo são sinais para verificação. Quando faltam data de início, ligação epidemiológica ou testes específicos, o painel não declara um surto; mostra a lacuna e usa a data do evento apenas como aproximação."
      ),
      shiny::br()
    )
  })

  output$measles_alert_table <- DT::renderDT({
    x <- measles_alert_data()
    if (nrow(x) == 0) return(datatable_pt(data.frame(Mensagem = "Sem casos que satisfaçam a definição suspeita disponível")))
    x <- x |>
      dplyr::transmute(
        `US/grupo` = analysis_unit, `US de origem` = facility,
        `Data do evento` = event_date, `Início/aproximação` = measles_onset_date,
        `Data é aproximação` = ifelse(measles_onset_is_proxy, "Sim", "Não"),
        Faixa = age_group, Sexo = sex, Classificação = definition_status,
        `Suspeitos/21 dias` = suspected_21d, `Confirmados/21 dias` = confirmed_21d,
        `Ligados/21 dias` = linked_21d, `Amplitude do início (dias)` = onset_span_21d,
        `Datas aproximadas/21 dias` = proxy_21d, Sinal = signal
      )
    datatable_pt(x, page_length = 12, buttons = TRUE)
  })



#SARAMPO -MEASLES 
  
  # ==============================================================================
  # LÓGICA DO SERVIDOR - MÉTRICAS E EPIDEMIOLOGIA DE SARAMPO (OMS)
  # ==============================================================================
  
  # 1. UI Dinâmica de Definições de Caso (OMS)
  output$who_definitions_ui <- shiny::renderUI({
    shiny::tagList(
      shiny::div(
        class = "alert alert-info",
        style = "margin-bottom: 20px; border-left: 5px solid #0073b7;",
        shiny::tags$h5(shiny::tags$strong("Definições de Caso e Limiares de Alerta (OMS/MISAU):")),
        shiny::tags$ul(
          shiny::tags$li(shiny::tags$strong("Caso Suspeito: "), "Febre e exantema máculo-papular generalizado com pelo menos um dos sintomas: tosse, coriza ou conjuntivite."),
          shiny::tags$li(shiny::tags$strong("Limiar de Alerta Semanal: "), "≥ 1 caso suspeito notificado numa semana epidemiológica."),
          shiny::tags$li(shiny::tags$strong("Limiar de Surto: "), "≥ 5 casos suspeitos/confirmados num intervalo de 30 dias na mesma área geográfica (juízo clínico/laboratorial).")
        )
      )
    )
  })
  
  # ------------------------------------------------------------------------------
  # 2. Gráfico 1: Limiar Semanal (≥ 1 caso/semana)
  # ------------------------------------------------------------------------------
  output$measles_alert_plot <- plotly::renderPlotly({
    df <- if (exists("filtered_data") && is.reactive(filtered_data)) filtered_data() else hpt_data
    if (is.null(df) || nrow(df) == 0) return(no_plot())
    
    # Filter strictly for Measles / Sarampo cases and aggregate by Epidemiological Week
    weekly_data <- df %>%
      dplyr::filter(
        !is.na(event_date),
        !is.na(diagnosis),
        # Case-insensitive exact match or regex match for 'sarampo' / 'measles'
        stringr::str_trim(tolower(diagnosis)) %in% c("sarampo", "measles")
      ) %>%
      dplyr::mutate(
        event_date = as.Date(event_date),
        epi_year = lubridate::epiyear(event_date),
        epi_week = lubridate::epiweek(event_date),
        epi_week_date = lubridate::floor_date(event_date, unit = "week", week_start = 7)
      ) %>%
      dplyr::group_by(epi_week_date) %>%
      dplyr::summarise(casos_suspeitos = dplyr::n(), .groups = "drop")
    
    if (nrow(weekly_data) == 0) return(no_plot())
    
    # Plotly with Alert Line (≥ 1 case)
    plotly::plot_ly(weekly_data, x = ~epi_week_date) %>%
      plotly::add_trace(
        y = ~casos_suspeitos,
        type = "bar",
        name = "Casos Notificados",
        marker = list(color = "#3c8dbc"),
        hovertemplate = "Semana: %{x|%Y-W%U}<br>Casos: %{y}<extra></extra>"
      ) %>%
      plotly::add_lines(
        y = 1,
        name = "Limiar de Alerta (≥1 caso)",
        line = list(color = "orange", width = 2, dash = "dash")
      ) %>%
      plotly::layout(
        xaxis = list(title = "Semana Epidemiológica", showgrid = FALSE),
        yaxis = list(title = "Nº de Casos Suspeitos", zeroline = TRUE),
        hovermode = "x unified",
        legend = list(orientation = "h", x = 0.1, y = 1.15)
      )
  })
  
  # ------------------------------------------------------------------------------
  # 3. Gráfico 2: Limiar de Surto (≥ 5 casos / 30 Dias por Área Geográfica)
  # ------------------------------------------------------------------------------
  output$measles_outbreak_plot <- plotly::renderPlotly({
    df <- if (exists("filtered_data") && is.reactive(filtered_data)) filtered_data() else hpt_data
    if (is.null(df) || nrow(df) == 0) return(no_plot())
    
    # 30-day rolling window calculation grouped by Facility/District for Measles
    outbreak_data <- df %>%
      dplyr::filter(
        !is.na(event_date),
        !is.na(diagnosis),
        stringr::str_trim(tolower(diagnosis)) %in% c("sarampo", "measles")
      ) %>%
      dplyr::mutate(event_date = as.Date(event_date)) %>%
      dplyr::arrange(facility, event_date) %>%
      dplyr::group_by(facility, event_date) %>%
      dplyr::summarise(casos_dia = dplyr::n(), .groups = "drop_last") %>%
      dplyr::mutate(
        # 30-day rolling sum per District/Facility
        casos_30d = slider::slide_index_sum(
          x = casos_dia,
          i = event_date,
          before = lubridate::days(29)
        ),
        em_surto = casos_30d >= 5
      ) %>%
      dplyr::ungroup()
    
    if (nrow(outbreak_data) == 0) return(no_plot())
    
    # Group by date for overall plot highlighting outbreak incidence
    summary_outbreak <- outbreak_data %>%
      dplyr::group_by(event_date) %>%
      dplyr::summarise(
        max_casos_30d = max(casos_30d, na.rm = TRUE),
        us_em_surto = sum(em_surto, na.rm = TRUE),
        .groups = "drop"
      )
    
    plotly::plot_ly(summary_outbreak, x = ~event_date) %>%
      plotly::add_lines(
        y = ~max_casos_30d,
        name = "Máx. Casos (Janela 30d)",
        line = list(color = "#00c0ef", width = 2)
      ) %>%
      plotly::add_lines(
        y = 5,
        name = "Limiar de Surto (≥5 casos/30d)",
        line = list(color = "red", width = 2.5, dash = "dot")
      ) %>%
      plotly::layout(
        xaxis = list(title = "Data de Notificação"),
        yaxis = list(title = "Acumulado de Casos (30 dias)"),
        hovermode = "x unified",
        legend = list(orientation = "h", x = 0.1, y = 1.15)
      )
  })
  
  # ------------------------------------------------------------------------------
  # 4. Tabela de Monitoria de Alertas e Surtos por Unidade Sanitária
  # ------------------------------------------------------------------------------
  output$measles_alert_table <- DT::renderDT({
    df <- if (exists("filtered_data") && is.reactive(filtered_data)) filtered_data() else hpt_data
    if (is.null(df) || nrow(df) == 0) return(NULL)
    
    # Consolidate alert and outbreak indicators by Health Facility
    table_summary <- df %>%
      dplyr::filter(
        !is.na(event_date),
        !is.na(diagnosis),
        stringr::str_trim(tolower(diagnosis)) %in% c("sarampo", "measles")
      ) %>%
      dplyr::mutate(event_date = as.Date(event_date)) %>%
      dplyr::group_by(facility_code, facility) %>%
      dplyr::summarise(
        total_casos = dplyr::n(),
        ultim_caso = max(event_date, na.rm = TRUE),
        casos_ultimos_30d = sum(event_date >= (Sys.Date() - 30), na.rm = TRUE),
        status_epidemiologico = dplyr::case_when(
          casos_ultimos_30d >= 5 ~ "SURTO (≥5 casos/30d)",
          casos_ultimos_30d >= 1 ~ "ALERTA (≥1 caso/30d)",
          TRUE ~ "Sem Alerta"
        ),
        .groups = "drop"
      ) %>%
      dplyr::arrange(dplyr::desc(casos_ultimos_30d))
    
    DT::datatable(
      table_summary,
      colnames = c("Código US", "Unidade Sanitária", "Total Histórico", "Última Notificação", "Casos (Últimos 30 Dias)", "Estado Epidemiológico"),
      options = list(
        pageLength = 5,
        autoWidth = TRUE,
        language = list(url = "//cdn.datatables.net/plug-ins/1.10.25/i18n/Portuguese-Brasil.json")
      ),
      rownames = FALSE
    ) %>%
      DT::formatStyle(
        "status_epidemiologico",
        backgroundColor = DT::styleEqual(
          c("SURTO (≥5 casos/30d)", "ALERTA (≥1 caso/30d)", "Sem Alerta"),
          c("#f2dede", "#fcf8e3", "#dff0d8")
        ),
        color = DT::styleEqual(
          c("SURTO (≥5 casos/30d)", "ALERTA (≥1 caso/30d)", "Sem Alerta"),
          c("#a94442", "#8a6d3b", "#3c763d")
        ),
        fontWeight = "bold"
      )
  })
  
#FIM METODOS DE SARAMPO
  
  
  
  
# INICIO DE DIARREA
  
  # ==============================================================================
  # DIARRHEA (OMS / WHO STANDARDS) — SERVER OUTPUTS
  # ==============================================================================
  
  # 1. WHO Case Definition UI Box
  output$who_diarrhea_definitions_ui <- shiny::renderUI({
    shiny::div(
      class = "method-card",
      shiny::h4("Definições de Caso e Limiares Epidemiológicos da OMS (Diarreia / Cólera)"),
      shiny::tags$ul(
        shiny::tags$li(
          shiny::strong("Diarreia Aquosa Aguda (DAA):"), 
          " 3 ou mais deposições líquidas ou semilíquidas em 24 horas. Monitoria semanal para deteção de picos acentuados."
        ),
        shiny::tags$li(
          shiny::strong("Limiar de Alerta Semanal:"), 
          " Notificação superior à média histórica + 2 desvios-padrão (SD) na mesma área geográfica."
        ),
        shiny::tags$li(
          shiny::strong("Limiar de Declaração de Surto (Suspeita de Cólera):"), 
          " Em áreas sem surto ativo: 1 caso suspeito de cólera com teste rápido positivo ou desidratação grave/morte por DAA em doente com ≥5 anos."
        )
      )
    )
  })
  
  # 2. Weekly Diarrhea Alert Plot (Trend vs Historical Baseline Threshold)
  output$diarrhea_alert_plot <- plotly::renderPlotly({
    df <- if (exists("filtered_data") && is.reactive(filtered_data)) filtered_data() else hpt_data
    if (is.null(df) || nrow(df) == 0) return(no_plot())
    
    # Filter strictly for Diarrhea / Cholera cases
    weekly_data <- df %>%
      dplyr::filter(
        !is.na(event_date),
        !is.na(diagnosis),
        stringr::str_trim(tolower(diagnosis)) %in% c("diarreia", "diarrhea", "diarreia aquosa aguda", "colera", "cholera")
      ) %>%
      dplyr::mutate(
        event_date = as.Date(event_date),
        epi_week_date = lubridate::floor_date(event_date, unit = "week", week_start = 7)
      ) %>%
      dplyr::group_by(epi_week_date) %>%
      dplyr::summarise(casos_notificados = dplyr::n(), .groups = "drop")
    
    if (nrow(weekly_data) == 0) return(no_plot())
    
    # Dynamic baseline average calculation (e.g. mean + 2SD threshold baseline)
    mean_cases <- mean(weekly_data$casos_notificados, na.rm = TRUE)
    sd_cases <- sd(weekly_data$casos_notificados, na.rm = TRUE)
    alert_threshold <- max(ceiling(mean_cases + (2 * ifelse(is.na(sd_cases), 0, sd_cases))), 5)
    
    plotly::plot_ly(weekly_data, x = ~epi_week_date) %>%
      plotly::add_trace(
        y = ~casos_notificados,
        type = "bar",
        name = "Casos Notificados",
        marker = list(color = "#00838f"),
        hovertemplate = "Semana: %{x|%Y-W%U}<br>Casos: %{y}<extra></extra>"
      ) %>%
      plotly::add_lines(
        y = alert_threshold,
        name = paste0("Limiar de Alerta (≥ ", alert_threshold, " casos/semana)"),
        line = list(color = "orange", width = 2, dash = "dash")
      ) %>%
      plotly::layout(
        xaxis = list(title = "Semana Epidemiológica", showgrid = FALSE),
        yaxis = list(title = "Nº de Casos de Diarreia", zeroline = TRUE),
        hovermode = "x unified",
        legend = list(orientation = "h", x = 0.1, y = 1.15)
      )
  })
  
  # 3. 30-Day Rolling Outbreak Window Plot (Facility Level Clustering)
  output$diarrhea_outbreak_plot <- plotly::renderPlotly({
    df <- if (exists("filtered_data") && is.reactive(filtered_data)) filtered_data() else hpt_data
    if (is.null(df) || nrow(df) == 0) return(no_plot())
    
    outbreak_data <- df %>%
      dplyr::filter(
        !is.na(event_date),
        !is.na(diagnosis),
        stringr::str_trim(tolower(diagnosis)) %in% c("diarreia", "diarrhea", "diarreia aquosa aguda", "colera", "cholera")
      ) %>%
      dplyr::mutate(event_date = as.Date(event_date)) %>%
      dplyr::arrange(facility, event_date) %>%
      dplyr::group_by(facility, event_date) %>%
      dplyr::summarise(casos_dia = dplyr::n(), .groups = "drop_last") %>%
      dplyr::mutate(
        casos_30d = slider::slide_index_sum(
          x = casos_dia,
          i = event_date,
          before = lubridate::days(29)
        ),
        em_surto = casos_30d >= 10 # OMS Outbreak Trigger Benchmark
      ) %>%
      dplyr::ungroup()
    
    if (nrow(outbreak_data) == 0) return(no_plot())
    
    summary_outbreak <- outbreak_data %>%
      dplyr::group_by(event_date) %>%
      dplyr::summarise(
        max_casos_30d = max(casos_30d, na.rm = TRUE),
        us_em_surto = sum(em_surto, na.rm = TRUE),
        .groups = "drop"
      )
    
    plotly::plot_ly(summary_outbreak, x = ~event_date) %>%
      plotly::add_lines(
        y = ~max_casos_30d,
        name = "Máx. Casos Por US (Janela 30d)",
        line = list(color = "#0056b3", width = 2)
      ) %>%
      plotly::add_lines(
        y = 10,
        name = "Limiar de Surto (≥ 10 casos/30d por US)",
        line = list(color = "red", width = 2.5, dash = "dot")
      ) %>%
      plotly::layout(
        xaxis = list(title = "Data de Notificação"),
        yaxis = list(title = "Acumulado de Casos (30 dias)"),
        hovermode = "x unified",
        legend = list(orientation = "h", x = 0.1, y = 1.15)
      )
  })
  
  # 4. Facility Alert & Outbreak Summary Table
  output$diarrhea_alert_table <- DT::renderDT({
    df <- if (exists("filtered_data") && is.reactive(filtered_data)) filtered_data() else hpt_data
    if (is.null(df) || nrow(df) == 0) return(NULL)
    
    table_summary <- df %>%
      dplyr::filter(
        !is.na(event_date),
        !is.na(diagnosis),
        stringr::str_trim(tolower(diagnosis)) %in% c("diarreia", "diarrhea", "diarreia aquosa aguda", "colera", "cholera")
      ) %>%
      dplyr::mutate(event_date = as.Date(event_date)) %>%
      dplyr::group_by(facility_code, facility) %>%
      dplyr::summarise(
        total_casos = dplyr::n(),
        ultim_caso = max(event_date, na.rm = TRUE),
        casos_ultimos_30d = sum(event_date >= (Sys.Date() - 30), na.rm = TRUE),
        status_epidemiologico = dplyr::case_when(
          casos_ultimos_30d >= 10 ~ "SURTO (≥10 casos/30d)",
          casos_ultimos_30d >= 3  ~ "ALERTA (≥3 casos/30d)",
          TRUE ~ "Sem Alerta"
        ),
        .groups = "drop"
      ) %>%
      dplyr::arrange(dplyr::desc(casos_ultimos_30d))
    
    DT::datatable(
      table_summary,
      colnames = c("Código US", "Unidade Sanitária", "Total Histórico", "Última Notificação", "Casos (Últimos 30 Dias)", "Estado Epidemiológico"),
      options = list(
        pageLength = 5,
        autoWidth = TRUE,
        language = list(url = "//cdn.datatables.net/plug-ins/1.10.25/i18n/Portuguese-Brasil.json")
      ),
      rownames = FALSE
    ) %>%
      DT::formatStyle(
        "status_epidemiologico",
        backgroundColor = DT::styleEqual(
          c("SURTO (≥10 casos/30d)", "ALERTA (≥3 casos/30d)", "Sem Alerta"),
          c("#f2dede", "#fcf8e3", "#dff0d8")
        ),
        color = DT::styleEqual(
          c("SURTO (≥10 casos/30d)", "ALERTA (≥3 casos/30d)", "Sem Alerta"),
          c("#a94442", "#8a6d3b", "#3c763d")
        ),
        fontWeight = "bold"
      )
  })
  
  
  # FIM DE DIARREA
  

  
  
  
  # INICIO DE DIABETE MELITUS
  
  # ==============================================================================
  # DIABETES MELLITUS (TIPO I & II) — OMS / WHO SURVEILLANCE MODULE
  # ==============================================================================
  
  # 1. WHO Case Definition & Clinical Target UI Box
  output$who_diabetes_definitions_ui <- shiny::renderUI({
    shiny::div(
      class = "method-card",
      shiny::h4("Critérios Diagnósticos e Alvos de Controlo da OMS (Diabetes Mellitus)"),
      shiny::tags$ul(
        shiny::tags$li(
          shiny::strong("Critérios Diagnósticos (OMS):"), 
          " Glicemia em jejum ≥ 126 mg/dL (7.0 mmol/L) OU HbA1c ≥ 6.5% (48 mmol/mol) OU Glicemia casual ≥ 200 mg/dL com sintomas."
        ),
        shiny::tags$li(
          shiny::strong("Diabetes Tipo 1 vs. Tipo 2:"), 
          " Tipo 1 (deficiência absoluta de insulina, diagnóstico frequente em jovens) vs. Tipo 2 (resistência à insulina/deficiência relativa, responsável por >90% dos casos)."
        ),
        shiny::tags$li(
          shiny::strong("Alvo de Controlo Glicémico (OMS Package of Essential NCD Interventions - PEN):"), 
          " Manter HbA1c < 7.0% (ou Glicemia em Jejum < 130 mg/dL) para prevenção de complicações micro e macrovasculares."
        )
      )
    )
  })
  
  # 2. Monthly Diagnoses Breakdown by Type (Type 1 vs. Type 2)
  output$diabetes_incidence_plot <- plotly::renderPlotly({
    df <- if (exists("filtered_data") && is.reactive(filtered_data)) filtered_data() else hpt_data
    if (is.null(df) || nrow(df) == 0) return(no_plot())
    
    # Normalize spacing and lowercases for exact match against "Diabetes Mellitus - Tipo I" & "Diabetes Mellitus - Tipo II"
    diabetes_data <- df %>%
      dplyr::filter(
        !is.na(event_date),
        !is.na(diagnosis),
        stringr::str_detect(
          stringr::str_squish(tolower(diagnosis)), 
          "^diabetes mellitus\\s*-\\s*tipo\\s*(i{1,2}|1|2)$"
        )
      ) %>%
      dplyr::mutate(
        event_date = as.Date(event_date),
        month_date = lubridate::floor_date(event_date, unit = "month"),
        clean_diag = stringr::str_squish(tolower(diagnosis)),
        tipo_diabetes = dplyr::case_when(
          stringr::str_detect(clean_diag, "tipo\\s*i$|tipo\\s*1$") ~ "Diabetes Mellitus - Tipo I",
          stringr::str_detect(clean_diag, "tipo\\s*ii$|tipo\\s*2$") ~ "Diabetes Tipo II",
          TRUE ~ "Diabetes Mellitus - Tipo II"
        )
      ) %>%
      dplyr::group_by(month_date, tipo_diabetes) %>%
      dplyr::summarise(novos_casos = dplyr::n(), .groups = "drop")
    
    if (nrow(diabetes_data) == 0) return(no_plot())
    
    plotly::plot_ly(
      diabetes_data, 
      x = ~month_date, 
      y = ~novos_casos, 
      color = ~tipo_diabetes,
      colors = c(
        "Diabetes Mellitus - Tipo I" = "#e65100", 
        "Diabetes Tipo II" = "#0288d1"
      ),
      type = "bar"
    ) %>%
      plotly::layout(
        barmode = "stack",
        xaxis = list(title = "Mês de Registo", showgrid = FALSE),
        yaxis = list(title = "Nº de Novos Casos"),
        hovermode = "x unified",
        legend = list(orientation = "h", x = 0.1, y = 1.15)
      )
  })
  
  # 3. Glycemic Control & Uncontrolled Risk Tracking Plot
  # ==============================================================================
  # 3. DIABETES GLYCEMIC CONTROL & RISK TRACKING PLOT
  # ==============================================================================
  output$diabetes_control_plot <- plotly::renderPlotly({
    df <- if (exists("filtered_data") && is.reactive(filtered_data)) filtered_data() else hpt_data
    if (is.null(df) || nrow(df) == 0) return(no_plot())
    
    # Filter strictly using normalized regex for Diabetes Tipo I and II
    control_data <- df %>%
      dplyr::filter(
        !is.na(event_date),
        !is.na(diagnosis),
        stringr::str_detect(
          stringr::str_squish(tolower(diagnosis)), 
          "^diabetes mellitus\\s*-\\s*tipo\\s*(i{1,2}|1|2)$"
        )
      ) %>%
      dplyr::mutate(
        event_date = as.Date(event_date),
        month_date = lubridate::floor_date(event_date, unit = "month"),
        # Dynamic check for clinical threshold status column if available
        status_glicemico = if ("glucose_level" %in% names(.)) {
          dplyr::case_when(
            glucose_level >= 200 ~ "Descompensado / Alerta (≥ 200 mg/dL)",
            glucose_level >= 130 ~ "Subotimizar (130-199 mg/dL)",
            TRUE ~ "Controlado (< 130 mg/dL)"
          )
        } else {
          "Em Seguimento Clinico"
        }
      ) %>%
      dplyr::group_by(month_date, status_glicemico) %>%
      dplyr::summarise(total_pacientes = dplyr::n(), .groups = "drop")
    
    if (nrow(control_data) == 0) return(no_plot())
    
    plotly::plot_ly(
      control_data, 
      x = ~month_date, 
      y = ~total_pacientes, 
      color = ~status_glicemico,
      type = "scatter",
      mode = "lines+markers"
    ) %>%
      plotly::layout(
        xaxis = list(title = "Mês de Acompanhamento"),
        yaxis = list(title = "Total de Pacientes"),
        hovermode = "x unified",
        legend = list(orientation = "h", x = 0.0, y = 1.15)
      )
  })
  
  # ==============================================================================
  # 4. FACILITY DIABETES SURVEILLANCE & PATIENT LOAD TABLE
  # ==============================================================================
  output$diabetes_monitoring_table <- DT::renderDT({
    df <- if (exists("filtered_data") && is.reactive(filtered_data)) filtered_data() else hpt_data
    if (is.null(df) || nrow(df) == 0) return(NULL)
    
    table_summary <- df %>%
      dplyr::filter(
        !is.na(event_date),
        !is.na(diagnosis),
        stringr::str_detect(
          stringr::str_squish(tolower(diagnosis)), 
          "^diabetes mellitus\\s*-\\s*tipo\\s*(i{1,2}|1|2)$"
        )
      ) %>%
      dplyr::mutate(
        event_date = as.Date(event_date),
        clean_diag = stringr::str_squish(tolower(diagnosis)),
        is_tipo1 = stringr::str_detect(clean_diag, "tipo\\s*i$|tipo\\s*1$"),
        is_tipo2 = stringr::str_detect(clean_diag, "tipo\\s*ii$|tipo\\s*2$")
      ) %>%
      dplyr::group_by(facility_code, facility) %>%
      dplyr::summarise(
        total_pacientes = dplyr::n(),
        casos_tipo1 = sum(is_tipo1, na.rm = TRUE),
        casos_tipo2 = sum(is_tipo2, na.rm = TRUE),
        ultim_consulta = max(event_date, na.rm = TRUE),
        novos_ultimos_30d = sum(event_date >= (Sys.Date() - 30), na.rm = TRUE),
        nivel_carga = dplyr::case_when(
          novos_ultimos_30d >= 15 ~ "Carga Elevada (≥15/mês)",
          novos_ultimos_30d >= 5  ~ "Carga Moderada (5-14/mês)",
          TRUE ~ "Carga Baixa (<5/mês)"
        ),
        .groups = "drop"
      ) %>%
      dplyr::arrange(dplyr::desc(novos_ultimos_30d))
    
    DT::datatable(
      table_summary,
      colnames = c("Código US", "Unidade Sanitária", "Total Registados", "Tipo 1", "Tipo 2", "Último Registo", "Novos (Últimos 30 Dias)", "Nível de Carga US"),
      options = list(
        pageLength = 5,
        autoWidth = TRUE,
        language = list(url = "//cdn.datatables.net/plug-ins/1.10.25/i18n/Portuguese-Brasil.json")
      ),
      rownames = FALSE
    ) %>%
      DT::formatStyle(
        "nivel_carga",
        backgroundColor = DT::styleEqual(
          c("Carga Elevada (≥15/mês)", "Carga Moderada (5-14/mês)", "Carga Baixa (<5/mês)"),
          c("#f2dede", "#fcf8e3", "#dff0d8")
        ),
        color = DT::styleEqual(
          c("Carga Elevada (≥15/mês)", "Carga Moderada (5-14/mês)", "Carga Baixa (<5/mês)"),
          c("#a94442", "#8a6d3b", "#3c763d")
        ),
        fontWeight = "bold"
      )
  })
  
  #FIM DE DIABETE MELITUS 


  output$facility_variation_plot <- plotly::renderPlotly({
    x <- facility_variation_data()
    if (nrow(x) == 0) return(no_plot("Seleccione diagnósticos presentes nas US"))
    top <- x |>
      dplyr::group_by(diagnosis_final) |>
      dplyr::summarise(total = sum(cases), .groups = "drop") |>
      dplyr::slice_max(total, n = 12, with_ties = FALSE) |>
      dplyr::pull(diagnosis_final)
    x <- x |>
      dplyr::filter(diagnosis_final %in% top) |>
      dplyr::mutate(text = paste0(
        facility, "<br>", diagnosis_final, "<br>Casos: ", cases,
        "<br>Por 100 contactos: ", round(proportion_per_100_contacts, 1),
        "<br>Relativo à média: ", round(relative_to_average, 2), "×<br>", variation_signal
      ))
    p <- ggplot2::ggplot(
      x, ggplot2::aes(diagnosis_final, facility, fill = proportion_per_100_contacts, text = text)
    ) +
      ggplot2::geom_tile(colour = "white", linewidth = .3) +
      ggplot2::scale_fill_gradient(low = "#eef4f5", high = "#c93c3c") +
      ggplot2::labs(x = NULL, y = NULL, fill = "Por 100\ncontactos") + monitor_theme(9) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 40, hjust = 1))
    to_plotly(p)
  })

  output$facility_variation_table <- DT::renderDT({
    x <- facility_variation_data()
    if (nrow(x) == 0) return(datatable_pt(data.frame(Mensagem = "Sem dados")))
    x <- x |>
      dplyr::transmute(
        `Unidade sanitária` = facility, Diagnóstico = diagnosis_final, Casos = cases,
        `Todos os contactos` = all_contacts,
        `Casos/100 contactos` = round(proportion_per_100_contacts, 2),
        `Casos/1.000 habitantes` = round(cases_per_1000, 2),
        `Relativo à média` = round(relative_to_average, 2),
        `Z-score` = round(z_score, 2), Sinal = variation_signal
      )
    datatable_pt(x, page_length = 15, buttons = TRUE)
  })

  output$download_denominator_template <- shiny::downloadHandler(
    filename = function() {
      "modelo_denominadores_us.csv"
    },
    content = function(file) {
      x <- analysis_universe_data() |>
        dplyr::distinct(facility_code, facility) |>
        dplyr::filter(has_value(facility)) |>
        dplyr::mutate(population = NA_real_, target_population = NA_real_) |>
        dplyr::arrange(facility)
      readr::write_excel_csv(x, file, na = "")
    }
  )



  # ---------- Pontualidade ----------
  output$delay_histogram <- plotly::renderPlotly({
    # 1. Agrupa os dados filtrados por data do registo e Unidade Sanitária
    x <- filtered_data() |> 
      dplyr::filter(!is.na(event_date), !is.na(facility)) |> 
      dplyr::mutate(data = as.Date(event_date)) |> 
      dplyr::group_by(data, facility) |> 
      dplyr::summarise(
        entradas = dplyr::n(),
        .groups = "drop"
      )
    
    if (nrow(x) == 0) return(no_plot("Sem registos de entradas para o período selecionado"))
    
    # 2. Gera o gráfico de linhas com evolução diária por Unidade Sanitária
    p <- ggplot2::ggplot(x, ggplot2::aes(
      x = data, 
      y = entradas, 
      color = facility,
      group = facility,
      text = paste0("Data: ", format(data, "%d/%m/%Y"),
                    "<br>Unidade Sanitária: ", facility,
                    "<br>Entradas: ", entradas)
    )) +
      ggplot2::geom_line(linewidth = 0.8) +
      ggplot2::geom_point(size = 1.8) +
      ggplot2::labs(
        x = "Data do Registo", 
        y = "Número de Entradas / Notificações",
        title = "Relação Diária de Entradas por Unidade Sanitária",
        color = "Unidade Sanitária"
      ) + 
      monitor_theme() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
    
    to_plotly(p, tooltip = "text")
  })


  # ---------- Alertas ----------
  output$facility_alert_table <- DT::renderDT({
    x <- facility_alert_summary(filtered_data(), input$late_days %||% 2)
    if (nrow(x) == 0) return(datatable_pt(data.frame(Mensagem = "Sem dados")))
    x <- x |>
      dplyr::transmute(
        Província = province, Distrito = district, `Unidade sanitária` = facility,
        Registos = records, Alta = high, Média = medium, Baixa = low,
        Incompletos = incomplete, Atrasados = late, `IDs repetidos` = duplicate_id,
        `Possíveis duplicados` = possible_duplicates, Inconsistentes = inconsistent,
        `Qualidade (%)` = round(quality_score, 1), Situação = status
      )
    datatable_pt(x, page_length = 15, buttons = TRUE) |>
      DT::formatStyle("Alta", backgroundColor = DT::styleInterval(0, c("#ffffff", "#f8d7da"))) |>
      DT::formatStyle("Qualidade (%)", backgroundColor = DT::styleInterval(
        c(60, 80, 90), c("#f8d7da", "#fff0c2", "#d9f2df", "#c9eee4")
      ))
  })

  output$record_alert_table <- DT::renderDT({
    x <- alerts_table(filtered_data())
    if (nrow(x) == 0) x <- data.frame(Mensagem = "Nenhum alerta nos filtros seleccionados")
    datatable_pt(x, page_length = 20, buttons = TRUE)
  })

  # ---------- Dados e exportação ----------
  output$safe_data_table <- DT::renderDT({
    x <- safe_export_data(filtered_data())
    datatable_pt(x, page_length = 20, buttons = TRUE)
  })

  output$download_csv <- shiny::downloadHandler(
    filename = function() {
      paste0("dados_monitoria_filtrados_", Sys.Date(), ".csv")
    },
    content = function(file) {
      readr::write_excel_csv(safe_export_data(filtered_data()), file, na = "")
    }
  )

  output$download_alerts <- shiny::downloadHandler(
    filename = function() {
      paste0("alertas_qualidade_", Sys.Date(), ".csv")
    },
    content = function(file) {
      readr::write_excel_csv(alerts_table(filtered_data()), file, na = "")
    }
  )
  
  
  
  #Badplus Data
  # -----------------------------------------------------------------------------
  # ABA BADPLUS - PROCESSAMENTO E GRÁFICOS POR HEALTH FACILITY
  # -----------------------------------------------------------------------------
  
  # 1. Atualização dinâmica dos escolhas do filtro Health Facility (organisationunitname)
  shiny::observe({
    df <- if (is.function(dados)) dados() else dados
    shiny::req(df)
    
    if ("organisationunitname" %in% names(df)) {
      facilities <- sort(unique(na.omit(df$organisationunitname)))
      shiny::updateSelectInput(
        session,
        inputId = "badplus_org_select",
        choices = c("Todas as Health Facilities" = "ALL", facilities),
        selected = input$badplus_org_select %||% "ALL"
      )
    }
  })
  
  # 2. Processamento Reativo com Filtro de Health Facility
  badplus_processed <- shiny::reactive({
    df <- if (is.function(dados)) dados() else dados
    shiny::req(df)
    
    # Filtro reativo por Health Facility (organisationunitname)
    if (!is.null(input$badplus_org_select) && input$badplus_org_select != "ALL" && "organisationunitname" %in% names(df)) {
      df <- df %>% dplyr::filter(organisationunitname == input$badplus_org_select)
    }
    
    target_cols <- c(
      "RDTR - BED - Total casos diarreia",
      "RDTR - BED - Diarreia - Óbitos",
      "RDTR - BED - Total casos cólera",
      "RDTR - BED - Cólera - Óbitos",
      "RDTR - BED - PFA - Casos",
      "RDTR - BED - PFA - Óbitos",
      "RDTR - BED - Malaria - Casos",
      "RDTR - BED - Malaria - Óbitos",
      "RDTR - BED - Sarampo - Casos",
      "RDTR - BED - Sarampo - Óbitos",
      "RDTR - BED - Sindrome Febril - Casos",
      "RDTR - BED - Sindrome Febril - Óbitos",
      "RDTR - BED - IRAS - Casos",
      "RDTR - BED - IRAS - Óbitos"
    )
    
    bed_cols <- intersect(names(df), target_cols)
    if (length(bed_cols) == 0) {
      bed_cols <- names(df)[stringr::str_detect(names(df), "(RDTR|BED)")]
    }
    
    shiny::req(length(bed_cols) > 0)
    
    df_selected <- df %>%
      dplyr::select(
        dplyr::any_of(c("organisationunitname", "periodname", "periodcode")),
        dplyr::all_of(bed_cols)
      )
    
    df_long <- df_selected %>%
      tidyr::pivot_longer(
        cols = dplyr::all_of(bed_cols),
        names_to = "Variavel",
        values_to = "Valor"
      ) %>%
      dplyr::mutate(
        Valor = suppressWarnings(as.numeric(Valor)),
        Valor = dplyr::coalesce(Valor, 0),
        Tipo = dplyr::if_else(stringr::str_detect(Variavel, "(Óbitos|obitos)"), "Óbitos", "Casos"),
        Doenca = dplyr::case_when(
          stringr::str_detect(Variavel, "(?i)diarreia") ~ "Diarreia",
          stringr::str_detect(Variavel, "(?i)cólera|colera") ~ "Cólera",
          stringr::str_detect(Variavel, "(?i)pfa") ~ "PFA",
          stringr::str_detect(Variavel, "(?i)malaria") ~ "Malaria",
          stringr::str_detect(Variavel, "(?i)sarampo") ~ "Sarampo",
          stringr::str_detect(Variavel, "(?i)sindrome febril|febril") ~ "Sindrome Febril",
          stringr::str_detect(Variavel, "(?i)iras") ~ "IRAS",
          TRUE ~ Variavel
        )
      )
    
    return(df_long)
  })
  
  # 3. Gráfico 1: Tendência Temporal
  output$badplus_trend <- plotly::renderPlotly({
    shiny::req(input$badplus_disease_select)
    
    df_disease <- badplus_processed() %>%
      dplyr::filter(Doenca == input$badplus_disease_select) %>%
      dplyr::group_by(periodname, Tipo) %>%
      dplyr::summarise(Total = sum(Valor, na.rm = TRUE), .groups = "drop")
    
    shiny::req(nrow(df_disease) > 0)
    
    facility_label <- if (is.null(input$badplus_org_select) || input$badplus_org_select == "ALL") "Todas as Health Facilities" else input$badplus_org_select
    
    plotly::plot_ly(
      data = df_disease,
      x = ~periodname,
      y = ~Total,
      color = ~Tipo,
      colors = c("Casos" = "#1f77b4", "Óbitos" = "#d62728"),
      type = 'scatter',
      mode = 'lines+markers'
    ) %>%
      plotly::layout(
        title = paste("Evolução Temporal -", input$badplus_disease_select, "<br><sup>Facility:", facility_label, "</sup>"),
        xaxis = list(title = "Período"),
        yaxis = list(title = "Número de Registos"),
        hovermode = "x unified"
      )
  })
  
  # 4. Gráfico 2: Comparativo Entre Doenças
  output$badplus_summary_bar <- plotly::renderPlotly({
    shiny::req(input$badplus_type)
    
    df_bar <- badplus_processed() %>%
      dplyr::filter(Tipo == input$badplus_type) %>%
      dplyr::group_by(Doenca) %>%
      dplyr::summarise(Total = sum(Valor, na.rm = TRUE), .groups = "drop") %>%
      dplyr::arrange(desc(Total))
    
    plotly::plot_ly(
      data = df_bar,
      x = ~Total,
      y = ~reorder(Doenca, Total),
      type = 'bar',
      orientation = 'h',
      marker = list(color = '#2ca02c')
    ) %>%
      plotly::layout(
        xaxis = list(title = paste("Total de", input$badplus_type)),
        yaxis = list(title = "Doença / Diagnóstico")
      )
  })
  
  # 5. Novo Gráfico 3: Total de Registos por Health Facility
  output$badplus_facility_bar <- plotly::renderPlotly({
    shiny::req(input$badplus_type)
    
    # Seleciona dados brutos sem filtrar por facility individual se "ALL" estiver ativo
    df_fac <- badplus_processed() %>%
      dplyr::filter(Tipo == input$badplus_type) %>%
      dplyr::group_by(organisationunitname, Doenca) %>%
      dplyr::summarise(Total = sum(Valor, na.rm = TRUE), .groups = "drop")
    
    shiny::req(nrow(df_fac) > 0)
    
    plotly::plot_ly(
      data = df_fac,
      x = ~organisationunitname,
      y = ~Total,
      color = ~Doenca,
      type = 'bar'
    ) %>%
      plotly::layout(
        barmode = 'stack',
        title = paste("Totais por Health Facility -", input$badplus_type),
        xaxis = list(title = "Health Facility", tickangle = -45),
        yaxis = list(title = paste("Total de", input$badplus_type)),
        legend = list(title = list(text = 'Doença'))
      )
  })
  
  # 6. Tabela DT Filtrada
  output$badplus_table <- DT::renderDT({
    df <- if (is.function(dados)) dados() else dados
    shiny::req(df)
    
    if (!is.null(input$badplus_org_select) && input$badplus_org_select != "ALL" && "organisationunitname" %in% names(df)) {
      df <- df %>% dplyr::filter(organisationunitname == input$badplus_org_select)
    }
    
    cols_presentes <- intersect(
      c("organisationunitname", "periodname", names(df)[stringr::str_detect(names(df), "(RDTR|BED)")]),
      names(df)
    )
    
    df_table <- df %>% dplyr::select(dplyr::all_of(cols_presentes))
    
    DT::datatable(
      df_table,
      options = list(pageLength = 10, scrollX = TRUE),
      rownames = FALSE
    )
  })
  
  #Fim dos dados Badplus

}
