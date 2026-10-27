# ============================================================
# DASHBOARD INTERFACE — PROJECT ALIVE (UPDATED PALETTE)
# ============================================================

kpi_placeholder <- function(output_id) {
  shiny::uiOutput(output_id, class = "kpi-placeholder")
}

panel_box <- function(..., title = NULL, width = 12, status = NULL, solid_header = FALSE) {
  shinydashboard::box(
    ...,
    title = title,
    width = width,
    status = status,
    solidHeader = solid_header,
    collapsible = FALSE
  )
}

app_css <- "
:root {
  --primary: #5b8def;
  --primary-dark: #3b67c8;
  --secondary: #7ad8d0;
  --accent: #8fcd88;
  --accent-light: #eaf9ec;
  --blue-light: #edf6ff;
  --amber: #f3b76b;
  --red: #ee7d7d;
  --ink: #20314a;
  --muted: #5e7286;
  --page: #f5f8fc;
  --panel: #ffffff;
  --panel-border: #e6eef7;
}

.content-wrapper, .right-side { background: linear-gradient(180deg, #f7f9fd 0%, #f1f6fb 100%); }

.main-header .logo {
  background: linear-gradient(135deg, var(--primary-dark), var(--primary)) !important;
  color: #ffffff !important;
  font-weight: 800;
}
.main-header .navbar {
  background: linear-gradient(135deg, var(--primary), #7ba3ff) !important;
  box-shadow: 0 1px 0 rgba(35, 68, 120, 0.08);
}

.skin-blue .main-sidebar {
  background: linear-gradient(180deg, #10263f 0%, #173a5d 100%) !important;
}
.skin-blue .sidebar-menu > li.active > a,
.skin-blue .sidebar-menu > li:hover > a {
  border-left-color: var(--accent) !important;
  background: rgba(110, 161, 255, 0.18) !important;
  color: #ffffff !important;
}
.sidebar-section-title {
  color: #cfe8ff;
  text-transform: uppercase;
  font-size: 10px;
  letter-spacing: .12em;
  font-weight: 800;
  padding: 14px 15px 6px;
}
.sidebar .form-group { padding: 0 12px; margin-bottom: 11px; }
.sidebar label { color: #edf7ff; font-size: 12px; }
.sidebar .selectize-input, .sidebar .form-control { font-size: 12px; }
.sidebar .btn { width: calc(100% - 24px); margin: 4px 12px; }
.main-header .dropdown { padding: 8px 14px 0 0; }
.main-header .dropdown .form-group { margin-bottom: 0; }
.main-header .dropdown .selectize-input, .main-header .dropdown .form-control {
border-radius: 999px; border: 1px solid rgba(255,255,255,0.65); background: rgba(255,255,255,0.12); color: #fff; min-height: 30px; font-size: 12px;
}
.main-header .dropdown .selectize-input input { color: #fff; }
.main-header .dropdown .selectize-control.single .selectize-input:after { border-color: #fff transparent transparent transparent; }

.dashboard-title { color: var(--ink); font-weight: 800; margin: 4px 0 2px; }
.dashboard-subtitle { color: var(--muted); margin-bottom: 16px; }

.data-banner {
  display: flex; justify-content: space-between; gap: 12px; align-items: center;
  background: linear-gradient(180deg, #edf6ff 0%, #f4f9ff 100%);
  border: 1px solid #cfe0f8;
  border-radius: 12px;
  padding: 12px 16px; margin-bottom: 14px; color: var(--primary-dark);
  box-shadow: 0 8px 18px rgba(67, 108, 175, 0.06);
}

.kpi-grid {
  display: grid; grid-template-columns: repeat(4, minmax(0, 1fr));
  gap: 13px; margin-bottom: 13px;
}
.kpi-card {
  min-height: 118px; background: linear-gradient(180deg, #ffffff 0%, #fbfdff 100%);
  border-radius: 14px; border: 1px solid var(--panel-border);
  box-shadow: 0 10px 24px rgba(38, 88, 154, 0.08);
  border-left: 5px solid var(--accent);
  padding: 15px 16px; display: flex; align-items: center; justify-content: space-between;
}
.kpi-title { color: var(--muted); text-transform: uppercase; letter-spacing: .055em; font-size: 11px; font-weight: 800; }
.kpi-value { color: var(--ink); font-size: 30px; font-weight: 800; line-height: 1.08; margin-top: 6px; }
.kpi-note { color: var(--muted); font-size: 11px; margin-top: 5px; }
.kpi-icon { color: var(--accent); opacity: .95; font-size: 28px; margin-left: 12px; }

.content .box {
  border-top: 3px solid var(--primary); border-radius: 12px;
  box-shadow: 0 10px 24px rgba(38, 88, 154, 0.08);
  border: 1px solid var(--panel-border);
}
.content .box > .box-header { padding: 14px 14px 7px; }
.content .box > .box-header .box-title { color: var(--ink); font-weight: 750; font-size: 15px; }
.content .box > .box-body { padding: 12px 14px 15px; }

.automatic-summary { font-size: 14px; line-height: 1.65; color: #1d3557; }
.automatic-summary strong { color: var(--primary-dark); }

.method-card { background: linear-gradient(180deg, #ffffff 0%, #f7fbff 100%); border: 1px solid #dfeaf8; border-radius: 11px; padding: 15px; margin-bottom: 12px; }
.method-card h4 { margin-top: 0; color: var(--ink); font-weight: 750; }
.method-card li { margin-bottom: 7px; }

.privacy-note { background: linear-gradient(180deg, #fff9ee 0%, #fff5df 100%); border-left: 4px solid var(--amber); padding: 11px 13px; border-radius: 8px; }
.status-pill { display: inline-block; padding: 3px 9px; color: #fff; border-radius: 99px; font-size: 11px; font-weight: 800; }
.small-muted { color: var(--muted); font-size: 12px; }

.btn-primary { background: linear-gradient(135deg, var(--primary), var(--primary-dark)); border-color: var(--primary); color: #fff; }
.btn-primary:hover { background: linear-gradient(135deg, var(--primary-dark), var(--primary)); border-color: var(--primary-dark); color: #fff; }

.nav-tabs-custom > .nav-tabs > li.active { border-top-color: var(--primary); }
#shiny-tab-analysis .recalculating { opacity: 1 !important; transition: none !important; }

@media (max-width: 1100px) { .kpi-grid { grid-template-columns: repeat(2, minmax(0, 1fr)); } }
@media (max-width: 680px) { .kpi-grid { grid-template-columns: 1fr; } .data-banner { display:block; } }
"

ui <- shinydashboard::dashboardPage(
  skin = "blue",
  shinydashboard::dashboardHeader(
    title = shiny::tags$div(
      style = "display: flex; align-items: center; justify-content: center; height: 100%;",
      shiny::tags$img(
        src = "logo.png",
        alt = "ALIVE logo",
        height = "34px",
        style = "margin-right: 10px;"
      ),
      shiny::tags$span(
        "ALIVE",
        style = "font-weight: bold; font-size: 20px; color: white;"
      )
    ),
    titleWidth = 250
  ),
  shinydashboard::dashboardSidebar(
    width = 250,
    shiny::tags$div(
      style = "padding: 10px 12px 6px;",
      shiny::selectInput(
        "language_selector",
        label = shiny::tags$span(style = "color: #eaf5ff; font-size: 12px; font-weight: 700;", "Idioma / Language"),
        choices = c("Português (PT)" = "pt", "English (EN)" = "en"),
        selected = "pt",
        width = "100%"
      )
    ),
    shinydashboard::sidebarMenuOutput("main_sidebar_menu"),
    shiny::uiOutput("global_filters_title"),
    shiny::uiOutput("date_filter"),
    shiny::uiOutput("province_filter"),
    shiny::uiOutput("district_filter"),
    shiny::uiOutput("facility_filter"),
    shiny::uiOutput("disease_filter"),
    shiny::uiOutput("consulta"),
    shiny::uiOutput("consultation_type_filter"),
    shiny::uiOutput("age_filter"),
    shiny::uiOutput("sex_filter"),
    shiny::selectInput(
      "period_unit", "Periodicidade",
      choices = c("Diária" = "day", "Semanal" = "week", "Mensal" = "month"),
      selected = "week"
    ),
    shiny::uiOutput("reset_filters_button"),
    shiny::br(), shiny::br()
  ),
  shinydashboard::dashboardBody(
    shiny::tags$head(
      shiny::tags$style(shiny::HTML(app_css)),
      shiny::tags$meta(name = "viewport", content = "width=device-width, initial-scale=1")
    ),
    shiny::uiOutput("app_header_texts"),
    shiny::uiOutput("data_banner"),
    shinydashboard::tabItems(
      
      # -----------------------------------------------------------------------
      # TAB 1: OVERVIEW
      # -----------------------------------------------------------------------
      shinydashboard::tabItem(
        tabName = "overview",
        shiny::div(
          class = "kpi-grid",
          kpi_placeholder("kpi_records"),
          kpi_placeholder("kpi_facilities"),
          kpi_placeholder("kpi_diseases"),
          kpi_placeholder("kpi_quality")
        ),
        shiny::div(
          class = "kpi-grid",
          kpi_placeholder("kpi_completeness"),
          kpi_placeholder("kpi_timeliness"),
          kpi_placeholder("kpi_duplicates"),
          kpi_placeholder("kpi_alerts")
        ),
        shiny::fluidRow(
          panel_box(shiny::uiOutput("automatic_summary"), title = "Automated Insights Summary", width = 4),
          panel_box(plotly::plotlyOutput("overall_trend", height = 285), title = "Data Entry Trend", width = 8)
        ),
        shiny::fluidRow(
          panel_box(plotly::plotlyOutput("delay_histogram", height = 400), title = "Data Entry Delay Distribution by Facility", width = 12)
        ),
        shiny::fluidRow(
          panel_box(plotly::plotlyOutput("disease_pareto", height = 330), title = "Case Reporting by Health Facility", width = 6),
          panel_box(leaflet::leafletOutput("disease_map", height = 330), title = "Geographic Map of Reported Cases", width = 6)
        )
      ),
      
      # -----------------------------------------------------------------------
      # TAB 2: DATA ENTRY
      # -----------------------------------------------------------------------
      shinydashboard::tabItem(
        tabName = "entry",
        shiny::fluidRow(
          panel_box(plotly::plotlyOutput("entry_heatmap", height = 430), title = "Data Entry Heatmap", width = 12)
        ),
        shiny::fluidRow(
          panel_box(DT::DTOutput("facility_monitor_table"), title = "Reporting Coverage & Activity by Health Facility", width = 12)
        )
      ),
      
      # -----------------------------------------------------------------------
      # TAB 3: COMPLETENESS
      # -----------------------------------------------------------------------
      shinydashboard::tabItem(
        tabName = "completeness",
        shiny::fluidRow(
          panel_box(plotly::plotlyOutput("field_completeness_plot", height = 450), title = "Completeness by Field", width = 5),
          panel_box(plotly::plotlyOutput("completeness_heatmap", height = 450), title = "Completeness Heatmap (Field vs Facility)", width = 7)
        ),
        shiny::fluidRow(
          panel_box(DT::DTOutput("field_completeness_table"), title = "Completeness Details", width = 12)
        )
      ),
      
      # -----------------------------------------------------------------------
      # TAB 4: QUALITY
      # -----------------------------------------------------------------------
      shinydashboard::tabItem(
        tabName = "quality",
        shiny::fluidRow(
          panel_box(plotly::plotlyOutput("quality_radar", height = 400), title = "Data Quality Dimensions", width = 5),
          panel_box(plotly::plotlyOutput("issue_plot", height = 400), title = "Detected Anomalies & Data Issues", width = 7)
        )
      ),
      
      # -----------------------------------------------------------------------
      # TAB 5: DISEASES
      # -----------------------------------------------------------------------
      shinydashboard::tabItem(
        tabName = "diseases",
        shiny::tabsetPanel(
          id = "diseases_subtabs",
          type = "tabs",
          
          # Aba 1: Dados Gerais de Doenças
          shiny::tabPanel(
            title = "Dados Gerais",
            icon = shiny::icon("virus"),
            shiny::br(),
            shiny::fluidRow(
              panel_box(
                plotly::plotlyOutput("disease_trend", height = 390), 
                title = "Disease / Diagnosis Trend", 
                width = 7
              ),
              panel_box(
                plotly::plotlyOutput("disease_age_sex", height = 390), 
                title = "Demographic Distribution (Age & Sex)", 
                width = 5
              )
            ),
            shiny::fluidRow(
              panel_box(
                DT::DTOutput("disease_table"), 
                title = "Notification Records Summary", 
                width = 12
              )
            )
          ),
          
          # Aba 2: Dados Específicos Badplus (RDTR - BED)
          shiny::tabPanel(
            title = "Badplus Data",
            icon = shiny::icon("chart-line"),
            shiny::br(),
            
            # Seletores de Filtro
            shiny::fluidRow(
              shiny::column(
                width = 4,
                shiny::selectInput(
                  inputId = "badplus_org_select",
                  label = "Selecione a Health Facility:",
                  choices = c("Todas as Health Facilities" = "ALL"),
                  selected = "ALL"
                )
              ),
              shiny::column(
                width = 4,
                shiny::selectInput(
                  inputId = "badplus_disease_select",
                  label = "Selecione a Doença / Diagnóstico:",
                  choices = c(
                    "Diarreia", 
                    "Cólera", 
                    "PFA", 
                    "Malaria", 
                    "Sarampo", 
                    "Sindrome Febril", 
                    "IRAS"
                  ),
                  selected = "Malaria"
                )
              ),
              shiny::column(
                width = 4,
                shiny::radioButtons(
                  inputId = "badplus_type",
                  label = "Métrica Visualizada:",
                  choices = c("Casos Notificados" = "Casos", "Óbitos Registados" = "Óbitos"),
                  selected = "Casos",
                  inline = TRUE
                )
              )
            ),
            
            # Primeira Linha: Tendência Temporal + Comparação por Doença
            shiny::fluidRow(
              panel_box(
                plotly::plotlyOutput("badplus_trend", height = 380), 
                title = "Tendência Temporal da Doença Selecionada", 
                width = 7
              ),
              panel_box(
                plotly::plotlyOutput("badplus_summary_bar", height = 380), 
                title = "Comparação Geral Entre Doenças", 
                width = 5
              )
            ),
            
            # Segunda Linha: Distribuição de Doenças por Health Facility
            shiny::fluidRow(
              panel_box(
                plotly::plotlyOutput("badplus_facility_bar", height = 400), 
                title = "Distribuição por Health Facility", 
                width = 12
              )
            ),
            
            # Terceira Linha: Tabela Resumo
            shiny::fluidRow(
              panel_box(
                DT::DTOutput("badplus_table"), 
                title = "Resumo dos Registos BADPLUS", 
                width = 12
              )
            )
          ) # Fim do tabPanel("Badplus Data")
        ) # Fim do tabsetPanel
      ), # Fim do tabItem("diseases")
      
      # -----------------------------------------------------------------------
      # TAB 6: ANALYSIS & ALERTS
      # -----------------------------------------------------------------------
      shinydashboard::tabItem(
        tabName = "analysis",
        shiny::fluidRow(
          shinydashboard::tabBox(
            width = 12, 
            id = "analysis_tab", 
            title = "Epidemiological Surveillance & Service Monitoring",
            
            # Sub-Tab 1: Hypertension Alerts
            shiny::tabPanel(
              "Hypertension Alerts",
              shiny::div(
                class = "kpi-grid",
                kpi_placeholder("kpi_htn_deaths_week"),      # Weekly Deaths
                kpi_placeholder("kpi_htn_admissions_week"),  # Weekly Admissions
                kpi_placeholder("kpi_htn_deaths_month"),     # Monthly Deaths Aggregate
                kpi_placeholder("kpi_htn_deaths_3months")    # 3-Month Cumulative Deaths
              ),
              plotly::plotlyOutput("htn_alert_plot", height = 380),
              shiny::h4("Periods Triggering Alert Status (≥ 1 Death or Admission)"),
              DT::DTOutput("htn_alert_table")
            ),
            
            # Sub-Tab 2: Monthly & Quarterly Summary
            shiny::tabPanel(
              "Monthly & Quarterly Summary",
              DT::DTOutput("htn_summary_table")
            ),
            
            # Sub-Tab 3: Measles
            shiny::tabPanel(
              "Measles — WHO Standards",
              shiny::uiOutput("who_definitions_ui"),
              shiny::h4("Weekly Measles Alert Threshold (≥ 1 Case / Week)"),
              plotly::plotlyOutput("measles_alert_plot", height = 380),
              shiny::h4("Measles Outbreak Declaration Threshold (≥ 5 Suspected Cases / 30 Days in Same Area)"),
              plotly::plotlyOutput("measles_outbreak_plot", height = 380),
              shiny::h4("Measles Alert Log Table (≥ 1 Case / Week)"),
              DT::DTOutput("measles_alert_table")
            ),
            
            # Sub-Tab 4: Diarrhea
            shiny::tabPanel(
              "Diarreia — Normas da OMS",
              shiny::uiOutput("who_diarrhea_definitions_ui"),
              
              shiny::h4("Limiar de Alerta Semanal para Diarreia (Aumento Anormal de Casos vs. Média Histórica)"),
              plotly::plotlyOutput("diarrhea_alert_plot", height = 380),
              
              shiny::h4("Limiar de Surto / Cólera Suspeita OMS (≥ 1 Caso Confirmado ou Rápido Aumento Semanal)"),
              plotly::plotlyOutput("diarrhea_outbreak_plot", height = 380),
              
              shiny::h4("Tabela de Log de Alertas de Diarreia por Unidade Sanitária"),
              DT::DTOutput("diarrhea_alert_table")
            ),
            
            # Sub-Tab 5: Diabetes
            shiny::tabPanel(
              "Diabetes Mellitus (Tipo I & II) — Normas da OMS",
              shiny::uiOutput("who_diabetes_definitions_ui"),
              
              shiny::h4("Tendência Mensal de Novos Diagnósticos por Tipo (OMS)"),
              plotly::plotlyOutput("diabetes_incidence_plot", height = 380),
              
              shiny::h4("Distribuição do Controlo Glicémico e Casos Descompensados (HbA1c / Glicemia)"),
              plotly::plotlyOutput("diabetes_control_plot", height = 380),
              
              shiny::h4("Tabela de Monitoria de Doentes com Diabetes por Unidade Sanitária"),
              DT::DTOutput("diabetes_monitoring_table")
            )
          )
        )
      ),
      
      # -----------------------------------------------------------------------
      # TAB 7: ALERTS SUMMARY
      # -----------------------------------------------------------------------
      shinydashboard::tabItem(
        tabName = "alerts",
        shiny::div(
          class = "kpi-grid",
          kpi_placeholder("kpi_high_alerts"),
          kpi_placeholder("kpi_medium_alerts"),
          kpi_placeholder("kpi_silent_facilities"),
          kpi_placeholder("kpi_anomalies")
        ),
        shiny::fluidRow(
          panel_box(DT::DTOutput("facility_alert_table"), title = "Intervention Priority by Health Facility", width = 12)
        ),
        shiny::fluidRow(
          panel_box(DT::DTOutput("record_alert_table"), title = "Flagged Records with Issues (De-identified)", width = 12)
        )
      ),
      
      # -----------------------------------------------------------------------
      # TAB 8: DATA & METHODOLOGY
      # -----------------------------------------------------------------------
      shinydashboard::tabItem(
        tabName = "data",
        shiny::fluidRow(
          panel_box(
            shiny::div(
              class = "privacy-note", 
              shiny::icon("user-shield"),
              " Patient names, surnames, and occupations are stripped from exported tables and files."
            ),
            shiny::br(),
            shiny::downloadButton("download_csv", "Export Filtered Dataset (CSV)", class = "btn-primary"),
            shiny::downloadButton("download_alerts", "Export Active Alerts (CSV)"),
            shiny::downloadButton("download_excel", "Export Full Dashboard Report (Excel)"),
            title = "Data Security & Export Options", width = 12
          )
        ),
        shiny::fluidRow(
          panel_box(DT::DTOutput("safe_data_table"), title = "Filtered & De-identified Master Records", width = 12)
        ),
        shiny::fluidRow(
          panel_box(
            shiny::div(
              class = "method-card",
              shiny::h4("Data Quality Metrics Methodology"),
              shiny::tags$ul(
                shiny::tags$li(shiny::strong("Field Completeness:"), " Percentage of mandatory fields filled per record."),
                shiny::tags$li(shiny::strong("Reporting Coverage:"), " Percentage of expected facility-period combinations meeting minimal record counts."),
                shiny::tags$li(shiny::strong("Timeliness:"), " Proportion of records entered within the maximum specified delay window."),
                shiny::tags$li(shiny::strong("Uniqueness:"), " Absence of duplicate record IDs and clinical duplication signals."),
                shiny::tags$li(shiny::strong("Validity:"), " Logical date validation, active coding values, and complete event identifiers."),
                shiny::tags$li(shiny::strong("Consistency:"), " Logical agreement across patient status, treatment regimen, and outcomes."),
                shiny::tags$li(shiny::strong("Global Quality Index:"), " 35% Completeness + 20% Timeliness + 15% Uniqueness + 15% Validity + 15% Consistency.")
              )
            ),
            shiny::div(
              class = "method-card",
              shiny::h4("Operational Guidance & Limitations"),
              shiny::p(
                "This dashboard evaluates individual surveillance events. A period with no recorded events could indicate zero cases or zero reporting. Coverage figures serve as operational signals that must be validated against master facility lists and zero-reporting forms."
              )
            ),
            title = "Surveillance Methodology & Operational Context", width = 12
          )
        )
      ) # Fim do tabItem("data")
      
    ) # Fim do tabItems
  ) # Fim do dashboardBody
) # Fim do dashboardPage