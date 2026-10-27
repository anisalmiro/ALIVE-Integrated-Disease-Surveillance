# ============================================================
# CONFIGURAÇÃO DO PAINEL DE MONITORIA DE DADOS
# Projecto ALIVE — WP2 (DHIS2 Tracker)
# ============================================================

# Convenção centralizada para caminhos de dados. Mantém a compatibilidade com
# as variáveis de ambiente existentes e facilita a manutenção em produção.
resolve_data_path <- function(env_var, default_file) {
  value <- Sys.getenv(env_var, unset = default_file)
  if (is.null(value) || length(value) == 0 || !nzchar(value)) {
    value <- default_file
  }
  value
}

# Caminho padrão. Também pode ser definido antes de iniciar o painel:
# Sys.setenv(MONITORIA_DATA_PATH = "caminho/para/o/ficheiro.csv")
DATA_PATH <- resolve_data_path(
  env_var = "MONITORIA_DATA_PATH",
  default_file = file.path("data-raw", "dados_processados.csv")
)

AGREGADOS_DATA_PATH <- resolve_data_path(
  env_var = "AGREGADOS_DATA_PATH",
  default_file = file.path("data-raw", "AGREGADOS.csv")
)

SARA_GEO_DATA_PATH <- resolve_data_path(
  env_var = "SARA_GEO_DATA_PATH",
  default_file = file.path("data-raw", "Coordenadas_Geograficas_SARA.csv")
)


# Basemap do módulo de risco (Leaflet usa raster tiles)
CARTO_RASTER_TEMPLATE <- Sys.getenv(
  "CARTO_RASTER_TEMPLATE",
  unset = "https://basemaps.cartocdn.com/rastertiles/voyager/{z}/{x}/{y}.png"
)
CARTO_API_KEY <- Sys.getenv(
  "CARTO_API_KEY",
  unset = "cb1_3v96_1_ee046544cb05ea2f1d83c753"
)


# Fuso horário usado para interpretar as datas do DHIS2.
APP_TIMEZONE <- "Africa/Maputo"

# Mapeamento entre nomes legíveis e os UIDs/colunas da exportação DHIS2.
# Se os UIDs mudarem, altere apenas o valor à direita. O formulário Word não
# mostra os UIDs; por isso, confirme no dicionário de metadados do DHIS2 os dois
# campos binários de referência comunitária e população vulnerável antes do uso
# em produção.
FIELD_MAP <- c(
  record_id             = "psi",
  program_stage_id      = "ps",
  event_date            = "eventdate",
  stored_by             = "storedby",
  created_by            = "createdbydisplayname",
  updated_by            = "lastupdatedbydisplayname",
  last_updated          = "lastupdated",
  scheduled_date        = "scheduleddate",
  enrollment_date       = "enrollmentdate",
  incident_date         = "incidentdate",
  tracked_entity_id     = "tei",
  enrollment_id         = "pi",
  geometry              = "geometry",
  longitude             = "longitude",
  latitude              = "latitude",
  facility              = "ouname",
  organisation_hierarchy = "ounamehierarchy",
  facility_code         = "oucode",
  program_status        = "programstatus",
  event_status          = "eventstatus",
  facility_id           = "ou",
  fever                 = "oNN1FVNaV4r.eMVytXtuMWh",
  requested_test        = "oNN1FVNaV4r.mtkN9C1BJX4",
  first_name            = "ENRjVGxVL6l",
  age_group             = "MZeTF32uOUD",
  diagnosis             = "oNN1FVNaV4r.nRdPCxj7d9z",
  occupation            = "Q1NM5BSIxl0",
  surname               = "sB1IHYu2xQT",
  group_code            = "ANXuo6saopx",
  care_action           = "oNN1FVNaV4r.F4XfAlCCrf2",
  consultation_type     = "oNN1FVNaV4r.kP29shPSHl8",
  community_actor_group = "VCtm2pySeEV",
  vulnerable_group      = "oNN1FVNaV4r.BsKcBDC0kSq",
  sex                   = "CklPZdOd6H1",
  other_test            = "oNN1FVNaV4r.rLDcMERXNrS",
  received_treatment    = "oNN1FVNaV4r.sh6WSMuKbof",
  test_result           = "oNN1FVNaV4r.exTgdok33ph",
  test_status           = "oNN1FVNaV4r.teO9EjrYeF6",
  outcome               = "oNN1FVNaV4r.HG5ltiCtn1D",
  vulnerable_population = "oNN1FVNaV4r.vK9PPhCrA6Q",
  community_referral    = "oNN1FVNaV4r.M9GNJpUsAeT",
  other_diagnosis       = "oNN1FVNaV4r.vrJV8duHFUy"
)

# Variáveis opcionais para aprofundar as definições de caso, continuidade e
# acesso. Preencha os UIDs no ficheiro ou defina as variáveis de ambiente. Se
# não forem configuradas, o painel continua a funcionar e mostra a lacuna.
OPTIONAL_FIELD_MAP <- c(
  rash_generalized = Sys.getenv("ALIVE_UID_RASH", unset = ""),
  cough = Sys.getenv("ALIVE_UID_COUGH", unset = ""),
  coryza = Sys.getenv("ALIVE_UID_CORYZA", unset = ""),
  conjunctivitis = Sys.getenv("ALIVE_UID_CONJUNCTIVITIS", unset = ""),
  clinician_suspects_measles = Sys.getenv("ALIVE_UID_MEASLES_SUSPECT", unset = ""),
  rash_onset_date = Sys.getenv("ALIVE_UID_RASH_ONSET", unset = ""),
  measles_igm_result = Sys.getenv("ALIVE_UID_MEASLES_IGM", unset = ""),
  measles_pcr_result = Sys.getenv("ALIVE_UID_MEASLES_PCR", unset = ""),
  epidemiological_link = Sys.getenv("ALIVE_UID_EPI_LINK", unset = ""),
  acute_watery_diarrhoea = Sys.getenv("ALIVE_UID_AWD", unset = ""),
  severe_dehydration = Sys.getenv("ALIVE_UID_SEVERE_DEHYDRATION", unset = ""),
  symptom_onset_date = Sys.getenv("ALIVE_UID_SYMPTOM_ONSET", unset = "")
)

# Campos usados no indicador principal de completude.
# Identificadores e nomes próprios não entram no cálculo exibido.
REQUIRED_FIELDS <- c(
  "event_date", "facility", "district", "age_group", "sex",
  "consultation_type", "requested_test", "test_status",
  "test_result", "diagnosis", "received_treatment", "outcome"
)

FIELD_LABELS <- c(
  event_date = "Data do registo",
  facility = "Unidade sanitária",
  province = "Província",
  district = "Distrito",
  age_group = "Faixa etária",
  sex = "Sexo",
  consultation_type = "Tipo de consulta",
  requested_test = "Exame/teste solicitado",
  other_test = "Outro exame/teste",
  test_status = "Estado do exame/teste",
  test_result = "Resultado",
  diagnosis = "Diagnóstico",
  other_diagnosis = "Outro diagnóstico",
  received_treatment = "Recebeu tratamento",
  care_action = "Conduta/seguimento",
  outcome = "Desfecho",
  community_referral = "Referido pela comunidade",
  community_actor_group = "Actor/grupo comunitário",
  vulnerable_population = "População vulnerável",
  vulnerable_group = "Grupo vulnerável",
  event_status = "Estado do evento",
  program_status = "Estado do programa"
)

OPTIONAL_FIELD_LABELS <- c(
  rash_generalized = "Erupção maculopapular generalizada",
  cough = "Tosse",
  coryza = "Coriza",
  conjunctivitis = "Conjuntivite",
  clinician_suspects_measles = "Clínico suspeita de sarampo",
  rash_onset_date = "Data de início da erupção",
  measles_igm_result = "Resultado IgM para sarampo",
  measles_pcr_result = "Resultado PCR para sarampo",
  epidemiological_link = "Ligação epidemiológica",
  acute_watery_diarrhoea = "Diarreia aquosa aguda",
  severe_dehydration = "Desidratação grave",
  symptom_onset_date = "Data de início dos sintomas"
)

FIELD_LABELS <- c(FIELD_LABELS, OPTIONAL_FIELD_LABELS)

VALID_AGE_GROUPS <- c(
  "0-4 anos", "5-14 anos", "15-24 anos", "25-54 anos", "55+ anos"
)

VALID_SEX <- c("Feminino", "Masculino")

# Pesos do índice composto de qualidade. A soma deve ser igual a 1.
QUALITY_WEIGHTS <- c(
  completeness = 0.35,
  timeliness = 0.20,
  uniqueness = 0.15,
  validity = 0.15,
  consistency = 0.15
)

QUALITY_BANDS <- c(critical = 60, attention = 80, good = 90)

# Parâmetros iniciais do módulo de análise ALIVE. Todos podem ser alterados na
# interface durante a análise.
ALIVE_ANALYSIS_DEFAULTS <- list(
  baseline_periods = 8L,
  minimum_history = 4L,
  standard_deviations = 2,
  mad_multiplier = 3,
  percentile = 0.95,
  growth_percent = 50,
  minimum_cases = 2L,
  continuity_days = 60L
)

WHO_REFERENCE_URLS <- c(
  measles = "https://www.who.int/emergencies/outbreak-toolkit/disease-outbreak-toolboxes/measles-outbreak-toolbox",
  cholera = "https://www.who.int/emergencies/outbreak-toolkit/disease-outbreak-toolboxes/cholera-outbreak-toolbox",
  malaria_thresholds = "https://iris.who.int/bitstreams/035f2b17-fb2b-4aee-9aca-e2591405bfa7/download"
)

# Colunas seguras para visualização/exportação. Nomes, apelidos e profissão
# são deliberadamente excluídos para proteger os utentes.
SAFE_EXPORT_COLUMNS <- c(
  "record_id", "event_date", "last_updated", "province", "district",
  "facility", "facility_code", "age_group", "sex", "fever",
  "consultation_type", "requested_test", "test_status", "test_result",
  "diagnosis_final", "received_treatment", "care_action", "outcome",
  "event_status", "program_status", "record_completeness",
  "delay_days", "duplicate_id", "potential_duplicate", "issue_count",
  "severity", "issue_detail"
)

# A leitura de ficheiros CSV é centralizada em funções de apoio para evitar
# efeitos laterais e facilitar a manutenção futura do painel.
