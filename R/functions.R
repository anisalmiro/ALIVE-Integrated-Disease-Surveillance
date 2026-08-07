# ============================================================
# FUNÇÕES DE PREPARAÇÃO, QUALIDADE E AGREGAÇÃO
# ============================================================

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) y else x
}

has_value <- function(x) {
  !is.na(x) & nzchar(trimws(as.character(x)))
}

safe_mean <- function(x, default = NA_real_) {
  if (length(x) == 0 || all(is.na(x))) return(default)
  mean(x, na.rm = TRUE)
}

safe_median <- function(x, default = NA_real_) {
  if (length(x) == 0 || all(is.na(x))) return(default)
  stats::median(x, na.rm = TRUE)
}

safe_pct <- function(x, default = 0) {
  if (length(x) == 0 || all(is.na(x))) return(default)
  100 * mean(x, na.rm = TRUE)
}

fmt_number <- function(x, digits = 0) {
  ifelse(
    is.na(x), "—",
    scales::number(x, accuracy = 10^(-digits), big.mark = " ", decimal.mark = ",")
  )
}

fmt_pct <- function(x, digits = 1) {
  ifelse(
    is.na(x), "—",
    paste0(format(round(x, digits), nsmall = digits, decimal.mark = ","), "%")
  )
}

quality_band <- function(score) {
  dplyr::case_when(
    is.na(score) ~ "Sem dados",
    score < QUALITY_BANDS[["critical"]] ~ "Crítico",
    score < QUALITY_BANDS[["attention"]] ~ "Atenção",
    score < QUALITY_BANDS[["good"]] ~ "Bom",
    TRUE ~ "Excelente"
  )
}

status_colour <- function(status) {
  dplyr::case_when(
    status == "Excelente" ~ "#0f9d72",
    status == "Bom" ~ "#38a169",
    status == "Atenção" ~ "#e6a700",
    status == "Crítico" ~ "#d64545",
    TRUE ~ "#64748b"
  )
}

clean_character <- function(x) {
  x <- trimws(as.character(x))
  x[x %in% c("", "NA", "N/A", "NULL", "null", "-")] <- NA_character_
  x
}

normalise_text <- function(x) {
  x <- clean_character(x)
  transliterated <- suppressWarnings(iconv(x, from = "", to = "ASCII//TRANSLIT"))
  transliterated[is.na(transliterated) & !is.na(x)] <- x[is.na(transliterated) & !is.na(x)]
  stringr::str_to_lower(transliterated)
}

is_yes <- function(x) {
  result <- normalise_text(x) %in% c(
    "1", "true", "sim", "yes", "positivo", "positive", "detectado", "detected"
  )
  result[is.na(result)] <- FALSE
  result
}

is_positive_result <- function(x) {
  result <- stringr::str_detect(
    normalise_text(x),
    "positiv|detectad|reagent|confirmad"
  )
  result[is.na(result)] <- FALSE
  result
}

parse_datetime_flexible <- function(x) {
  x <- clean_character(x)
  suppressWarnings(
    lubridate::parse_date_time(
      x,
      orders = c("Ymd HMS", "Ymd HM", "Ymd", "dmY HMS", "dmY HM", "dmY"),
      tz = APP_TIMEZONE,
      truncated = 3,
      quiet = TRUE
    )
  )
}

read_monitor_data <- function(path) {
  if (!file.exists(path)) {
    stop(
      paste0(
        "Ficheiro não encontrado: ", path,
        ". Coloque o CSV em data-raw/dados_processados.csv ou carregue-o no painel."
      ),
      call. = FALSE
    )
  }

  readr::read_csv(
    path,
    col_types = readr::cols(.default = readr::col_character()),
    na = c("", "NA", "N/A", "NULL", "null"),
    trim_ws = TRUE,
    show_col_types = FALSE,
    progress = FALSE,
    name_repair = "unique"
  )
}

map_to_canonical <- function(raw) {
  x <- tibble::as_tibble(raw)

  active_field_map <- c(FIELD_MAP, OPTIONAL_FIELD_MAP)

  for (canonical_name in names(active_field_map)) {
    source_name <- unname(active_field_map[[canonical_name]])
    if (nzchar(source_name) && source_name %in% names(x)) {
      x[[canonical_name]] <- x[[source_name]]
    } else {
      x[[canonical_name]] <- NA_character_
    }
  }

  canonical <- x[, names(active_field_map), drop = FALSE]
  canonical[] <- lapply(canonical, clean_character)

  canonical$event_datetime <- parse_datetime_flexible(canonical$event_date)
  canonical$event_date <- as.Date(canonical$event_datetime, tz = APP_TIMEZONE)
  canonical$last_updated <- parse_datetime_flexible(canonical$last_updated)
  canonical$scheduled_date <- as.Date(parse_datetime_flexible(canonical$scheduled_date))
  canonical$enrollment_date <- as.Date(parse_datetime_flexible(canonical$enrollment_date))
  canonical$incident_date <- as.Date(parse_datetime_flexible(canonical$incident_date))
  canonical$rash_onset_date <- as.Date(parse_datetime_flexible(canonical$rash_onset_date))
  canonical$symptom_onset_date <- as.Date(parse_datetime_flexible(canonical$symptom_onset_date))
  canonical$longitude <- suppressWarnings(readr::parse_number(canonical$longitude))
  canonical$latitude <- suppressWarnings(readr::parse_number(canonical$latitude))

  hierarchy <- stringr::str_split_fixed(
    canonical$organisation_hierarchy %||% NA_character_,
    "\\s*/\\s*",
    n = 6
  )
  canonical$country <- clean_character(hierarchy[, 1])
  canonical$province <- clean_character(hierarchy[, 2])
  canonical$district <- clean_character(hierarchy[, 3])

  canonical$sex <- dplyr::recode(
    stringr::str_to_lower(canonical$sex),
    "female" = "Feminino",
    "feminino" = "Feminino",
    "male" = "Masculino",
    "masculino" = "Masculino",
    .default = canonical$sex
  )

  binary_fields <- c(
    "fever", "received_treatment", "vulnerable_population", "community_referral",
    "rash_generalized", "cough", "coryza", "conjunctivitis",
    "clinician_suspects_measles", "epidemiological_link",
    "acute_watery_diarrhoea", "severe_dehydration"
  )
  for (field in binary_fields) {
    canonical[[field]] <- dplyr::case_when(
      canonical[[field]] %in% c("1", "TRUE", "true", "Sim", "SIM") ~ "Sim",
      canonical[[field]] %in% c("0", "FALSE", "false", "Não", "Nao", "NÃO") ~ "Não",
      TRUE ~ canonical[[field]]
    )
  }

  canonical$diagnosis_final <- dplyr::case_when(
    !has_value(canonical$diagnosis) & has_value(canonical$other_diagnosis) ~ canonical$other_diagnosis,
    stringr::str_to_lower(canonical$diagnosis) == "outro" & has_value(canonical$other_diagnosis) ~
      paste0("Outro: ", canonical$other_diagnosis),
    TRUE ~ canonical$diagnosis
  )

  canonical$valid_coordinates <-
    is.finite(canonical$longitude) & is.finite(canonical$latitude) &
    canonical$longitude != 0 & canonical$latitude != 0 &
    dplyr::between(canonical$longitude, -180, 180) &
    dplyr::between(canonical$latitude, -90, 90)

  mention_text <- paste(
    canonical$diagnosis, canonical$other_diagnosis, canonical$requested_test,
    canonical$other_test
  )
  mention_text <- normalise_text(mention_text)

  canonical$measles_mentioned <- stringr::str_detect(mention_text, "sarampo|measles")
  canonical$measles_mentioned[is.na(canonical$measles_mentioned)] <- FALSE
  canonical$measles_clinician_suspected <-
    is_yes(canonical$clinician_suspects_measles) | canonical$measles_mentioned
  canonical$measles_who_suspected <-
    canonical$measles_clinician_suspected |
    (canonical$fever == "Sim" & canonical$rash_generalized == "Sim")
  canonical$measles_who_suspected[is.na(canonical$measles_who_suspected)] <- FALSE
  canonical$measles_who_clinical <-
    canonical$measles_clinician_suspected |
    (
      canonical$fever == "Sim" & canonical$rash_generalized == "Sim" &
      (
        canonical$cough == "Sim" | canonical$coryza == "Sim" |
        canonical$conjunctivitis == "Sim"
      )
    )
  canonical$measles_who_clinical[is.na(canonical$measles_who_clinical)] <- FALSE
  measles_test <- stringr::str_detect(normalise_text(canonical$requested_test), "sarampo|measles")
  measles_test[is.na(measles_test)] <- FALSE
  canonical$measles_lab_confirmed <-
    canonical$measles_who_suspected & (
      is_positive_result(canonical$measles_igm_result) |
      is_positive_result(canonical$measles_pcr_result) |
      (measles_test & canonical$test_result == "Positivo")
    )
  canonical$measles_lab_confirmed[is.na(canonical$measles_lab_confirmed)] <- FALSE
  canonical$measles_epi_linked <-
    canonical$measles_who_clinical & (
      is_yes(canonical$epidemiological_link) |
      stringr::str_detect(normalise_text(canonical$test_result), "ligacao epidemiologica")
    )
  canonical$measles_epi_linked[is.na(canonical$measles_epi_linked)] <- FALSE
  canonical$measles_case_class <- dplyr::case_when(
    canonical$measles_lab_confirmed ~ "Confirmado laboratorialmente",
    canonical$measles_epi_linked ~ "Ligado epidemiologicamente",
    canonical$measles_who_clinical ~ "Clínico/compatível",
    canonical$measles_who_suspected ~ "Suspeito",
    TRUE ~ "Não classificado como sarampo"
  )
  canonical$measles_onset_date <- dplyr::coalesce(
    canonical$rash_onset_date, canonical$symptom_onset_date, canonical$event_date
  )
  canonical$measles_onset_is_proxy <-
    is.na(canonical$rash_onset_date) & is.na(canonical$symptom_onset_date)

  canonical$cholera_mentioned <- stringr::str_detect(mention_text, "colera|cholera")
  canonical$cholera_mentioned[is.na(canonical$cholera_mentioned)] <- FALSE
  canonical$cholera_rdt_tested <- stringr::str_detect(
    normalise_text(canonical$requested_test),
    "coltdr|rdt.*colera|tdr.*colera|teste rapido.*colera"
  )
  canonical$cholera_rdt_tested[is.na(canonical$cholera_rdt_tested)] <- FALSE
  canonical$cholera_rdt_positive <-
    canonical$cholera_rdt_tested & canonical$test_result == "Positivo"
  canonical$cholera_rdt_positive[is.na(canonical$cholera_rdt_positive)] <- FALSE
  canonical$cholera_lab_tested <- stringr::str_detect(
    normalise_text(canonical$requested_test),
    "cultura.*colera|pcr.*colera"
  )
  canonical$cholera_lab_tested[is.na(canonical$cholera_lab_tested)] <- FALSE
  canonical$cholera_lab_confirmed <-
    canonical$cholera_lab_tested & canonical$test_result == "Positivo"
  canonical$cholera_lab_confirmed[is.na(canonical$cholera_lab_confirmed)] <- FALSE
  canonical$cholera_suspected_proxy <-
    canonical$cholera_mentioned |
    (canonical$acute_watery_diarrhoea == "Sim" & canonical$severe_dehydration == "Sim")
  canonical$cholera_suspected_proxy[is.na(canonical$cholera_suspected_proxy)] <- FALSE

  canonical$service_domain <- dplyr::case_when(
    stringr::str_detect(normalise_text(canonical$diagnosis_final),
                        "colera|diarr|disenter|sarampo|malaria|dengue|chik|zika|tubercul|hiv|ira") ~
      "Doenças transmissíveis",
    stringr::str_detect(normalise_text(canonical$diagnosis_final),
                        "hipertens|diabetes|asma|cardi|renal") ~
      "Doenças não transmissíveis",
    stringr::str_detect(normalise_text(canonical$diagnosis_final),
                        "f[0-9]|depress|ansiedade|stress|psic|esquiz|saude mental") ~
      "Saúde mental",
    stringr::str_detect(normalise_text(canonical$diagnosis_final), "desnutri") ~
      "Nutrição",
    stringr::str_detect(normalise_text(canonical$diagnosis_final), "trauma|feriment|acident") ~
      "Trauma",
    has_value(canonical$diagnosis_final) ~ "Outros serviços",
    TRUE ~ "Não especificado"
  )
  canonical$negative_outcome <-
    normalise_text(canonical$outcome) %in% c("obito", "trasferencia", "transferencia") |
    normalise_text(canonical$care_action) == "internamento"
  canonical$negative_outcome[is.na(canonical$negative_outcome)] <- FALSE
  canonical$followup_expected <-
    stringr::str_detect(normalise_text(canonical$care_action), "seguimento") |
    stringr::str_detect(normalise_text(canonical$outcome), "seguimento")
  canonical$followup_expected[is.na(canonical$followup_expected)] <- FALSE
  canonical$vulnerability_group_final <- dplyr::case_when(
    canonical$vulnerable_population == "Sim" & has_value(canonical$vulnerable_group) ~ canonical$vulnerable_group,
    canonical$vulnerable_population == "Sim" ~ "Grupo não especificado",
    TRUE ~ "Não registado como vulnerável"
  )

  visits <- canonical |>
    dplyr::filter(has_value(tracked_entity_id), !is.na(event_date)) |>
    dplyr::distinct(tracked_entity_id, event_date) |>
    dplyr::arrange(tracked_entity_id, event_date) |>
    dplyr::group_by(tracked_entity_id) |>
    dplyr::mutate(next_event_date = dplyr::lead(event_date)) |>
    dplyr::ungroup()
  canonical <- canonical |>
    dplyr::left_join(visits, by = c("tracked_entity_id", "event_date")) |>
    dplyr::mutate(days_to_next_visit = as.integer(next_event_date - event_date))

  canonical
}

evaluate_quality <- function(data, late_days = 2) {
  x <- tibble::as_tibble(data)
  if (nrow(x) == 0) return(x)

  required <- intersect(REQUIRED_FIELDS, names(x))
  present_matrix <- do.call(
    cbind,
    lapply(required, function(field) has_value(x[[field]]))
  )
  colnames(present_matrix) <- required

  x$missing_required_n <- rowSums(!present_matrix)
  x$record_completeness <- 100 * rowMeans(present_matrix)
  x$missing_required_fields <- apply(
    !present_matrix,
    1,
    function(flag) {
      labels <- unname(FIELD_LABELS[required][flag])
      paste(labels, collapse = ", ")
    }
  )

  x$delay_days <- as.integer(as.Date(x$last_updated, tz = APP_TIMEZONE) - x$event_date)
  x$late_entry <- is.na(x$delay_days) | x$delay_days > late_days
  x$future_event <- !is.na(x$event_date) & x$event_date > Sys.Date()
  x$update_before_event <- !is.na(x$delay_days) & x$delay_days < 0
  x$invalid_age_group <- has_value(x$age_group) & !x$age_group %in% VALID_AGE_GROUPS
  x$invalid_sex <- has_value(x$sex) & !x$sex %in% VALID_SEX
  x$missing_record_id <- !has_value(x$record_id)

  valid_id <- has_value(x$record_id)
  x$duplicate_id <- valid_id & (
    duplicated(x$record_id) | duplicated(x$record_id, fromLast = TRUE)
  )

  duplicate_key <- c(
    "facility_id", "event_date", "first_name", "surname",
    "age_group", "sex", "diagnosis"
  )
  valid_duplicate_key <- Reduce(
    `&`,
    lapply(duplicate_key, function(field) has_value(x[[field]]))
  )
  x <- x |>
    dplyr::group_by(dplyr::across(dplyr::all_of(duplicate_key))) |>
    dplyr::mutate(potential_duplicate_n = dplyr::n()) |>
    dplyr::ungroup()
  x$potential_duplicate <- valid_duplicate_key & x$potential_duplicate_n > 1

  x$missing_other_test <-
    has_value(x$requested_test) & stringr::str_to_lower(x$requested_test) == "outro" &
    !has_value(x$other_test)
  x$missing_other_diagnosis <-
    has_value(x$diagnosis) & stringr::str_to_lower(x$diagnosis) == "outro" &
    !has_value(x$other_diagnosis)
  x$missing_vulnerable_group <-
    has_value(x$vulnerable_population) & x$vulnerable_population == "Sim" &
    !has_value(x$vulnerable_group)
  x$missing_community_actor <-
    has_value(x$community_referral) & x$community_referral == "Sim" &
    !has_value(x$community_actor_group)
  x$completed_missing_result <-
    has_value(x$event_status) & x$event_status == "COMPLETED" &
    !has_value(x$test_result)
  x$completed_missing_outcome <-
    has_value(x$event_status) & x$event_status == "COMPLETED" &
    !has_value(x$outcome)
  x$confirmed_indeterminate <-
    has_value(x$test_status) & has_value(x$test_result) &
    x$test_status == "Confirmado" & x$test_result == "Indeterminado"
  x$treatment_missing_action <-
    has_value(x$received_treatment) & x$received_treatment == "Sim" &
    !has_value(x$care_action)

  x$validity_issue <-
    x$future_event | x$update_before_event | x$invalid_age_group |
    x$invalid_sex | x$missing_record_id

  x$consistency_issue <-
    x$missing_other_test | x$missing_other_diagnosis |
    x$missing_vulnerable_group | x$missing_community_actor |
    x$completed_missing_result | x$completed_missing_outcome |
    x$confirmed_indeterminate | x$treatment_missing_action

  x$completeness_issue <- x$missing_required_n > 0
  x$uniqueness_issue <- x$duplicate_id | x$potential_duplicate

  x$issue_count <-
    as.integer(x$completeness_issue) + as.integer(x$late_entry) +
    as.integer(x$duplicate_id) + as.integer(x$potential_duplicate) +
    as.integer(x$validity_issue) + as.integer(x$consistency_issue)

  issue_flags <- cbind(
    "ID de evento repetido" = x$duplicate_id,
    "Possível duplicação clínica" = x$potential_duplicate,
    "Digitação fora do prazo" = x$late_entry,
    "Data futura" = x$future_event,
    "Actualização anterior ao evento" = x$update_before_event,
    "Faixa etária inválida" = x$invalid_age_group,
    "Sexo inválido" = x$invalid_sex,
    "ID do evento ausente" = x$missing_record_id,
    "Outro teste sem especificação" = x$missing_other_test,
    "Outro diagnóstico sem especificação" = x$missing_other_diagnosis,
    "Grupo vulnerável não especificado" = x$missing_vulnerable_group,
    "Actor comunitário não especificado" = x$missing_community_actor,
    "Evento concluído sem resultado" = x$completed_missing_result,
    "Evento concluído sem desfecho" = x$completed_missing_outcome,
    "Teste confirmado com resultado indeterminado" = x$confirmed_indeterminate,
    "Tratamento sem conduta registada" = x$treatment_missing_action
  )
  issue_flags[is.na(issue_flags)] <- FALSE

  generic_details <- apply(
    issue_flags,
    1,
    function(flag) paste(colnames(issue_flags)[flag], collapse = "; ")
  )
  missing_details <- ifelse(
    x$completeness_issue,
    paste0("Campos obrigatórios ausentes: ", x$missing_required_fields),
    ""
  )
  x$issue_detail <- mapply(
    function(a, b) paste(c(a, b)[nzchar(c(a, b))], collapse = "; "),
    missing_details,
    generic_details,
    USE.NAMES = FALSE
  )
  x$issue_detail[!nzchar(x$issue_detail)] <- "Sem problemas detectados"

  x$severity <- dplyr::case_when(
    x$duplicate_id | x$future_event | x$update_before_event ~ "Alta",
    x$potential_duplicate | x$validity_issue | x$consistency_issue ~ "Média",
    x$completeness_issue | x$late_entry ~ "Baixa",
    TRUE ~ "Nenhuma"
  )

  x$completeness_score <- x$record_completeness
  x$timeliness_score <- ifelse(!is.na(x$delay_days) & x$delay_days <= late_days, 100, 0)
  x$uniqueness_score <- ifelse(x$uniqueness_issue, 0, 100)
  x$validity_score <- ifelse(x$validity_issue, 0, 100)
  x$consistency_score <- ifelse(x$consistency_issue, 0, 100)
  x$quality_score <-
    QUALITY_WEIGHTS[["completeness"]] * x$completeness_score +
    QUALITY_WEIGHTS[["timeliness"]] * x$timeliness_score +
    QUALITY_WEIGHTS[["uniqueness"]] * x$uniqueness_score +
    QUALITY_WEIGHTS[["validity"]] * x$validity_score +
    QUALITY_WEIGHTS[["consistency"]] * x$consistency_score
  x$quality_status <- quality_band(x$quality_score)

  x
}

prepare_monitor_data <- function(raw, late_days = 2) {
  map_to_canonical(raw) |>
    evaluate_quality(late_days = late_days)
}

quality_dimensions <- function(data, late_days = 2) {
  if (nrow(data) == 0) {
    return(tibble::tibble(
      dimension = c("Completude", "Pontualidade", "Unicidade", "Validade", "Consistência"),
      score = rep(NA_real_, 5)
    ))
  }

  tibble::tibble(
    dimension = c("Completude", "Pontualidade", "Unicidade", "Validade", "Consistência"),
    score = c(
      safe_mean(data$record_completeness),
      safe_pct(!is.na(data$delay_days) & data$delay_days <= late_days),
      safe_pct(!data$uniqueness_issue),
      safe_pct(!data$validity_issue),
      safe_pct(!data$consistency_issue)
    )
  )
}

overall_quality_score <- function(data, late_days = 2) {
  dimensions <- quality_dimensions(data, late_days)
  weights <- unname(QUALITY_WEIGHTS)
  if (any(is.na(dimensions$score))) return(NA_real_)
  sum(dimensions$score * weights)
}

field_completeness <- function(data) {
  fields <- intersect(names(FIELD_LABELS), names(data))
  if (nrow(data) == 0 || length(fields) == 0) {
    return(tibble::tibble(field = character(), label = character(), complete_n = integer(),
                          missing_n = integer(), completeness = numeric()))
  }

  purrr::map_dfr(fields, function(field) {
    complete <- has_value(data[[field]])
    tibble::tibble(
      field = field,
      label = unname(FIELD_LABELS[[field]] %||% field),
      complete_n = sum(complete),
      missing_n = sum(!complete),
      completeness = safe_pct(complete)
    )
  }) |>
    dplyr::arrange(completeness, label)
}

period_start_date <- function(date, unit = "week") {
  lubridate::floor_date(date, unit = unit, week_start = 1) |>
    as.Date()
}

period_sequence <- function(start_date, end_date, unit = "week") {
  start_date <- period_start_date(as.Date(start_date), unit)
  end_date <- period_start_date(as.Date(end_date), unit)
  seq.Date(start_date, end_date, by = switch(unit, day = "day", week = "week", month = "month"))
}

period_label <- function(date, unit = "week") {
  dplyr::case_when(
    unit == "day" ~ format(date, "%d/%m/%Y"),
    unit == "month" ~ format(date, "%Y-%m"),
    TRUE ~ paste0("S", format(date, "%V"), "/", format(date, "%G"))
  )
}

facility_monitoring <- function(
    data,
    start_date,
    end_date,
    unit = "week",
    minimum_events = 1,
    late_days = 2,
    facility_universe = NULL) {

  if (is.null(facility_universe)) {
    facilities <- data |>
      dplyr::distinct(province, district, facility, facility_code)
  } else {
    facilities <- facility_universe |>
      dplyr::distinct(province, district, facility, facility_code)
  }
  facilities <- facilities |> dplyr::filter(has_value(facility))
  if (nrow(facilities) == 0) return(tibble::tibble())

  periods <- period_sequence(start_date, end_date, unit)

  counts <- data |>
    dplyr::mutate(period_start = period_start_date(event_date, unit)) |>
    dplyr::count(province, district, facility, facility_code, period_start, name = "events")

  coverage <- tidyr::crossing(facilities, period_start = periods) |>
    dplyr::left_join(
      counts,
      by = c("province", "district", "facility", "facility_code", "period_start")
    ) |>
    dplyr::mutate(
      events = tidyr::replace_na(events, 0L),
      period_reported = events >= minimum_events
    ) |>
    dplyr::group_by(province, district, facility, facility_code) |>
    dplyr::summarise(
      expected_periods = dplyr::n(),
      reported_periods = sum(period_reported),
      silent_periods = sum(!period_reported),
      reporting_coverage = safe_pct(period_reported),
      .groups = "drop"
    )

  metrics <- data |>
    dplyr::group_by(province, district, facility, facility_code) |>
    dplyr::summarise(
      events = dplyr::n(),
      diseases = dplyr::n_distinct(diagnosis_final[has_value(diagnosis_final)]),
      last_event = suppressWarnings(max(event_date, na.rm = TRUE)),
      days_silent = as.integer(as.Date(end_date) - last_event),
      field_completeness = safe_mean(record_completeness),
      timely = safe_pct(!is.na(delay_days) & delay_days <= late_days),
      uniqueness = safe_pct(!uniqueness_issue),
      validity = safe_pct(!validity_issue),
      consistency = safe_pct(!consistency_issue),
      alerts = sum(issue_count > 0),
      high_alerts = sum(severity == "Alta"),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      quality_score =
        QUALITY_WEIGHTS[["completeness"]] * field_completeness +
        QUALITY_WEIGHTS[["timeliness"]] * timely +
        QUALITY_WEIGHTS[["uniqueness"]] * uniqueness +
        QUALITY_WEIGHTS[["validity"]] * validity +
        QUALITY_WEIGHTS[["consistency"]] * consistency,
      quality_status = quality_band(quality_score)
    )

  coverage |>
    dplyr::left_join(
      metrics,
      by = c("province", "district", "facility", "facility_code")
    ) |>
    dplyr::mutate(
      events = tidyr::replace_na(events, 0L),
      diseases = tidyr::replace_na(diseases, 0L),
      alerts = tidyr::replace_na(alerts, 0L),
      high_alerts = tidyr::replace_na(high_alerts, 0L),
      quality_status = tidyr::replace_na(quality_status, "Sem dados"),
      reporting_status = dplyr::case_when(
        reporting_coverage >= 90 ~ "Excelente",
        reporting_coverage >= 80 ~ "Bom",
        reporting_coverage >= 60 ~ "Atenção",
        TRUE ~ "Crítico"
      )
    ) |>
    dplyr::arrange(quality_score, reporting_coverage)
}

facility_period_grid <- function(data, start_date, end_date, unit = "week", facilities = NULL) {
  if (is.null(facilities)) {
    facilities <- sort(unique(data$facility[has_value(data$facility)]))
  } else {
    facilities <- sort(unique(facilities[has_value(facilities)]))
  }
  if (length(facilities) == 0) return(tibble::tibble())
  periods <- period_sequence(start_date, end_date, unit)

  data |>
    dplyr::mutate(period_start = period_start_date(event_date, unit)) |>
    dplyr::count(facility, period_start, name = "events") |>
    tidyr::complete(
      facility = facilities,
      period_start = periods,
      fill = list(events = 0L)
    ) |>
    dplyr::mutate(period_label = period_label(period_start, unit))
}

facility_field_matrix <- function(data) {
  fields <- intersect(REQUIRED_FIELDS, names(data))
  if (nrow(data) == 0) return(tibble::tibble())

  data |>
    dplyr::select(facility, dplyr::all_of(fields)) |>
    dplyr::mutate(dplyr::across(dplyr::all_of(fields), as.character)) |>
    tidyr::pivot_longer(
      cols = -facility,
      names_to = "field",
      values_to = "value"
    ) |>
    dplyr::mutate(complete = has_value(value)) |>
    dplyr::group_by(facility, field) |>
    dplyr::summarise(completeness = safe_pct(complete), .groups = "drop") |>
    dplyr::mutate(label = unname(FIELD_LABELS[field]))
}

issue_summary <- function(data) {
  if (nrow(data) == 0) return(tibble::tibble())

  tibble::tibble(
    issue = c(
      "Campos obrigatórios incompletos", "Digitação fora do prazo",
      "ID de evento repetido", "Possível duplicação clínica",
      "Problema de validade", "Inconsistência lógica",
      "Coordenadas não utilizáveis"
    ),
    records = c(
      sum(data$completeness_issue), sum(data$late_entry),
      sum(data$duplicate_id), sum(data$potential_duplicate),
      sum(data$validity_issue), sum(data$consistency_issue),
      sum(!data$valid_coordinates)
    )
  ) |>
    dplyr::mutate(percent = 100 * records / nrow(data)) |>
    dplyr::arrange(dplyr::desc(records))
}

alerts_table <- function(data) {
  safe_columns <- intersect(SAFE_EXPORT_COLUMNS, names(data))
  data |>
    dplyr::filter(issue_count > 0) |>
    dplyr::select(dplyr::all_of(safe_columns)) |>
    dplyr::arrange(
      factor(severity, levels = c("Alta", "Média", "Baixa", "Nenhuma")),
      dplyr::desc(event_date)
    )
}

facility_alert_summary <- function(data, late_days = 2) {
  if (nrow(data) == 0) return(tibble::tibble())

  data |>
    dplyr::group_by(province, district, facility) |>
    dplyr::summarise(
      records = dplyr::n(),
      high = sum(severity == "Alta"),
      medium = sum(severity == "Média"),
      low = sum(severity == "Baixa"),
      incomplete = sum(completeness_issue),
      late = sum(late_entry),
      duplicate_id = sum(duplicate_id),
      possible_duplicates = sum(potential_duplicate),
      inconsistent = sum(consistency_issue),
      field_completeness = safe_mean(record_completeness),
      timely = safe_pct(!is.na(delay_days) & delay_days <= late_days),
      uniqueness = safe_pct(!uniqueness_issue),
      validity = safe_pct(!validity_issue),
      consistency = safe_pct(!consistency_issue),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      quality_score =
        QUALITY_WEIGHTS[["completeness"]] * field_completeness +
        QUALITY_WEIGHTS[["timeliness"]] * timely +
        QUALITY_WEIGHTS[["uniqueness"]] * uniqueness +
        QUALITY_WEIGHTS[["validity"]] * validity +
        QUALITY_WEIGHTS[["consistency"]] * consistency,
      status = quality_band(quality_score)
    ) |>
    dplyr::arrange(dplyr::desc(high), quality_score)
}

period_outliers <- function(data, start_date, end_date, unit = "week", facilities = NULL) {
  grid <- facility_period_grid(data, start_date, end_date, unit, facilities)
  if (nrow(grid) == 0) return(grid)

  grid |>
    dplyr::group_by(facility) |>
    dplyr::mutate(
      median_events = stats::median(events, na.rm = TRUE),
      mad_events = stats::mad(events, constant = 1, na.rm = TRUE),
      robust_z = dplyr::if_else(
        mad_events > 0,
        0.6745 * (events - median_events) / mad_events,
        dplyr::if_else(events == median_events, 0, sign(events - median_events) * 9)
      ),
      anomaly = abs(robust_z) >= 3.5,
      anomaly_type = dplyr::case_when(
        !anomaly ~ "Normal",
        events == 0 | robust_z < 0 ~ "Queda/silêncio",
        TRUE ~ "Pico de registos"
      )
    ) |>
    dplyr::ungroup()
}

spc_data <- function(data, start_date, end_date, unit = "week") {
  if (nrow(data) == 0) return(tibble::tibble())

  periods <- tibble::tibble(period_start = period_sequence(start_date, end_date, unit))
  counts <- data |>
    dplyr::mutate(period_start = period_start_date(event_date, unit)) |>
    dplyr::count(period_start, name = "events")

  result <- periods |>
    dplyr::left_join(counts, by = "period_start") |>
    dplyr::mutate(events = tidyr::replace_na(events, 0L))

  centre <- mean(result$events)
  sigma <- stats::sd(result$events)
  if (is.na(sigma)) sigma <- 0
  result |>
    dplyr::mutate(
      centre = centre,
      upper_control = centre + 3 * sigma,
      lower_control = pmax(0, centre - 3 * sigma),
      outside_control = events > upper_control | events < lower_control,
      period_label = period_label(period_start, unit)
    )
}

disease_summary <- function(data) {
  if (nrow(data) == 0) return(tibble::tibble())

  data |>
    dplyr::filter(has_value(diagnosis_final)) |>
    dplyr::group_by(diagnosis_final) |>
    dplyr::summarise(
      notifications = dplyr::n(),
      facilities = dplyr::n_distinct(facility),
      positive = sum(test_result %in% c("Positivo", "Confirmado-Ligação epidemiológica"), na.rm = TRUE),
      positivity = safe_pct(test_result %in% c("Positivo", "Confirmado-Ligação epidemiológica")),
      deaths = sum(outcome == "Óbito", na.rm = TRUE),
      field_completeness = safe_mean(record_completeness),
      .groups = "drop"
    ) |>
    dplyr::arrange(dplyr::desc(notifications))
}

safe_export_data <- function(data) {
  data |>
    dplyr::select(dplyr::all_of(intersect(SAFE_EXPORT_COLUMNS, names(data))))
}

write_quality_workbook <- function(
    data,
    path,
    start_date,
    end_date,
    unit = "week",
    minimum_events = 1,
    late_days = 2) {

  dimensions <- quality_dimensions(data, late_days)
  score <- overall_quality_score(data, late_days)
  facilities <- facility_monitoring(
    data, start_date, end_date, unit, minimum_events, late_days
  )
  fields <- field_completeness(data)
  issues <- issue_summary(data)
  alerts <- alerts_table(data)
  diseases <- disease_summary(data)

  summary <- tibble::tibble(
    Indicador = c(
      "Período analisado", "Total de registos", "Unidades sanitárias",
      "Diagnósticos notificados", "Índice global de qualidade",
      "Classificação", "Registos com alerta", "Alertas de gravidade alta",
      "Mediana do atraso de digitação (dias)"
    ),
    Valor = c(
      paste(format(start_date, "%d/%m/%Y"), "a", format(end_date, "%d/%m/%Y")),
      nrow(data), dplyr::n_distinct(data$facility),
      dplyr::n_distinct(data$diagnosis_final[has_value(data$diagnosis_final)]),
      fmt_pct(score), quality_band(score), sum(data$issue_count > 0),
      sum(data$severity == "Alta"), fmt_number(safe_median(data$delay_days), 1)
    )
  )

  wb <- openxlsx::createWorkbook(creator = "Projecto ALIVE")
  header_style <- openxlsx::createStyle(
    fgFill = "#0B5D4B", fontColour = "#FFFFFF", textDecoration = "bold",
    halign = "center", border = "Bottom"
  )
  percent_style <- openxlsx::createStyle(numFmt = "0.0%")

  sheets <- list(
    Resumo = summary,
    Dimensoes_qualidade = dimensions,
    Unidades_sanitarias = facilities,
    Completude_campos = fields,
    Problemas = issues,
    Doencas = diseases,
    Alertas = alerts,
    Dados_sem_identificadores = safe_export_data(data)
  )

  for (sheet_name in names(sheets)) {
    openxlsx::addWorksheet(wb, sheet_name)
    openxlsx::writeData(wb, sheet_name, sheets[[sheet_name]], withFilter = TRUE)
    if (ncol(sheets[[sheet_name]]) > 0) {
      openxlsx::addStyle(
        wb, sheet_name, header_style, rows = 1,
        cols = seq_len(ncol(sheets[[sheet_name]])), gridExpand = TRUE
      )
      openxlsx::freezePane(wb, sheet_name, firstRow = TRUE)
      openxlsx::setColWidths(wb, sheet_name, cols = seq_len(ncol(sheets[[sheet_name]])), widths = "auto")
    }
  }

  openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
  invisible(path)
}

# ============================================================
# ANÁLISES EPIDEMIOLÓGICAS E DE ACESSO — PROJECTO ALIVE
# ============================================================

analysis_deduplicate <- function(data) {
  x <- tibble::as_tibble(data)
  if (nrow(x) == 0) return(x)

  x$.analysis_row <- seq_len(nrow(x))
  x <- x |>
    dplyr::arrange(dplyr::desc(last_updated), dplyr::desc(event_date), .analysis_row)

  id_key <- ifelse(
    has_value(x$record_id), paste0("id:", x$record_id), paste0("row:", x$.analysis_row)
  )
  x <- x[!duplicated(id_key), , drop = FALSE]

  clinical_fields <- intersect(
    c("facility_id", "event_date", "first_name", "surname", "age_group", "sex", "diagnosis_final"),
    names(x)
  )
  if (length(clinical_fields) > 0) {
    complete_key <- Reduce(`&`, lapply(clinical_fields, function(field) has_value(x[[field]])))
    clinical_key <- do.call(
      paste,
      c(lapply(clinical_fields, function(field) normalise_text(x[[field]])), sep = "\r")
    )
    clinical_key[!complete_key] <- paste0("row:", x$.analysis_row[!complete_key])
    x <- x[!duplicated(clinical_key), , drop = FALSE]
  }

  x |>
    dplyr::arrange(event_date, facility) |>
    dplyr::select(-.analysis_row)
}

analysis_scope_data <- function(
    data,
    facilities = NULL,
    diseases = NULL,
    combine_facilities = FALSE,
    group_name = "Grupo seleccionado",
    exclude_duplicates = TRUE) {
  x <- tibble::as_tibble(data)
  if (!is.null(facilities) && length(facilities)) {
    x <- x |> dplyr::filter(facility %in% facilities)
  }
  if (!is.null(diseases) && length(diseases)) {
    x <- x |> dplyr::filter(diagnosis_final %in% diseases)
  }
  if (exclude_duplicates) x <- analysis_deduplicate(x)

  selected_name <- clean_character(group_name)[1]
  if (is.na(selected_name)) selected_name <- "Grupo seleccionado"
  x$analysis_unit <- if (isTRUE(combine_facilities)) selected_name else x$facility
  x
}

rolling_threshold_vector <- function(
    counts,
    method = "mean_sd",
    baseline_periods = 8L,
    minimum_history = 4L,
    standard_deviations = 2,
    mad_multiplier = 3,
    percentile = 0.95,
    growth_percent = 50) {
  n <- length(counts)
  threshold <- rep(NA_real_, n)
  baseline <- rep(NA_real_, n)
  history_n <- integer(n)

  for (i in seq_len(n)) {
    previous_positions <- seq.int(max(1L, i - as.integer(baseline_periods)), i - 1L)
    previous_positions <- previous_positions[previous_positions >= 1L & previous_positions < i]
    history <- counts[previous_positions]
    history <- history[is.finite(history)]
    history_n[i] <- length(history)
    if (length(history) < as.integer(minimum_history)) next

    baseline[i] <- mean(history)
    threshold[i] <- switch(
      method,
      mean_sd = mean(history) + standard_deviations * (stats::sd(history) %||% 0),
      median_mad = stats::median(history) + mad_multiplier * stats::mad(history, constant = 1),
      percentile = as.numeric(stats::quantile(history, probs = percentile, names = FALSE, type = 7)),
      growth = utils::tail(history, 1) * (1 + growth_percent / 100),
      mean(history) + standard_deviations * (stats::sd(history) %||% 0)
    )
    if (!is.finite(threshold[i])) threshold[i] <- baseline[i]
  }

  tibble::tibble(threshold = threshold, baseline = baseline, history_n = history_n)
}

outbreak_alert_series <- function(
    data,
    start_date,
    end_date,
    unit = "week",
    facilities = NULL,
    diseases = NULL,
    combine_facilities = FALSE,
    group_name = "Grupo seleccionado",
    exclude_duplicates = TRUE,
    method = "mean_sd",
    baseline_periods = 8L,
    minimum_history = 4L,
    minimum_cases = 2L,
    standard_deviations = 2,
    mad_multiplier = 3,
    percentile = 0.95,
    growth_percent = 50) {
  x <- analysis_scope_data(
    data, facilities, diseases, combine_facilities, group_name, exclude_duplicates
  ) |>
    dplyr::filter(!is.na(event_date), has_value(analysis_unit), has_value(diagnosis_final))

  if (nrow(x) == 0) return(tibble::tibble())
  periods <- period_sequence(start_date, end_date, unit)
  combinations <- x |> dplyr::distinct(analysis_unit, diagnosis_final)
  grid <- merge(
    combinations, tibble::tibble(period_start = periods), by = NULL, sort = FALSE
  ) |>
    tibble::as_tibble()
  counts <- x |>
    dplyr::mutate(period_start = period_start_date(event_date, unit)) |>
    dplyr::count(analysis_unit, diagnosis_final, period_start, name = "cases")

  grid |>
    dplyr::left_join(counts, by = c("analysis_unit", "diagnosis_final", "period_start")) |>
    dplyr::mutate(cases = tidyr::replace_na(cases, 0L)) |>
    dplyr::group_by(analysis_unit, diagnosis_final) |>
    dplyr::group_modify(function(.x, .y) {
      limits <- rolling_threshold_vector(
        .x$cases, method, baseline_periods, minimum_history,
        standard_deviations, mad_multiplier, percentile, growth_percent
      )
      dplyr::bind_cols(.x, limits)
    }) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      alert = history_n >= minimum_history & cases >= minimum_cases & cases > threshold,
      excess_cases = dplyr::if_else(alert, pmax(0, cases - threshold), 0),
      alert_level = dplyr::case_when(
        !alert ~ "Sem alerta",
        threshold <= 0 | cases >= 2 * threshold ~ "Muito elevado",
        TRUE ~ "Elevado"
      ),
      method_label = dplyr::recode(
        method,
        mean_sd = "Média + desvios-padrão",
        median_mad = "Mediana + MAD",
        percentile = "Percentil histórico",
        growth = "Crescimento face ao período anterior",
        .default = method
      ),
      period_label = period_label(period_start, unit)
    ) |>
    dplyr::arrange(dplyr::desc(alert), dplyr::desc(period_start), analysis_unit, diagnosis_final)
}

who_measles_alerts <- function(
    data,
    facilities = NULL,
    combine_facilities = FALSE,
    group_name = "Grupo seleccionado",
    exclude_duplicates = TRUE) {
  x <- analysis_scope_data(
    data, facilities, NULL, combine_facilities, group_name, exclude_duplicates
  ) |>
    dplyr::filter(measles_who_suspected, !is.na(measles_onset_date)) |>
    dplyr::arrange(analysis_unit, measles_onset_date)
  if (nrow(x) == 0) return(tibble::tibble())

  x |>
    dplyr::group_by(analysis_unit) |>
    dplyr::group_modify(function(.x, .y) {
      dates <- .x$measles_onset_date
      suspected_21d <- vapply(
        dates, function(day) sum(dates >= day - 20 & dates <= day, na.rm = TRUE), numeric(1)
      )
      confirmed_21d <- vapply(
        dates,
        function(day) sum(
          .x$measles_lab_confirmed & dates >= day - 20 & dates <= day, na.rm = TRUE
        ),
        numeric(1)
      )
      linked_21d <- vapply(
        dates,
        function(day) sum(
          .x$measles_epi_linked & dates >= day - 20 & dates <= day, na.rm = TRUE
        ),
        numeric(1)
      )
      onset_span_21d <- vapply(
        dates,
        function(day) {
          window_dates <- dates[dates >= day - 20 & dates <= day]
          if (length(window_dates) < 2) return(0)
          as.numeric(max(window_dates) - min(window_dates))
        },
        numeric(1)
      )
      proxy_21d <- vapply(
        dates,
        function(day) sum(
          .x$measles_onset_is_proxy & dates >= day - 20 & dates <= day, na.rm = TRUE
        ),
        numeric(1)
      )
      .x |>
        dplyr::mutate(
          suspected_21d = suspected_21d,
          confirmed_21d = confirmed_21d,
          linked_21d = linked_21d,
          onset_span_21d = onset_span_21d,
          proxy_21d = proxy_21d
        )
    }) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      signal = dplyr::case_when(
        confirmed_21d >= 2 & linked_21d >= 2 &
          onset_span_21d >= 7 & onset_span_21d <= 21 & proxy_21d == 0 ~
          "Sinal compatível com surto confirmado — validar ligação virológica/epidemiológica",
        suspected_21d >= 5 & linked_21d >= 5 &
          onset_span_21d >= 7 & onset_span_21d <= 21 & proxy_21d == 0 ~
          "Sinal compatível com surto suspeito — iniciar investigação",
        confirmed_21d >= 2 | suspected_21d >= 5 ~
          "Agregado acima da contagem OMS — critérios temporais/ligação ainda não confirmados",
        TRUE ~ "Caso suspeito — notificar e investigar imediatamente"
      ),
      definition_status = dplyr::case_when(
        measles_lab_confirmed ~ "Confirmado laboratorialmente",
        measles_epi_linked ~ "Ligado epidemiologicamente",
        measles_who_clinical ~ "Compatível com definição clínica",
        TRUE ~ "Suspeito"
      )
    ) |>
    dplyr::select(
      analysis_unit, facility, event_date, measles_onset_date, measles_onset_is_proxy,
      age_group, sex, diagnosis_final, definition_status,
      suspected_21d, confirmed_21d, linked_21d, onset_span_21d, proxy_21d, signal
    ) |>
    dplyr::arrange(dplyr::desc(measles_onset_date), analysis_unit)
}

cholera_probable_required <- function(tests) {
  dplyr::case_when(
    tests >= 3 & tests <= 7 ~ 3,
    tests >= 8 & tests <= 10 ~ 4,
    tests >= 11 & tests <= 14 ~ 5,
    tests >= 15 & tests <= 17 ~ 6,
    tests >= 18 & tests <= 21 ~ 7,
    TRUE ~ NA_real_
  )
}

who_cholera_alerts <- function(
    data,
    facilities = NULL,
    combine_facilities = FALSE,
    group_name = "Grupo seleccionado",
    exclude_duplicates = TRUE) {
  x <- analysis_scope_data(
    data, facilities, NULL, combine_facilities, group_name, exclude_duplicates
  ) |>
    dplyr::filter(
      cholera_suspected_proxy | cholera_rdt_tested | cholera_lab_tested,
      !is.na(event_date)
    ) |>
    dplyr::arrange(analysis_unit, event_date)
  if (nrow(x) == 0) return(tibble::tibble())

  x |>
    dplyr::group_by(analysis_unit) |>
    dplyr::group_modify(function(.x, .y) {
      dates <- .x$event_date
      rolling_sum <- function(flag, days) {
        vapply(
          dates,
          function(day) sum(flag & dates >= day - (days - 1) & dates <= day, na.rm = TRUE),
          numeric(1)
        )
      }
      .x |>
        dplyr::mutate(
          suspected_7d = rolling_sum(cholera_suspected_proxy, 7),
          rdt_positive_7d = rolling_sum(cholera_rdt_positive, 7),
          rdt_tested_14d = rolling_sum(cholera_rdt_tested, 14),
          rdt_positive_14d = rolling_sum(cholera_rdt_positive, 14),
          confirmed_14d = rolling_sum(cholera_lab_confirmed, 14)
        )
    }) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      probable_required = cholera_probable_required(rdt_tested_14d),
      signal = dplyr::case_when(
        confirmed_14d >= 1 ~ "Sinal de surto confirmado — confirmar aquisição local",
        !is.na(probable_required) & rdt_positive_14d >= probable_required ~ "Sinal de surto provável",
        suspected_7d >= 2 | rdt_positive_7d >= 1 ~ "Sinal de surto suspeito",
        TRUE ~ "Sem limiar de surto atingido"
      )
    ) |>
    dplyr::filter(signal != "Sem limiar de surto atingido") |>
    dplyr::select(
      analysis_unit, facility, event_date, suspected_7d, rdt_positive_7d,
      rdt_tested_14d, rdt_positive_14d, probable_required, confirmed_14d, signal
    ) |>
    dplyr::arrange(dplyr::desc(event_date), analysis_unit)
}

read_denominators <- function(path) {
  if (is.null(path) || !file.exists(path)) return(tibble::tibble())
  x <- readr::read_csv(path, show_col_types = FALSE, trim_ws = TRUE)
  names(x) <- normalise_text(names(x)) |>
    stringr::str_replace_all("[^a-z0-9]+", "_") |>
    stringr::str_replace_all("^_|_$", "")
  aliases <- c(
    unidade_sanitaria = "facility", us = "facility", codigo_us = "facility_code",
    codigo = "facility_code", populacao = "population", populacao_alvo = "target_population"
  )
  for (old in names(aliases)) {
    if (old %in% names(x) && !aliases[[old]] %in% names(x)) names(x)[names(x) == old] <- aliases[[old]]
  }
  for (field in c("facility", "facility_code", "population", "target_population")) {
    if (!field %in% names(x)) x[[field]] <- NA
  }
  x |>
    dplyr::transmute(
      facility = clean_character(facility),
      facility_code = clean_character(facility_code),
      population = suppressWarnings(readr::parse_number(as.character(population))),
      target_population = suppressWarnings(readr::parse_number(as.character(target_population)))
    ) |>
    dplyr::filter(has_value(facility) | has_value(facility_code))
}

service_access_summary <- function(
    data,
    continuity_days = 60L,
    denominators = NULL,
    start_date = NULL,
    end_date = NULL,
    unit = "week",
    facility_universe = NULL) {
  x <- tibble::as_tibble(data) |>
    dplyr::filter(has_value(facility))
  if (is.null(facility_universe)) {
    facilities <- x |> dplyr::distinct(province, district, facility, facility_code)
  } else {
    facilities <- tibble::as_tibble(facility_universe) |>
      dplyr::distinct(province, district, facility, facility_code) |>
      dplyr::filter(has_value(facility))
  }
  if (nrow(facilities) == 0) return(tibble::tibble())

  if (is.null(start_date) || is.null(end_date)) {
    valid_dates <- x$event_date[!is.na(x$event_date)]
    start_date <- if (length(valid_dates)) min(valid_dates) else Sys.Date()
    end_date <- if (length(valid_dates)) max(valid_dates) else start_date
  }
  periods <- period_sequence(start_date, end_date, unit)
  expected_periods <- length(periods)
  activity <- x |>
    dplyr::filter(!is.na(event_date)) |>
    dplyr::mutate(period_start = period_start_date(event_date, unit)) |>
    dplyr::distinct(facility, facility_code, period_start) |>
    dplyr::count(facility, facility_code, name = "active_periods")

  metrics <- x |>
    dplyr::group_by(province, district, facility, facility_code) |>
    dplyr::summarise(
      contacts = dplyr::n(),
      people = dplyr::n_distinct(tracked_entity_id[has_value(tracked_entity_id)]),
      service_days = dplyr::n_distinct(event_date[!is.na(event_date)]),
      first_consultations = sum(stringr::str_detect(normalise_text(consultation_type), "primeira"), na.rm = TRUE),
      followup_consultations = sum(stringr::str_detect(normalise_text(consultation_type), "seguimento"), na.rm = TRUE),
      diagnosed = sum(has_value(diagnosis_final)),
      treated = sum(received_treatment == "Sim", na.rm = TRUE),
      community_referred = sum(community_referral == "Sim", na.rm = TRUE),
      vulnerable = sum(vulnerable_population == "Sim", na.rm = TRUE),
      followup_expected_n = sum(followup_expected, na.rm = TRUE),
      returned_on_time_n = sum(
        followup_expected & !is.na(days_to_next_visit) & days_to_next_visit <= continuity_days,
        na.rm = TRUE
      ),
      discharged = sum(normalise_text(outcome) == "alta", na.rm = TRUE),
      negative_outcomes = sum(negative_outcome, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      diagnostic_completion = 100 * diagnosed / pmax(contacts, 1),
      treatment_coverage = 100 * treated / pmax(diagnosed, 1),
      community_access_share = 100 * community_referred / pmax(contacts, 1),
      vulnerable_access_share = 100 * vulnerable / pmax(contacts, 1),
      continuity_rate = dplyr::if_else(
        followup_expected_n > 0, 100 * returned_on_time_n / followup_expected_n, NA_real_
      )
    )

  result <- facilities |>
    dplyr::left_join(metrics, by = c("province", "district", "facility", "facility_code")) |>
    dplyr::left_join(activity, by = c("facility", "facility_code"))
  count_fields <- c(
    "contacts", "people", "service_days", "first_consultations", "followup_consultations",
    "diagnosed", "treated", "community_referred", "vulnerable", "followup_expected_n",
    "returned_on_time_n", "discharged", "negative_outcomes", "active_periods"
  )
  result <- result |>
    dplyr::mutate(dplyr::across(dplyr::all_of(count_fields), ~tidyr::replace_na(.x, 0))) |>
    dplyr::mutate(
      expected_periods = expected_periods,
      silent_periods = pmax(0, expected_periods - active_periods),
      service_continuity = 100 * active_periods / pmax(expected_periods, 1),
      diagnostic_completion = 100 * diagnosed / pmax(contacts, 1),
      treatment_coverage = dplyr::if_else(diagnosed > 0, 100 * treated / diagnosed, NA_real_),
      community_access_share = 100 * community_referred / pmax(contacts, 1),
      vulnerable_access_share = 100 * vulnerable / pmax(contacts, 1),
      continuity_rate = dplyr::if_else(
        followup_expected_n > 0, 100 * returned_on_time_n / followup_expected_n, NA_real_
      )
    )

  if (!is.null(denominators) && nrow(denominators) > 0) {
    by_code <- denominators |>
      dplyr::filter(has_value(facility_code)) |>
      dplyr::select(facility_code, population, target_population) |>
      dplyr::distinct(facility_code, .keep_all = TRUE)
    by_name <- denominators |>
      dplyr::filter(has_value(facility)) |>
      dplyr::select(facility, population_name = population, target_population_name = target_population) |>
      dplyr::distinct(facility, .keep_all = TRUE)
    result <- result |>
      dplyr::left_join(by_code, by = "facility_code") |>
      dplyr::left_join(by_name, by = "facility") |>
      dplyr::mutate(
        population = dplyr::coalesce(population, population_name),
        target_population = dplyr::coalesce(target_population, target_population_name)
      ) |>
      dplyr::select(-population_name, -target_population_name)
  } else {
    result$population <- NA_real_
    result$target_population <- NA_real_
  }

  result |>
    dplyr::mutate(
      contacts_per_1000 = dplyr::if_else(
        !is.na(population) & population > 0, 1000 * contacts / population, NA_real_
      ),
      target_reach = dplyr::if_else(
        !is.na(target_population) & target_population > 0,
        100 * people / target_population,
        NA_real_
      )
    ) |>
    dplyr::arrange(dplyr::desc(contacts))
}

service_cascade <- function(data, continuity_days = 60L) {
  x <- tibble::as_tibble(data)
  if (nrow(x) == 0) return(tibble::tibble())
  expected <- sum(x$followup_expected, na.rm = TRUE)
  tibble::tibble(
    stage = factor(
      c("Contactos", "Diagnóstico registado", "Tratamento recebido", "Seguimento indicado", "Retorno no prazo"),
      levels = c("Contactos", "Diagnóstico registado", "Tratamento recebido", "Seguimento indicado", "Retorno no prazo")
    ),
    value = c(
      nrow(x), sum(has_value(x$diagnosis_final)), sum(x$received_treatment == "Sim", na.rm = TRUE),
      expected,
      sum(x$followup_expected & !is.na(x$days_to_next_visit) & x$days_to_next_visit <= continuity_days, na.rm = TRUE)
    ),
    denominator_note = c(
      "Todos os contactos", "Todos os contactos", "Diagnósticos/contactos", "Todos os contactos",
      paste0("Dos ", expected, " com seguimento indicado")
    )
  )
}

vulnerability_summary <- function(data, continuity_days = 60L) {
  x <- tibble::as_tibble(data) |>
    dplyr::filter(has_value(facility))
  if (nrow(x) == 0) return(tibble::tibble())

  x |>
    dplyr::mutate(
      vulnerability_dimension = dplyr::case_when(
        vulnerable_population == "Sim" ~ vulnerability_group_final,
        age_group == "0-4 anos" ~ "Crianças de 0–4 anos",
        age_group == "55+ anos" ~ "Pessoas de 55+ anos",
        TRUE ~ "Outros utentes"
      )
    ) |>
    dplyr::group_by(facility, vulnerability_dimension, sex, age_group) |>
    dplyr::summarise(
      contacts = dplyr::n(),
      community_referred = sum(community_referral == "Sim", na.rm = TRUE),
      treated = sum(received_treatment == "Sim", na.rm = TRUE),
      negative_outcomes = sum(negative_outcome, na.rm = TRUE),
      followup_expected_n = sum(followup_expected, na.rm = TRUE),
      returned_on_time_n = sum(
        followup_expected & !is.na(days_to_next_visit) & days_to_next_visit <= continuity_days,
        na.rm = TRUE
      ),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      treatment_rate = 100 * treated / pmax(contacts, 1),
      negative_outcome_rate = 100 * negative_outcomes / pmax(contacts, 1),
      continuity_rate = dplyr::if_else(
        followup_expected_n > 0, 100 * returned_on_time_n / followup_expected_n, NA_real_
      )
    ) |>
    dplyr::arrange(dplyr::desc(contacts))
}

facility_variation <- function(data, diseases = NULL, denominators = NULL) {
  all_data <- tibble::as_tibble(data) |>
    dplyr::filter(has_value(facility))
  totals <- all_data |> dplyr::count(facility, facility_code, name = "all_contacts")
  x <- all_data |> dplyr::filter(has_value(diagnosis_final))
  if (nrow(x) == 0) return(tibble::tibble())
  if (!is.null(diseases) && length(diseases)) {
    diagnoses <- intersect(diseases, unique(x$diagnosis_final))
    x <- x |> dplyr::filter(diagnosis_final %in% diagnoses)
  } else {
    diagnoses <- sort(unique(x$diagnosis_final))
  }
  if (nrow(x) == 0) return(tibble::tibble())

  facility_grid <- all_data |> dplyr::distinct(facility, facility_code)
  analysis_grid <- merge(
    facility_grid, tibble::tibble(diagnosis_final = diagnoses), by = NULL, sort = FALSE
  ) |>
    tibble::as_tibble()
  counts <- x |>
    dplyr::count(facility, facility_code, diagnosis_final, name = "cases")

  result <- analysis_grid |>
    dplyr::left_join(counts, by = c("facility", "facility_code", "diagnosis_final")) |>
    dplyr::mutate(cases = tidyr::replace_na(cases, 0L)) |>
    dplyr::left_join(totals, by = c("facility", "facility_code")) |>
    dplyr::mutate(proportion_per_100_contacts = 100 * cases / pmax(all_contacts, 1))

  if (!is.null(denominators) && nrow(denominators) > 0) {
    by_code <- denominators |>
      dplyr::filter(has_value(facility_code)) |>
      dplyr::select(facility_code, population) |>
      dplyr::distinct(facility_code, .keep_all = TRUE)
    by_name <- denominators |>
      dplyr::filter(has_value(facility)) |>
      dplyr::select(facility, population_name = population) |>
      dplyr::distinct(facility, .keep_all = TRUE)
    result <- result |>
      dplyr::left_join(by_code, by = "facility_code") |>
      dplyr::left_join(by_name, by = "facility") |>
      dplyr::mutate(population = dplyr::coalesce(population, population_name)) |>
      dplyr::select(-population_name)
  } else {
    result$population <- NA_real_
  }

  result |>
    dplyr::group_by(diagnosis_final) |>
    dplyr::mutate(
      average_proportion = mean(proportion_per_100_contacts, na.rm = TRUE),
      sd_proportion = stats::sd(proportion_per_100_contacts, na.rm = TRUE),
      relative_to_average = proportion_per_100_contacts / pmax(average_proportion, .Machine$double.eps),
      z_score = dplyr::if_else(
        is.finite(sd_proportion) & sd_proportion > 0,
        (proportion_per_100_contacts - average_proportion) / sd_proportion,
        0
      ),
      cases_per_1000 = dplyr::if_else(
        !is.na(population) & population > 0, 1000 * cases / population, NA_real_
      ),
      variation_signal = dplyr::case_when(
        z_score >= 2 ~ "Muito acima das outras US",
        z_score <= -2 ~ "Muito abaixo das outras US",
        abs(z_score) >= 1 ~ "Variação relevante",
        TRUE ~ "Próximo da média"
      )
    ) |>
    dplyr::ungroup() |>
    dplyr::arrange(dplyr::desc(abs(z_score)), diagnosis_final, facility)
}

measles_classification_summary <- function(data, exclude_duplicates = TRUE) {
  x <- if (exclude_duplicates) analysis_deduplicate(data) else tibble::as_tibble(data)
  x |>
    dplyr::filter(measles_who_suspected) |>
    dplyr::count(measles_case_class, name = "cases") |>
    dplyr::arrange(dplyr::desc(cases))
}

optional_field_gaps <- function(data) {
  purrr::map_dfr(names(OPTIONAL_FIELD_MAP), function(field) {
    configured_uid <- OPTIONAL_FIELD_MAP[[field]]
    available <- field %in% names(data)
    complete <- if (available) safe_pct(has_value(data[[field]])) else 0
    tibble::tibble(
      field = field,
      label = unname(OPTIONAL_FIELD_LABELS[[field]] %||% field),
      uid_configured = nzchar(configured_uid),
      completeness = complete,
      implication = dplyr::case_when(
        field %in% c("rash_generalized", "cough", "coryza", "conjunctivitis") ~
          "Necessário para aplicar integralmente a definição clínica de sarampo",
        field %in% c("rash_onset_date", "epidemiological_link") ~
          "Necessário para confirmar ligação e janela temporal de surto de sarampo",
        field %in% c("measles_igm_result", "measles_pcr_result") ~
          "Necessário para classificação laboratorial específica de sarampo",
        field %in% c("acute_watery_diarrhoea", "severe_dehydration") ~
          "Necessário para a definição de caso suspeito de cólera",
        TRUE ~ "Melhora a classificação epidemiológica e a oportunidade da resposta"
      )
    )
  })
}
