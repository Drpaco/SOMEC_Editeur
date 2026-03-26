# SOMEC cross-validation rules for the QC console.
#   Sourced by interactive_mission_qc_console.R
#   Requires: .lang, msg(), observations, transects, append_fix, view_in_viewer, prompt
# ============================================================

# Missions where BOTH _rad and _par are valid (all others: _par only)
.rad_par_both_missions <- c("HUD081026", "MLB090423", "HUD091025", "TEL100603", "HUD101107")

# Fields that must be NA when code_espece = RIEN
.rien_cols <- c("nb_individu", "activite", "tra_rad", "dis_rad",
                "tra_par", "dis_par", "association", "comportement",
                "direction", "dir_heure", "age", "plumage", "sexe")

# Normalize key observation columns for cross-validation
normalize_obs <- function(df) {
  df |>
    mutate(
      activite_n    = str_to_upper(str_trim(activite)),
      snapshot_n    = str_to_upper(str_trim(snapshot)),
      in_transect_n = str_to_upper(str_trim(in_transect)),
      espece_n      = str_to_upper(str_trim(code_espece))
    )
}

# Run all cross-validation rules for a given mission.
# Returns a named list of violation tibbles (one per rule), empty list if no violations.
run_cross_validation <- function(mission_id) {

  obs_mis <- observations |>
    as_tibble() |>
    filter(.data[["mission"]] == mission_id) |>
    normalize_obs()

  is_both_mission <- mission_id %in% .rad_par_both_missions
  results         <- list()

  # R1: id_transect NA but in_transect != HORS TRANSECT
  r1 <- obs_mis |>
    filter(is.na(id_transect), in_transect_n != "HORS TRANSECT") |>
    select(any_of(c("id", "date", "heure", "in_transect", "code_espece", "id_transect")))
  if (nrow(r1) > 0) results$R1 <- r1

  # R2: code_espece = RIEN but non-NA fields present
  r2 <- obs_mis |>
    filter(espece_n == "RIEN") |>
    filter(if_any(any_of(.rien_cols), ~ !is.na(.))) |>
    select(any_of(c("id", "date", "heure", "code_espece", .rien_cols)))
  if (nrow(r2) > 0) results$R2 <- r2

  # R3: code_espece != RIEN but nb_individu or activite NA
  r3 <- obs_mis |>
    filter(espece_n != "RIEN") |>
    filter(is.na(nb_individu) | is.na(activite)) |>
    select(any_of(c("id", "date", "heure", "code_espece",
                    "nb_individu", "activite", "snapshot", "in_transect")))
  if (nrow(r3) > 0) results$R3 <- r3

  # R4: code_espece != RIEN but missing expected distance data (protocol-aware)
  r4_base <- obs_mis |> filter(espece_n != "RIEN")
  r4 <- if (is_both_mission) {
    r4_base |>
      filter(is.na(tra_rad) & is.na(dis_rad) & is.na(tra_par) & is.na(dis_par))
  } else {
    r4_base |>
      filter(is.na(tra_par) & is.na(dis_par))
  }
  r4 <- r4 |>
    select(any_of(c("id", "date", "heure", "code_espece", "activite",
                    "tra_rad", "dis_rad", "tra_par", "dis_par")))
  if (nrow(r4) > 0) results$R4 <- r4

  # R5: _rad data present in _par-only mission
  if (!is_both_mission) {
    r5 <- obs_mis |>
      filter(espece_n != "RIEN") |>
      filter(!is.na(tra_rad) | !is.na(dis_rad)) |>
      select(any_of(c("id", "date", "heure", "code_espece",
                      "tra_rad", "dis_rad", "tra_par", "dis_par")))
    if (nrow(r5) > 0) results$R5 <- r5
  }

  # R6: activite=VOL but snapshot != OUI (normalized)
  r6 <- obs_mis |>
    filter(espece_n != "RIEN", activite_n == "VOL", snapshot_n != "OUI") |>
    select(any_of(c("id", "date", "heure", "code_espece",
                    "activite", "snapshot", "activite_n", "snapshot_n")))
  if (nrow(r6) > 0) results$R6 <- r6

  # R7: activite=EAU but snapshot != NON (normalized)
  r7 <- obs_mis |>
    filter(espece_n != "RIEN", activite_n == "EAU", snapshot_n != "NON") |>
    select(any_of(c("id", "date", "heure", "code_espece",
                    "activite", "snapshot", "activite_n", "snapshot_n")))
  if (nrow(r7) > 0) results$R7 <- r7

  # R8: in_transect or snapshot NA for non-RIEN rows
  r8 <- obs_mis |>
    filter(espece_n != "RIEN") |>
    filter(is.na(in_transect) | is.na(snapshot)) |>
    select(any_of(c("id", "date", "heure", "code_espece",
                    "activite", "snapshot", "in_transect")))
  if (nrow(r8) > 0) results$R8 <- r8

  # R9: _rad and _par values differ in both-protocol missions
  if (is_both_mission) {
    r9 <- obs_mis |>
      filter(espece_n != "RIEN") |>
      filter(!is.na(tra_rad) & !is.na(tra_par) & tra_rad != tra_par) |>
      select(any_of(c("id", "date", "heure", "code_espece", "activite",
                      "tra_rad", "dis_rad", "tra_par", "dis_par")))
    if (nrow(r9) > 0) results$R9 <- r9
  }

  # R10: WatchLenKm / vit_plateforme / duration inconsistency (transects, no plane missions)
  .plane_pattern <- "^(AVI|PAR|ISL)"
  if (!grepl(.plane_pattern, mission_id)) {
    t_mis <- transects |>
      as_tibble() |>
      filter(.data[["mission"]] == mission_id)

    req_cols <- c("id_transect", "site", "longitude", "latitude", "date_heure", "vit_plateforme")
    if (all(req_cols %in% names(t_mis))) {
      norm_site <- function(x) {
        y <- tolower(iconv(as.character(x), to = "ASCII//TRANSLIT"))
        dplyr::case_when(
          y %in% c("debut", "start") ~ "debut",
          y %in% c("fin",   "end")   ~ "fin",
          TRUE ~ NA_character_
        )
      }

      vit_stats <- t_mis |>
        filter(!is.na(vit_plateforme)) |>
        group_by(id_transect) |>
        summarise(
          vit_min_kn = min(vit_plateforme, na.rm = TRUE),
          vit_max_kn = max(vit_plateforme, na.rm = TRUE),
          vit_med_kn = median(vit_plateforme, na.rm = TRUE),
          .groups = "drop"
        )

      w <- t_mis |>
        mutate(site_norm = norm_site(site)) |>
        filter(site_norm %in% c("debut", "fin")) |>
        group_by(id_transect, site_norm) |>
        slice_head(n = 1) |>
        ungroup() |>
        select(id_transect, site_norm, longitude, latitude, date_heure) |>
        tidyr::pivot_wider(
          names_from  = site_norm,
          values_from = c(longitude, latitude, date_heure),
          names_sep   = "_"
        ) |>
        mutate(
          dt_debut = suppressWarnings(lubridate::as_datetime(date_heure_debut)),
          dt_fin   = suppressWarnings(lubridate::as_datetime(date_heure_fin)),
          WatchLenKm = geosphere::distHaversine(
            cbind(longitude_debut, latitude_debut),
            cbind(longitude_fin,   latitude_fin)
          ) / 1000,
          Duration_h = as.numeric(difftime(dt_fin, dt_debut, units = "hours")),
          Observed_kmh = if_else(!is.na(Duration_h) & Duration_h > 0,
                                 WatchLenKm / Duration_h, NA_real_)
        ) |>
        left_join(vit_stats, by = "id_transect") |>
        mutate(
          vit_min_kmh = vit_min_kn * 1.852,
          vit_max_kmh = vit_max_kn * 1.852,
          Vit_knots2kmh = vit_med_kn * 1.852,
          in_speed_range = !is.na(Observed_kmh) & !is.na(vit_min_kmh) & !is.na(vit_max_kmh) &
            Observed_kmh >= (vit_min_kmh * 0.9) &
            Observed_kmh <= (vit_max_kmh * 1.1),
          pct_diff = (Observed_kmh - Vit_knots2kmh) / Vit_knots2kmh * 100,
          flag = (!in_speed_range & abs(pct_diff) > 20) |
            (!is.na(Duration_h) & Duration_h < 0) |
            is.na(Vit_knots2kmh) |
            is.na(Observed_kmh),
          reason = dplyr::case_when(
            !is.na(Duration_h) & Duration_h < 0 ~ "duree_negative",
            is.na(Vit_knots2kmh) ~ "vit_plateforme_manquante",
            !is.na(vit_med_kn) & vit_med_kn <= 3 ~ "vit_tres_faible",
            !is.na(Duration_h) & Duration_h > 0 & Duration_h < 0.1 ~ "duree_tres_courte",
            TRUE ~ "diff_vitesse_elevee"
          )
        ) |>
        filter(flag) |>
        select(id_transect, WatchLenKm, Duration_h, Observed_kmh,
               vit_min_kn, vit_max_kn, vit_med_kn,
               Vit_knots2kmh, pct_diff, in_speed_range, reason)

      if (nrow(w) > 0) results$R10 <- w
    }
  }

  # R11: transect endpoint integrity (must have exactly one Début and one Fin)
  {
    t_mis <- transects |>
      as_tibble() |>
      filter(.data[["mission"]] == mission_id)

    req_cols_r11 <- c("id_transect", "site")
    if (all(req_cols_r11 %in% names(t_mis))) {
      norm_site <- function(x) {
        y <- tolower(iconv(as.character(x), to = "ASCII//TRANSLIT"))
        dplyr::case_when(
          y %in% c("debut", "start") ~ "debut",
          y %in% c("fin", "end") ~ "fin",
          TRUE ~ NA_character_
        )
      }

      r11 <- t_mis |>
        mutate(site_norm = norm_site(site)) |>
        group_by(id_transect) |>
        summarise(
          n_rows = n(),
          n_debut = sum(site_norm == "debut", na.rm = TRUE),
          n_fin = sum(site_norm == "fin", na.rm = TRUE),
          issue = case_when(
            n_debut == 1 & n_fin == 1 ~ "ok",
            n_debut == 0 & n_fin == 0 ~ "missing_both",
            n_debut == 0 ~ "missing_debut",
            n_fin == 0 ~ "missing_fin",
            n_debut > 1 | n_fin > 1 ~ "duplicate_debut_or_fin",
            TRUE ~ "other"
          ),
          .groups = "drop"
        ) |>
        filter(issue != "ok")

      if (nrow(r11) > 0) results$R11 <- r11
    }
  }

  results
}

# Human-readable rule descriptions (bilingual)
.cv_rule_desc <- list(
  R1  = c(e = "id_transect NA but in_transect != HORS TRANSECT",
          f = "id_transect manquant mais in_transect != HORS TRANSECT"),
  R2  = c(e = "code_espece = RIEN but non-NA fields present",
          f = "code_espece = RIEN mais champs non-NA presents"),
  R3  = c(e = "code_espece != RIEN but nb_individu or activite NA",
          f = "code_espece != RIEN mais nb_individu ou activite manquant"),
  R4  = c(e = "code_espece != RIEN but missing expected distance data",
          f = "code_espece != RIEN mais donnees de distance manquantes"),
  R5  = c(e = "_rad data present in _par-only mission",
          f = "Donnees _rad presentes dans une mission _par seulement"),
  R6  = c(e = "activite=VOL but snapshot != OUI",
          f = "activite=VOL mais snapshot != OUI"),
  R7  = c(e = "activite=EAU but snapshot != NON",
          f = "activite=EAU mais snapshot != NON"),
  R8  = c(e = "in_transect or snapshot NA for non-RIEN rows",
          f = "in_transect ou snapshot manquant pour lignes non-RIEN"),
  R9  = c(e = "_rad and _par values differ in both-protocol mission",
          f = "Valeurs _rad et _par differentes dans mission protocole double"),
  R10 = c(e = "WatchLenKm / vit_plateforme / duration inconsistency (transects)",
          f = "Incoherence WatchLenKm / vit_plateforme / duree (transects)"),
  R11 = c(e = "Transect endpoint integrity: missing/duplicate Début/Fin",
          f = "Integrite des extremites de transect: Debut/Fin manquant ou duplique")
)

# Explain a single CV rule violation and return a fix snippet if applicable
explain_cv_rule <- function(rule, df_violations, mission_id) {

  cat("\n--------------------------------------------\n")
  cat(msg("mission_label"), mission_id, "\n")
  cat(if (.lang == "f") "Regle : " else "Rule  : ", rule, "\n", sep = "")
  cat(if (.lang == "f") "Desc  : " else "Desc  : ",
      .cv_rule_desc[[rule]][[.lang]], "\n", sep = "")
  cat(if (.lang == "f") "Lignes: " else "Rows  : ", nrow(df_violations), "\n", sep = "")
  cat("--------------------------------------------\n\n")

  view_in_viewer(df_violations,
                 title = paste0(mission_id, " - ", rule, ": ",
                                .cv_rule_desc[[rule]][[.lang]]))

  snippet <- NULL

  # R4
  if (rule == "R4") {
    obs_mis_raw <- observations |>
      as_tibble() |>
      filter(.data[["mission"]] == mission_id) |>
      normalize_obs()
    has_rad_no_par <- obs_mis_raw |>
      filter(espece_n != "RIEN", !is.na(tra_rad) | !is.na(dis_rad),
             is.na(tra_par) & is.na(dis_par))
    if (nrow(has_rad_no_par) > 0) {
      cat(if (.lang == "f")
        paste0("  ", nrow(has_rad_no_par), " lignes ont des valeurs _rad mais pas de valeurs _par.\n")
      else
        paste0("  ", nrow(has_rad_no_par), " rows have _rad values but no _par values.\n"))
      snippet <- paste0(
        "# R4 fix: copy _rad into _par where _par is missing, then clear _rad\n",
        "observations <- observations |>\n",
        "  mutate(\n",
        "    tra_par = if_else(mission == \"", mission_id, "\" &\n",
        "      (is.na(tra_par) & is.na(dis_par)) &\n",
        "      (!is.na(tra_rad) | !is.na(dis_rad)),\n",
        "      tra_rad, tra_par),\n",
        "    dis_par = if_else(mission == \"", mission_id, "\" &\n",
        "      (is.na(tra_par) & is.na(dis_par)) &\n",
        "      (!is.na(tra_rad) | !is.na(dis_rad)),\n",
        "      dis_rad, dis_par),\n",
        "    tra_rad = if_else(mission == \"", mission_id, "\" &\n",
        "      !is.na(tra_par), NA_real_, tra_rad),\n",
        "    dis_rad = if_else(mission == \"", mission_id, "\" &\n",
        "      !is.na(dis_par), NA_real_, dis_rad)\n",
        "  )"
      )
      cat(if (.lang == "f") "CORRECTION SUGGEREE:\n\n" else "SUGGESTED FIX:\n\n")
      cat(snippet, "\n")
    }
  }

  # R6
  if (rule == "R6") {
    global_r6 <- observations |> as_tibble() |> normalize_obs() |>
      filter(activite_n == "VOL") |>
      count(mission, snapshot_pattern = snapshot_n, name = "n") |>
      arrange(mission, snapshot_pattern)
    cat(if (.lang == "f") "PATRON GLOBAL snapshot pour activite=VOL :\n"
        else              "GLOBAL snapshot pattern for activite=VOL:\n")
    print(global_r6, n = Inf)
    cat("\n")
    n_vol <- sum(normalize_obs(as_tibble(observations) |>
      filter(mission == mission_id))$activite_n == "VOL", na.rm = TRUE)
    pct_violation <- if (n_vol > 0) round(100 * nrow(df_violations) / n_vol) else 0
    cat(if (.lang == "f")
      paste0("  ", pct_violation, "% des lignes VOL sont en violation dans cette mission\n\n")
    else
      paste0("  ", pct_violation, "% of VOL rows are in violation in this mission\n\n"))
    snippet <- paste0(
      "observations <- observations |>\n",
      "  mutate(\n",
      "    snapshot = if_else(\n",
      "      mission == \"", mission_id, "\" &\n",
      "      str_to_upper(str_trim(activite)) == \"VOL\" &\n",
      "      str_to_upper(str_trim(snapshot)) != \"OUI\",\n",
      "      \"Oui\", snapshot\n",
      "    )\n",
      "  )"
    )
    cat(if (.lang == "f") "CORRECTION SUGGEREE:\n\n" else "SUGGESTED FIX:\n\n")
    cat(snippet, "\n")
  }

  # R7
  if (rule == "R7") {
    global_r7 <- observations |> as_tibble() |> normalize_obs() |>
      filter(activite_n == "EAU") |>
      count(mission, snapshot_pattern = snapshot_n, name = "n") |>
      arrange(mission, snapshot_pattern)
    cat(if (.lang == "f") "PATRON GLOBAL snapshot pour activite=EAU :\n"
        else              "GLOBAL snapshot pattern for activite=EAU:\n")
    print(global_r7, n = Inf)
    cat("\n")
    snippet <- paste0(
      "observations <- observations |>\n",
      "  mutate(\n",
      "    snapshot = if_else(\n",
      "      mission == \"", mission_id, "\" &\n",
      "      str_to_upper(str_trim(activite)) == \"EAU\" &\n",
      "      str_to_upper(str_trim(snapshot)) != \"NON\",\n",
      "      \"Non\", snapshot\n",
      "    )\n",
      "  )"
    )
    cat(if (.lang == "f") "CORRECTION SUGGEREE:\n\n" else "SUGGESTED FIX:\n\n")
    cat(snippet, "\n")
  }

  # R9
  if (rule == "R9") {
    cat(if (.lang == "f")
      "Les valeurs _rad et _par different - inspection manuelle requise.\n"
    else
      "_rad and _par values differ - manual inspection required.\n")
  }

  # R10
  if (rule == "R10") {
    x <- df_violations |>
      mutate(
        expected_duration_h = WatchLenKm / Vit_knots2kmh,
        delta_min = (Duration_h - expected_duration_h) * 60,
        abs_pct = abs(pct_diff),
        severity = case_when(
          abs_pct >= 100 ~ "critical",
          abs_pct >= 50 ~ "high",
          abs_pct >= 20 ~ "moderate",
          TRUE ~ "low"
        )
      )

    by_reason <- x |>
      count(reason, name = "n") |>
      mutate(pct = round(100 * n / sum(n), 1)) |>
      arrange(desc(n))

    cat("R10 = distance / duration / speed inconsistency.\n")
    cat("Interpretation: observed speed is compared against the range of recorded platform speeds within the transect (min-max), using the median speed as the reference for pct_diff.\n")
    cat("Threshold: flag only when observed speed falls outside the transect speed range (with 10% tolerance), or duration is negative, or speed info is missing.\n\n")

    cat("SUMMARY BY INCONSISTENCY REASON:\n")
    print(by_reason, n = Inf)

    cat("\nHow to read this:\n")
    cat("- vit_min_kn / vit_max_kn: minimum and maximum recorded platform speed within the transect\n")
    cat("- vit_med_kn: median recorded platform speed within the transect\n")
    cat("- in_speed_range: TRUE means observed speed is compatible with the transect speed range\n")
    cat("- expected_duration_h: expected duration from distance + median platform speed\n")
    cat("- delta_min: observed duration - expected duration (minutes)\n")
    cat("- pct_diff: relative speed difference (%) vs median platform speed\n")
    cat("- severity: potential impact level\n\n")

    detail <- x |>
      select(
        id_transect, reason, severity, WatchLenKm, Duration_h,
        vit_min_kn, vit_max_kn, vit_med_kn, in_speed_range,
        expected_duration_h, delta_min, Observed_kmh, Vit_knots2kmh, pct_diff
      ) |>
      arrange(desc(abs(pct_diff)))

    print(detail, n = min(20, nrow(detail)))

    cat("\nGUIDED ACTIONS:\n")
    cat("1) duree_negative: check swapped Start/End or timestamp error\n")
    cat("2) vit_plateforme_manquante: replace missing vit_plateforme with computed speed (Observed_kmh / 1.852) when acceptable\n")
    cat("3) duree_tres_courte: check time precision/rounding\n")
    cat("4) vit_tres_faible: check units or data entry\n")
    cat("5) diff_vitesse_elevee: inspect whether speed changed within the transect and whether the observed speed is still plausible given the full min-max range\n\n")

    ids <- paste(unique(x$id_transect), collapse = ", ")

    if (any(x$reason == "vit_plateforme_manquante")) {
      x_missing <- x |>
        filter(reason == "vit_plateforme_manquante") |>
        transmute(
          id_transect,
          vit_calc_kn = round(Observed_kmh / 1.852, 3)
        )

      rows_txt <- paste0(
        "    ",
        x_missing$id_transect,
        ", ",
        x_missing$vit_calc_kn,
        collapse = ",\n"
      )

      snippet <- paste0(
        "# R10 -- inspect flagged transects (mission ", mission_id, ")\n",
        "transects |> filter(mission == \"", mission_id, "\", id_transect %in% c(", ids, ")) |>\n",
        "  select(mission, id_transect, site, date_heure, longitude, latitude, vit_plateforme)\n\n",
        "# R10 -- replace missing vit_plateforme with computed speed in knots\n",
        "vit_impute <- tibble::tribble(\n",
        "  ~id_transect, ~vit_calc_kn,\n",
        rows_txt, "\n",
        ")\n\n",
        "transects <- transects |>\n",
        "  left_join(vit_impute, by = \"id_transect\") |>\n",
        "  mutate(\n",
        "    vit_plateforme = if_else(\n",
        "      mission == \"", mission_id, "\" & is.na(vit_plateforme) & !is.na(vit_calc_kn),\n",
        "      vit_calc_kn,\n",
        "      vit_plateforme\n",
        "    )\n",
        "  ) |>\n",
        "  select(-vit_calc_kn)\n"
      )
    } else {
      snippet <- paste0(
        "# R10 -- targeted inspection (mission ", mission_id, ")\n",
        "transects |> filter(mission == \"", mission_id, "\", id_transect %in% c(", ids, ")) |>\n",
        "  select(mission, id_transect, site, date_heure, longitude, latitude, vit_plateforme)\n"
      )
    }

    cat("SUGGESTED FIX:\n\n")
    cat(snippet, "\n")
    return(invisible(snippet))
  }

  # R11
  if (rule == "R11") {
    by_issue <- df_violations |>
      count(issue, name = "n") |>
      mutate(pct = round(100 * n / sum(n), 1)) |>
      arrange(desc(n))

    cat(if (.lang == "f")
      "R11 = verification de l'integrite des extremites de transect (Debut/Fin).\n"
    else
      "R11 = transect endpoint integrity check (Start/End).\n")
    cat(if (.lang == "f")
      "Objectif: chaque id_transect doit avoir exactement un Debut et une Fin.\n\n"
    else
      "Goal: each id_transect must have exactly one Start and one End.\n\n")

    print(by_issue, n = Inf)

    cat(if (.lang == "f")
      "\nLecture: missing_debut/missing_fin peuvent etre auto-completes; missing_both et duplicate_debut_or_fin demandent une verification manuelle.\n\n"
    else
      "\nReading: missing_debut/missing_fin can be auto-completed; missing_both and duplicate_debut_or_fin require manual review.\n\n")

    ids <- paste(unique(df_violations$id_transect), collapse = ", ")

    snippet <- paste0(
      "# R11 -- inspect transects with endpoint issues (mission ", mission_id, ")\n",
      "transects |> filter(mission == \"", mission_id, "\", id_transect %in% c(", ids, ")) |>\n",
      "  select(mission, id_transect, site, date_heure, longitude, latitude, vit_plateforme) |>\n",
      "  arrange(id_transect, date_heure)\n\n",
      "# R11 -- auto-create missing Debut/Fin rows for transects with exactly one missing endpoint\n",
      "norm_site <- function(x) {\n",
      "  y <- tolower(iconv(as.character(x), to = \"ASCII//TRANSLIT\"))\n",
      "  dplyr::case_when(\n",
      "    y %in% c(\"debut\", \"start\") ~ \"debut\",\n",
      "    y %in% c(\"fin\", \"end\") ~ \"fin\",\n",
      "    TRUE ~ NA_character_\n",
      "  )\n",
      "}\n\n",
      "t_mis <- transects |>\n",
      "  dplyr::filter(mission == \"", mission_id, "\") |>\n",
      "  dplyr::mutate(site_norm = norm_site(site), dt = suppressWarnings(lubridate::as_datetime(date_heure)))\n\n",
      "status <- t_mis |>\n",
      "  dplyr::group_by(id_transect) |>\n",
      "  dplyr::summarise(\n",
      "    n_debut = sum(site_norm == \"debut\", na.rm = TRUE),\n",
      "    n_fin = sum(site_norm == \"fin\", na.rm = TRUE),\n",
      "    issue = dplyr::case_when(\n",
      "      n_debut == 0 & n_fin > 0 ~ \"missing_debut\",\n",
      "      n_fin == 0 & n_debut > 0 ~ \"missing_fin\",\n",
      "      TRUE ~ \"other\"\n",
      "    ),\n",
      "    .groups = \"drop\"\n",
      "  ) |>\n",
      "  dplyr::filter(issue %in% c(\"missing_debut\", \"missing_fin\"), id_transect %in% c(", ids, "))\n\n",
      "first_rows <- t_mis |>\n",
      "  dplyr::arrange(id_transect, is.na(dt), dt) |>\n",
      "  dplyr::group_by(id_transect) |>\n",
      "  dplyr::slice_head(n = 1) |>\n",
      "  dplyr::ungroup()\n\n",
      "last_rows <- t_mis |>\n",
      "  dplyr::arrange(id_transect, is.na(dt), dplyr::desc(dt)) |>\n",
      "  dplyr::group_by(id_transect) |>\n",
      "  dplyr::slice_head(n = 1) |>\n",
      "  dplyr::ungroup()\n\n",
      "rows_debut <- status |>\n",
      "  dplyr::filter(issue == \"missing_debut\") |>\n",
      "  dplyr::inner_join(first_rows, by = \"id_transect\") |>\n",
      "  dplyr::mutate(site = \"Début\")\n\n",
      "rows_fin <- status |>\n",
      "  dplyr::filter(issue == \"missing_fin\") |>\n",
      "  dplyr::inner_join(last_rows, by = \"id_transect\") |>\n",
      "  dplyr::mutate(site = \"Fin\")\n\n",
      "rows_to_add <- dplyr::bind_rows(rows_debut, rows_fin) |>\n",
      "  dplyr::select(-n_debut, -n_fin, -issue, -site_norm, -dt)\n\n",
      "transects <- dplyr::bind_rows(transects, rows_to_add)\n"
    )

    cat(if (.lang == "f") "CORRECTION SUGGEREE:\n\n" else "SUGGESTED FIX:\n\n")
    cat(snippet, "\n")
    return(invisible(snippet))
  }

  invisible(snippet)
}

# Interactive CV loop for a single mission (called from run_interactive_qc)
run_cv_for_mission <- function(mission_id) {

  cv <- run_cross_validation(mission_id)

  if (length(cv) == 0) {
    cat(if (.lang == "f")
      "  Aucune violation de validation croisee detectee.\n"
    else
      "  No cross-validation violations found.\n")
    return(invisible(NULL))
  }

  rules <- names(cv)
  cat(if (.lang == "f")
    paste0("\n  ", length(rules), " regle(s) violee(s) : ", paste(rules, collapse = ", "), "\n")
  else
    paste0("\n  ", length(rules), " cross-validation rule(s) violated: ", paste(rules, collapse = ", "), "\n"))

  for (rule in rules) {
    df_viol <- cv[[rule]]
    cat(sprintf("\n  [%s] %d rows -- %s\n",
        rule, nrow(df_viol), .cv_rule_desc[[rule]][[.lang]]))

    choice <- prompt(
      if (.lang == "f") "  Options : [v]oir / [s]auter > "
      else              "  Options: [v]iew / [s]kip > ",
      c("v", "s")
    )

    if (choice == "v") {
      snippet <- explain_cv_rule(rule, df_viol, mission_id)
      if (!is.null(snippet)) {
        repeat {
          post <- prompt(
            if (.lang == "f") "  Options : [a]jouter correction / [Entree] continuer > "
            else              "  Options: [a]dd fix to script / [Enter] continue > ",
            c("a", "")
          )
          if (post == "a") {
            fix_table <- if (rule %in% c("R10", "R11")) "transects" else "observations"
            append_fix(snippet, mission_id, paste0("CV_", rule), fix_table, rule)
            break
          } else {
            break
          }
        }
      } else {
        prompt(if (.lang == "f") "Appuyer sur [Entree] pour continuer..."
               else              "Press [Enter] to continue...")
      }
    }
  }
}
