# ============================================================
# FILE: qc_cross_validation.R
# PURPOSE: Cross-validation rules for the SOMEC QC console.
#   Sourced by interactive_mission_qc_console.R
#   Requires: .lang, msg(), observations, append_fix, view_in_viewer, prompt
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

  results
}

# Human-readable rule descriptions (bilingual)
.cv_rule_desc <- list(
  R1 = c(e = "id_transect NA but in_transect != HORS TRANSECT",
         f = "id_transect manquant mais in_transect != HORS TRANSECT"),
  R2 = c(e = "code_espece = RIEN but non-NA fields present",
         f = "code_espece = RIEN mais champs non-NA presents"),
  R3 = c(e = "code_espece != RIEN but nb_individu or activite NA",
         f = "code_espece != RIEN mais nb_individu ou activite manquant"),
  R4 = c(e = "code_espece != RIEN but missing expected distance data",
         f = "code_espece != RIEN mais donnees de distance manquantes"),
  R5 = c(e = "_rad data present in _par-only mission",
         f = "Donnees _rad presentes dans une mission _par seulement"),
  R6 = c(e = "activite=VOL but snapshot != OUI",
         f = "activite=VOL mais snapshot != OUI"),
  R7 = c(e = "activite=EAU but snapshot != NON",
         f = "activite=EAU mais snapshot != NON"),
  R8 = c(e = "in_transect or snapshot NA for non-RIEN rows",
         f = "in_transect ou snapshot manquant pour lignes non-RIEN"),
  R9 = c(e = "_rad and _par values differ in both-protocol mission",
         f = "Valeurs _rad et _par differentes dans mission protocole double")
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

  # R4: offer dis_par <- dis_rad fix when dis_par is missing but dis_rad exists
  if (rule == "R4") {
    obs_mis_raw <- observations |>
      as_tibble() |>
      filter(.data[["mission"]] == mission_id) |>
      normalize_obs()

    has_rad_no_par <- obs_mis_raw |>
      filter(espece_n != "RIEN",
             !is.na(tra_rad) | !is.na(dis_rad),
             is.na(tra_par) & is.na(dis_par))

    if (nrow(has_rad_no_par) > 0) {
      cat(if (.lang == "f")
        paste0("  ", nrow(has_rad_no_par),
               " lignes ont des valeurs _rad mais pas de valeurs _par.\n")
      else
        paste0("  ", nrow(has_rad_no_par),
               " rows have _rad values but no _par values.\n"))

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

  # R6: show global VOL/snapshot pattern + suggest fix
  if (rule == "R6") {
    global_r6 <- observations |>
      as_tibble() |>
      normalize_obs() |>
      filter(activite_n == "VOL") |>
      count(mission, snapshot_pattern = snapshot_n, name = "n") |>
      arrange(mission, snapshot_pattern)

    cat(if (.lang == "f")
      "PATRON GLOBAL snapshot pour activite=VOL :\n"
    else
      "GLOBAL snapshot pattern for activite=VOL:\n")
    print(global_r6, n = Inf)
    cat("\n")

    n_vol <- sum(
      normalize_obs(as_tibble(observations) |>
        filter(mission == mission_id))$activite_n == "VOL",
      na.rm = TRUE
    )
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

  # R7: show global EAU/snapshot pattern + suggest fix
  if (rule == "R7") {
    global_r7 <- observations |>
      as_tibble() |>
      normalize_obs() |>
      filter(activite_n == "EAU") |>
      count(mission, snapshot_pattern = snapshot_n, name = "n") |>
      arrange(mission, snapshot_pattern)

    cat(if (.lang == "f")
      "PATRON GLOBAL snapshot pour activite=EAU :\n"
    else
      "GLOBAL snapshot pattern for activite=EAU:\n")
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

  # R9: manual inspection required
  if (rule == "R9") {
    cat(if (.lang == "f")
      "Les valeurs _rad et _par different - inspection manuelle requise.\n"
    else
      "_rad and _par values differ - manual inspection required.\n")
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
    paste0("\n  ", length(rules), " regle(s) violee(s) : ",
           paste(rules, collapse = ", "), "\n")
  else
    paste0("\n  ", length(rules), " cross-validation rule(s) violated: ",
           paste(rules, collapse = ", "), "\n"))

  for (rule in rules) {

    df_viol <- cv[[rule]]
    cat(sprintf("\n  [%s] %d rows — %s\n",
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
            if (.lang == "f")
              "  Options : [a]jouter correction / [Entree] continuer > "
            else
              "  Options: [a]dd fix to script / [Enter] continue > ",
            c("a", "")
          )
          if (post == "a") {
            append_fix(snippet, mission_id, paste0("CV_", rule), "observations", rule)
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
