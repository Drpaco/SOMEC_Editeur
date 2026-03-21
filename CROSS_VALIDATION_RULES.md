# Cross-Validation Rules — observations table
**Script:** `interactive_mission_qc_console.R`  
**Last updated:** 2026-03-21  

---

## Protocol context

| Protocol type | Missions | Distance variables |
|---|---|---|
| `_rad` and `_par` both valid | HUD081026, MLB090423, HUD091025, TEL100603, HUD101107 | `tra_rad`, `dis_rad`, `tra_par`, `dis_par` |
| `_par` only (all others) | All other missions | `tra_par`, `dis_par` only |

---

## Rules

### R1 — Missing transect ID outside transect
- **Condition:** `is.na(id_transect)` AND `in_transect != "HORS TRANSECT"` (normalized)
- **Expected:** Every observation recorded during a transect must have a transect ID
- **Exception:** Rows where `in_transect = "HORS TRANSECT"` legitimately have no transect ID

---

### R2 — RIEN row with non-NA species fields
- **Condition:** `code_espece = "RIEN"` AND any of the following are NOT NA:
  `nb_individu`, `activite`, `tra_rad`, `dis_rad`, `tra_par`, `dis_par`,
  `association`, `comportement`, `direction`, `dir_heure`, `age`, `plumage`, `sexe`
- **Expected:** RIEN rows represent "nothing seen" — all species-level fields must be NA

---

### R3 — Non-RIEN row missing core fields
- **Condition:** `code_espece != "RIEN"` AND (`is.na(nb_individu)` OR `is.na(activite)`)
- **Expected:** Every real observation must have at least a count and an activity code

---

### R4 — Non-RIEN row missing distance data (protocol-aware)
- **Condition (both-protocol missions):** `code_espece != "RIEN"` AND all of
  `tra_rad`, `dis_rad`, `tra_par`, `dis_par` are NA
- **Condition (_par-only missions):** `code_espece != "RIEN"` AND both
  `tra_par` and `dis_par` are NA
- **Expected:** Every real observation must have distance data appropriate to its protocol

---

### R5 — Radial data in a _par-only mission
- **Condition:** Mission is NOT in the both-protocol list AND
  (`!is.na(tra_rad)` OR `!is.na(dis_rad)`) for non-RIEN rows
- **Expected:** Radial distance variables should be empty in _par-only missions
- **Note:** Rule not applied to both-protocol missions

---

### R6 — VOL activity with wrong snapshot value
- **Condition:** `code_espece != "RIEN"` AND `activite = "VOL"` (normalized) AND
  `snapshot != "OUI"` (normalized)
- **Expected:** Flying birds (`VOL`) must be flagged as snapshot observations (`Oui`)
- **Note:** Comparison is case-insensitive (normalized to upper case before check)
- **Fix:** `if_else(mission == X & activite == "VOL" & snapshot != "OUI", "Oui", snapshot)`

---

### R7 — EAU activity with wrong snapshot value
- **Condition:** `code_espece != "RIEN"` AND `activite = "EAU"` (normalized) AND
  `snapshot != "NON"` (normalized)
- **Expected:** Birds on water (`EAU`) must NOT be flagged as snapshot observations (`Non`)
- **Note:** Comparison is case-insensitive (normalized to upper case before check)
- **Fix:** `if_else(mission == X & activite == "EAU" & snapshot != "NON", "Non", snapshot)`

---

### R8 — Missing in_transect or snapshot for non-RIEN rows
- **Condition:** `code_espece != "RIEN"` AND (`is.na(in_transect)` OR `is.na(snapshot)`)
- **Expected:** Every real observation must have both `in_transect` and `snapshot` recorded

---

### R9 — Radial and parallel distances differ (both-protocol missions only)
- **Condition:** Mission IS in the both-protocol list AND
  `!is.na(tra_rad)` AND `!is.na(tra_par)` AND `tra_rad != tra_par`
- **Expected:** In both-protocol missions, `tra_rad` and `tra_par` should be identical
  (one is a copy of the other)
- **Note:** Violations require manual inspection — no automated fix suggested
- **Rule not applied to:** _par-only missions

---

## Sentinel values (not flagged as errors)

| Variable | Sentinel | Meaning | Valid when |
|---|---|---|---|
| `association` | `999` | Not applicable | `code_espece = RIEN` or `activite IN (VOL, EAU, NAVIRE)` |
| `comportement` | `999` | Not applicable | Same as above |
| `dir_heure` | `999` | Not applicable | Same as above |
| `direction` | `"ND"` | No direction | `activite IN (VOL, EAU, NAVIRE)` or `code_espece = RIEN` |

---

## Adding new rules

To add a new rule to the script:

1. Add the rule logic in `run_cross_validation()` following the existing pattern:
   ```r
   r10 <- obs_mis |>
     filter(...) |>
     select(any_of(c("id", "date", "heure", ...)))
   if (nrow(r10) > 0) results$R10 <- r10
   ```

2. Add the rule description to `explain_cv_rule()` in the `rule_desc` list:
   ```r
   R10 = c(e = "English description", f = "Description en français")
   ```

3. Add fix snippet logic inside `explain_cv_rule()` if applicable.

4. Update this document.
