# Guide utilisateur complet (FR)
## Script `interactive_mission_qc_console.R`

**Projet :** `SOMEC_Editeur`  
**Public visé :** analystes QA/QC des tables `missions`, `transects`, `observations`  
**Portée :** utilisation interactive du script, génération de corrections, validation croisée (R1–R11), production d’un script de fixes exécutable et synthèse globale des erreurs mission

---

## 1) Objectif du script

`interactive_mission_qc_console.R` est une console QA/QC interactive qui :

- charge les données SOMEC (`missions`, `transects`, `observations`) depuis Access (avec cache RDS),
- lit les anomalies pré-détectées par mission (`MissionReports/mission_issues.rds`),
- guide l’utilisateur mission par mission et variable par variable,
- propose des diagnostics et des snippets de correction R,
- permet d’ajouter les corrections retenues dans un script versionné :
  - `apply_qc_fixes_<version_accdb>.R`
- exécute ensuite les règles de **validation croisée** (R1 à R11) sur `observations` et `transects`.

Le script est conçu pour garder une trace claire des décisions QA.

---

## 2) Fichiers du dépôt impliqués

### Scripts amont (à relancer si la base de données source change)

- `catalog_loader.R`  
  Recharge les catalogues de référence (`RelCatalog.xlsx` + Access) et reconstruit le mapping catalogue.

- `database_context_profiler_v1.R`  
  Recalcule le contexte global de la base et produit `GlobalContext/global_context.rds` (catalog map, baselines numériques globales, etc.) consommé par la console interactive.

- `mission_profiler_somerc_v6_3_4.R`  
  Recalcule le profilage mission et régénère les enjeux/anomalies (`MissionReports/mission_issues.rds`).

> **Important :** si la base Access de départ change (nouvelle version `.accdb` ou données modifiées), il faut rerouler ces trois scripts avant de relancer la console interactive, sinon le contexte global et la liste des enjeux peuvent être désynchronisés.

### Scripts principaux

- `interactive_mission_qc_console.R`  
  Orchestrateur principal (menus, explications, flux mission).

- `qc_helpers.R`  
  Fonctions utilitaires : prompt, viewer HTML, génération/append des fixes, blocs d’ouverture/fermeture.

- `qc_cross_validation.R`  
  Règles CV R1–R11 + boucle interactive de traitement CV.

- `mission_error_inventory.R`  
  Agrège les erreurs (profiler + CV), calcule un signal d’import suspect et exporte un sommaire Excel mission.

### Données / contexte

- `GlobalContext/global_context.rds`  
  Baselines globales (catalogue, quantiles numériques, etc.).

- `MissionReports/mission_issues.rds`  
  Liste d’anomalies pré-calculées par mission/table/colonne.

- `GlobalContext/_cache/*.rds`  
  Cache local des tables Access.

### Documentation connexe

- `CROSS_VALIDATION_RULES.md`  
  Détail des règles R1–R11.

### Sortie générée

- `apply_qc_fixes_SOMEC_<date/version>.R`  
  Script de corrections consolidé, exécutable ensuite.

### 2.1 Sorties Excel utiles en parallèle

Pour les utilisateurs qui veulent consulter des rapports en parallèle de la console interactive :

- `catalog_loader.R`  
  - **Entrée Excel** : `RelCatalog.xlsx` (source de mapping).  
  - **Sortie Excel** : **aucune sortie `.xlsx` générée** (script de chargement/mapping en mémoire).

- `database_context_profiler_v1.R`  
  - **Sortie Excel** : `GlobalContext/Global_Database_Context.xlsx`  
  - **Contenu** : contexte global base complète (résumés QA globaux, baselines et diagnostics consolidés) utilisé comme référence d’analyse.

- `mission_profiler_somerc_v6_3_4.R`  
  - **Sorties Excel mission** : `MissionReports/<MISSION>.xlsx` (un classeur par mission).  
  - **Sortie Excel index** : `MissionReports/SOMEC_Mission_QAQC_Index_<YYYYMMDD>.xlsx` (index global des rapports mission).

- `mission_error_inventory.R`
  - **Sortie Excel synthèse** : `MissionReports/mission_error_summary_<ACCDB_TAG>.xlsx`
  - **Onglets** : `Summary`, `Long`, `ImportSuspected`
  - **Usage** : repérer rapidement les missions avec volume d’erreurs anormal (`import_issue_suspected`) en parallèle de la console interactive.

Ces fichiers peuvent être ouverts pendant la révision dans `interactive_mission_qc_console.R` pour comparer rapidement une anomalie avec son contexte mission/global.

---

## 3) Prérequis

## 3.1 Environnement

- **Positron recommandé** (mode interactif fiable avec `readline()`).
- R 4.x.
- Sur Windows pour la création `.accdb` finale via `RODBC`.

## 3.2 Packages R requis

- `tidyverse`
- `lubridate`
- `janitor`
- `RODBC`
- `stringdist`
- `base64enc`

## 3.3 Prérequis opérationnels si la base change

Si la base Access source est mise à jour, remplacée, ou modifiée, exécuter dans cet ordre :

1. `catalog_loader.R`
2. `database_context_profiler_v1.R`
3. `mission_profiler_somerc_v6_3_4.R`
4. `interactive_mission_qc_console.R`

Objectif : garantir que `global_context.rds`, `Global_Database_Context.xlsx`, les rapports `MissionReports/*.xlsx`, l’index mission et `mission_issues.rds` correspondent bien à la base courante.

## 3.4 Contrainte importante (RStudio)

Le script détecte et bloque le cas suivant :

- source complète via `source()` dans RStudio (car `readline()` n’est pas fiable dans ce mode).

Utilisation autorisée :

- exécution ligne par ligne dans RStudio,
- ou exécution normale dans Positron.

---

## 4) Démarrage

Depuis la racine du dépôt `SOMEC_Editeur` :

```r
source("interactive_mission_qc_console.R")
```

Au lancement, le script :

1. demande la langue (`e` / `f`),
2. résout la racine du repo,
3. charge les 3 tables via cache/Access,
4. charge le contexte global,
5. charge `mission_issues.rds`,
6. démarre la boucle interactive QA.

## 4.1 Auto-détection des dossiers et de la base (court)

Le script détecte automatiquement :

- la racine repo (`.repo_root`) depuis le script/fichier actif, sinon `getwd()`,
- le dossier Access (`db_dir`) 2 niveaux au-dessus de `SOMEC_Editeur`,
- la base ACCDB avec cette priorité :
  1. `SOMEC_ACCDB_PATH`,
  2. `SOMEC_ACCDB_SUFFIX`,
  3. dernier fichier `SOMEC_YYYYMMDD.accdb` modifié,
  4. saisie interactive.

En cas de doute, forcer explicitement `SOMEC_ACCDB_PATH`.

---

## 5) Sélection des missions

Menu initial :

- `[1]` Commencer depuis le début
- `[2]` Commencer à une mission spécifique
- `[3]` Traiter une seule mission

Ensuite, pour chaque mission, les anomalies sont listées séquentiellement.

---

## 6) Types d’anomalies traités

Le script gère principalement :

- `UNKNOWN_CATALOG` (catégoriel),
- `NUMERIC_OUTLIER` (numérique),
- `OUTSIDE_MISSION_DATES` (date/heure hors bornes mission).

Pour chaque anomalie, l’écran montre :

- mission,
- type d’anomalie,
- variable ciblée (`table$colonne`),
- résumés statistiques/contextuels,
- lignes problématiques dans le viewer.

---

## 7) Navigation interactive par anomalie

Pour chaque item :

- `[v]` voir les détails
- `[s]` sauter
- `[n]` passer à la mission suivante
- `[q]` quitter

Après affichage détaillé, selon le cas :

- `[d]` inspecter une journée (toutes lignes d’une date),
- `[f]` générer des options de fix (numérique),
- `[a]` ajouter la/les correction(s) au script de fixes,
- `[Entrée]` continuer.

---

## 8) Détail des comportements par type

## 8.1 `UNKNOWN_CATALOG`

Le script :

- affiche la distribution des modalités dans la mission,
- isole les NA éventuels,
- compare chaque modalité inconnue au catalogue attendu,
- propose une correction par proximité (`stringdist`, distance de Levenshtein).

Actions possibles :

- ajouter snippet de `recode` (`case_when`),
- gérer NA (laisser tel quel ou valeur personnalisée).

## 8.2 `NUMERIC_OUTLIER`

Le script :

- calcule la distribution mission (n, NA, min, médiane, max),
- récupère la baseline globale (`p05`, `p50`, `p95`),
- identifie les outliers mission vs baseline,
- ouvre un viewer avec tableau + mini graphe (PNG embarqué).

Actions possibles :

- traiter les NA :
  - laisser NA,
  - imputer par moyenne mission,
  - imputer par médiane mission,
  - valeur personnalisée,
- générer un template pour outliers (décision manuelle du réviseur).

## 8.3 `OUTSIDE_MISSION_DATES`

Le script :

- calcule les bornes mission normalisées (`00:00:00` → `23:59:59`),
- liste les enregistrements hors bornes,
- propose un correctif standard :
  - conserver l’heure ligne,
  - remplacer la date par la date de référence (`date`) de la ligne.

---

## 9) Validation croisée (`qc_cross_validation.R`)

Après chaque mission, le script lance automatiquement :

```r
run_cv_for_mission(<mission>)
```

Règles prises en charge : **R1 à R11** (voir `CROSS_VALIDATION_RULES.md`).

Exemples :

- cohérence `id_transect` vs `in_transect`,
- cohérence `code_espece = RIEN`,
- présence des distances attendues selon protocole,
- contraintes `activite` ↔ `snapshot` (VOL/OUI, EAU/NON),
- cohérence `_rad` vs `_par` pour missions double protocole,
- cohérence distance/durée/vitesse sur transects (R10),
- intégrité des extrémités `Début`/`Fin` de transects (R11).

Pour certaines règles (dont R4, R6, R7, R10, R11), le script peut proposer un snippet prêt à ajouter.

## 9.1 Résumé rapide R1–R11

- **R1** : `id_transect` manquant hors `HORS TRANSECT`.
- **R2** : `code_espece = RIEN` avec champs espèce non-NA.
- **R3** : `code_espece != RIEN` avec `nb_individu` ou `activite` manquant.
- **R4** : distances attendues manquantes selon protocole.
- **R5** : données `_rad` présentes dans mission `_par`-only.
- **R6** : `activite = VOL` mais `snapshot != OUI`.
- **R7** : `activite = EAU` mais `snapshot != NON`.
- **R8** : `in_transect` ou `snapshot` manquant sur non-RIEN.
- **R9** : divergence `_rad` vs `_par` (double protocole).
- **R10** : incohérence transect distance/durée/vitesse.
- **R11** : intégrité `Début`/`Fin` (manquant/dupliqué).

## 9.2 Note pratique R10

R10 n’est pas appliquée aux missions avion (`^AVI|^PAR|^ISL`).

---

## 10) Gestion du script de corrections

Quand on ajoute une correction (`[a]`), `append_fix()` :

1. crée (ou réutilise) `apply_qc_fixes_<version>.R`,
2. propose :
   - conserver script existant (`k`) ou repartir propre (`n`),
3. propose d’ajouter un bloc d’ouverture (chargement cache),
4. ajoute un bloc horodaté par anomalie :
   - mission, type, table, colonne,
5. propose d’ajouter/rafraîchir le bloc de fermeture.

## 10.1 Bloc d’ouverture

Charge `missions`, `transects`, `observations` depuis `GlobalContext/_cache`.

## 10.2 Bloc de fermeture

- sauvegarde les RDS mis à jour,
- sous Windows :
  - copie l’ACCDB source vers une nouvelle version datée,
  - réécrit les 3 tables via `RODBC::sqlSave()`.

---

## 11) Flux recommandé (opérationnel)

1. **Si la base a changé**, rerouler `catalog_loader.R`, puis `database_context_profiler_v1.R`, puis `mission_profiler_somerc_v6_3_4.R`.
2. Vérifier les sorties parallèles :
   - `GlobalContext/Global_Database_Context.xlsx`
   - `MissionReports/<MISSION>.xlsx`
   - `MissionReports/SOMEC_Mission_QAQC_Index_<YYYYMMDD>.xlsx`
3. (Option recommandé) Lancer `mission_error_inventory.R` pour générer `MissionReports/mission_error_summary_<ACCDB_TAG>.xlsx`.
4. Lancer le script en FR.
5. Choisir la plage de missions.
6. Pour chaque anomalie pertinente :
   - ouvrir les détails,
   - inspecter les lignes/date si nécessaire,
   - générer snippet,
   - ajouter au script de fixes uniquement si validé.
7. Traiter les violations CV à la fin de chaque mission.
8. Quitter quand la session QA est terminée.
9. Ouvrir `apply_qc_fixes_*.R`, relire.
10. Exécuter le script de fixes pour produire cache + ACCDB mis à jour.
11. Rejouer `mission_error_inventory.R` et une passe QA rapide pour confirmer la réduction des enjeux.

## 11.1 Utilisation de `force_rebuild` / `force_rebuild_issues`

Dans `mission_profiler_somerc_v6_3_4.R` :

- `force_rebuild = TRUE` : reconstruit tous les Excel + index + `mission_issues.rds`.
- `force_rebuild = FALSE` et `force_rebuild_issues = TRUE` : met à jour `mission_issues.rds` sans réécrire les Excel.
- `force_rebuild = FALSE` et `force_rebuild_issues = FALSE` : réutilise les sorties existantes.

Usage conseillé :

- en itération QA : `force_rebuild_issues = TRUE`,
- avant diffusion : `force_rebuild = TRUE`.

## 11.2 Autres edits potentiels utiles

- `options(somec.mission_filter = c("MISSION1", "MISSION2"))` pour profiler un sous-ensemble.
- Ajuster `cfg$skip_pattern` (colonnes commentaire ignorées).
- Ajuster `cfg$top_unknowns_per_col` / `cfg$max_levels_show` (lisibilité rapports).
- Utiliser `SOMEC_ACCDB_PATH` pour verrouiller la base cible.

---

## 12) Bonnes pratiques QA

- Favoriser des corrections **minimales et traçables**.
- Vérifier les missions sensibles avant imputation automatique.
- Pour outliers numériques : valider l’unité avant modification.
- Utiliser la validation croisée comme garde-fou post-fix.
- Conserver l’historique des scripts `apply_qc_fixes_*`.

---

## 13) Dépannage

## 13.1 Le script s’arrête au démarrage dans RStudio

Cause : exécution via `source()` non interactive pour `readline()`.  
Solution :

- exécuter ligne par ligne,
- ou utiliser Positron.

## 13.2 Fichier Access introuvable

Vérifier la version et l’arborescence attendue :

- `BaseDeDonnees/SOMEC_20251106.accdb` (selon config courante du script).

Si nécessaire, forcer la sélection via :

- `SOMEC_ACCDB_PATH`, ou
- `SOMEC_ACCDB_SUFFIX`.

## 13.3 Viewer ne s’ouvre pas

Le viewer dépend de `rstudioapi::viewer()`.  
Dans ce cas, continuer via console et inspecter les objets manuellement.

## 13.4 Erreurs RODBC / export ACCDB

- Exécuter sur Windows,
- vérifier pilotes Access/ODBC,
- vérifier droits en écriture dans `BaseDeDonnees/`.

---

## 14) Résumé rapide des commandes utiles

### Lancer la session QA

```r
source("interactive_mission_qc_console.R")
```

### Rejouer uniquement les validations croisées pour une mission (après chargement des objets)

```r
run_cv_for_mission("HUD101107")
```

### Exécuter le script de fixes généré

```r
source("apply_qc_fixes_SOMEC_20251106.R")
```

(adapter le nom selon le fichier réellement créé)

### Générer la synthèse erreurs mission

```r
source("mission_error_inventory.R")
```

---

## 15) Scripts en interaction (vue d’ensemble)

Chaîne logique recommandée :

1. `catalog_loader.R`  
   ⟶ lit `RelCatalog.xlsx`, met à jour les objets catalogue et le mapping de référence (pas de sortie Excel générée).
2. `database_context_profiler_v1.R`  
   ⟶ produit/rafraîchit `GlobalContext/global_context.rds` **et** `GlobalContext/Global_Database_Context.xlsx` (contexte global), utilisé directement par `interactive_mission_qc_console.R`.
3. `mission_profiler_somerc_v6_3_4.R`  
   ⟶ produit/rafraîchit les rapports `MissionReports/<MISSION>.xlsx`, l’index `SOMEC_Mission_QAQC_Index_<YYYYMMDD>.xlsx` et les enjeux mission (`MissionReports/mission_issues.rds`).
4. `mission_error_inventory.R`
   ⟶ consolide `mission_issues` + règles CV (`run_cross_validation`) et exporte `MissionReports/mission_error_summary_<ACCDB_TAG>.xlsx`.
5. `interactive_mission_qc_console.R`  
   ⟶ consomme `mission_issues.rds` + `global_context.rds`, applique l’analyse interactive et propose les fixes.
6. `qc_helpers.R` + `qc_cross_validation.R` (chargés par la console)  
   ⟶ gèrent l’écriture des correctifs et la validation croisée R1–R11.
7. `apply_qc_fixes_<version>.R` (généré)  
   ⟶ applique les corrections, met à jour les caches RDS, puis peut exporter une nouvelle base `.accdb`.

## 16) Références internes

- `catalog_loader.R`
- `database_context_profiler_v1.R`
- `mission_profiler_somerc_v6_3_4.R`
- `interactive_mission_qc_console.R`
- `qc_helpers.R`
- `qc_cross_validation.R`
- `mission_error_inventory.R`
- `CROSS_VALIDATION_RULES.md`
- `GlobalContext/global_context.rds`
- `MissionReports/mission_issues.rds`
- `MissionReports/mission_error_summary_<ACCDB_TAG>.xlsx`

---

Document restauré et consolidé pour l’usage complet du flux QA/QC interactif SOMEC.
