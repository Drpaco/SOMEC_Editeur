# Guide utilisateur complet (FR)
## Script `interactive_mission_qc_console.R`

**Projet :** `SOMEC_Editeur`  
**Public visé :** analystes QA/QC des tables `missions`, `transects`, `observations`  
**Portée :** utilisation interactive du script, génération de corrections, validation croisée, production d’un script de fixes exécutable

---

## 1) Objectif du script

`interactive_mission_qc_console.R` est une console QA/QC interactive qui :

- charge les données SOMEC (`missions`, `transects`, `observations`) depuis Access (avec cache RDS),
- lit les anomalies pré-détectées par mission (`MissionReports/mission_issues.rds`),
- guide l’utilisateur mission par mission et variable par variable,
- propose des diagnostics et des snippets de correction R,
- permet d’ajouter les corrections retenues dans un script versionné :
  - `apply_qc_fixes_<version_accdb>.R`
- exécute ensuite les règles de **validation croisée** (R1 à R9) sur `observations`.

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
  Règles CV R1–R9 + boucle interactive de traitement CV.

### Données / contexte

- `GlobalContext/global_context.rds`  
  Baselines globales (catalogue, quantiles numériques, etc.).

- `MissionReports/mission_issues.rds`  
  Liste d’anomalies pré-calculées par mission/table/colonne.

- `GlobalContext/_cache/*.rds`  
  Cache local des tables Access.

### Documentation connexe

- `CROSS_VALIDATION_RULES.md`  
  Détail des règles R1–R9.

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

Règles prises en charge : **R1 à R9** (voir `CROSS_VALIDATION_RULES.md`).

Exemples :

- cohérence `id_transect` vs `in_transect`,
- cohérence `code_espece = RIEN`,
- présence des distances attendues selon protocole,
- contraintes `activite` ↔ `snapshot` (VOL/OUI, EAU/NON),
- cohérence `_rad` vs `_par` pour missions double protocole.

Pour certaines règles (R4, R6, R7), le script peut proposer un snippet prêt à ajouter.

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
3. Lancer le script en FR.
4. Choisir la plage de missions.
5. Pour chaque anomalie pertinente :
   - ouvrir les détails,
   - inspecter les lignes/date si nécessaire,
   - générer snippet,
   - ajouter au script de fixes uniquement si validé.
6. Traiter les violations CV à la fin de chaque mission.
7. Quitter quand la session QA est terminée.
8. Ouvrir `apply_qc_fixes_*.R`, relire.
9. Exécuter le script de fixes pour produire cache + ACCDB mis à jour.
10. Rejouer une passe QA rapide pour confirmer.

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

---

## 15) Scripts en interaction (vue d’ensemble)

Chaîne logique recommandée :

1. `catalog_loader.R`  
   ⟶ lit `RelCatalog.xlsx`, met à jour les objets catalogue et le mapping de référence (pas de sortie Excel générée).
2. `database_context_profiler_v1.R`  
   ⟶ produit/rafraîchit `GlobalContext/global_context.rds` **et** `GlobalContext/Global_Database_Context.xlsx` (contexte global), utilisé directement par `interactive_mission_qc_console.R`.
3. `mission_profiler_somerc_v6_3_4.R`  
   ⟶ produit/rafraîchit les rapports `MissionReports/<MISSION>.xlsx`, l’index `SOMEC_Mission_QAQC_Index_<YYYYMMDD>.xlsx` et les enjeux mission (`MissionReports/mission_issues.rds`).
4. `interactive_mission_qc_console.R`  
   ⟶ consomme `mission_issues.rds` + `global_context.rds`, applique l’analyse interactive et propose les fixes.
5. `qc_helpers.R` + `qc_cross_validation.R` (chargés par la console)  
   ⟶ gèrent l’écriture des correctifs et la validation croisée R1–R9.
6. `apply_qc_fixes_<version>.R` (généré)  
   ⟶ applique les corrections, met à jour les caches RDS, puis peut exporter une nouvelle base `.accdb`.

## 16) Références internes

- `catalog_loader.R`
- `database_context_profiler_v1.R`
- `mission_profiler_somerc_v6_3_4.R`
- `interactive_mission_qc_console.R`
- `qc_helpers.R`
- `qc_cross_validation.R`
- `CROSS_VALIDATION_RULES.md`
- `GlobalContext/global_context.rds`
- `MissionReports/mission_issues.rds`

---

Document restauré et consolidé pour l’usage complet du flux QA/QC interactif SOMEC.
