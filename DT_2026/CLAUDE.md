# Consignes pour Claude : document de travail GeoDep (DT_2026)

Lire d'abord `A_FAIRE.md` (état du travail, tâches, décisions) et `README.md` (qui écrit quoi).
Le mettre à jour quand une tâche est faite ou une décision prise.

## Organisation

- Ce dossier (git, github.com/pwibaux/GeoDep) : `trade_dependencies_geodep.qmd`, code R (`R/`),
  CSV (`data/`), tableaux `.md` (`tables/`), chiffres du texte (`_variables.yml`).
- La base GeoDep (Stata) est dans la Dropbox : `Dependence/01_GeoDep_database`
  (master `0_master.do`, chemins et paramètres dans `Program/00_config.do`, README.txt).
  Les do-files `Program/2_analysis/2*.do` écrivent `data/`, `tables/` et `_variables.yml` ici
  (global `$wp`, trouvé seul dans `Documents/GitHub/GeoDep/DT_2026`).
- Après chaque compilation, `R/copy_pdf.R` copie le PDF dans `Dependence/02_working_paper`
  (chemin selon le nom d'utilisateur).
- Co-autrice : Pauline Wibaux. À deux : `git pull` avant de lancer Stata, commit + push après.
  Demander avant de commiter ou de pousser.

## Règles de travail

- Jamais de chiffre tapé à la main dans le texte : `{{< var nom >}}` (`_variables.yml`) ou R en
  ligne (`` `r ...` ``). Ne jamais modifier à la main `data/`, `tables/`, `_variables.yml`,
  ni les données sources : on corrige dans le code Stata et on documente dans le README de la base.
- R : tidyverse uniquement, simple à lire, commentaires de 2 lignes au plus.
- Stata : code simple et aéré, sans commandes compliquées.
- Texte (anglais) : éviter les « ; » et les tirets « - » dans la prose. « 68% » sans décimale
  forcée convient. Proposer des améliorations, mais demander avant les changements de fond.
- Répondre en français.

## Outils (Windows)

- Stata en batch : `"<stata>" //e do fichier.do`
  (lancer depuis un dossier temporaire ; le log est écrit à côté). `<stata>` selon le poste :
  - `lefebvre` : `/c/Program Files/Stata18/StataSE-64.exe` (Stata 18) ;
  - `Kevin` : `/c/Users/Kevin/Desktop/Stata/Stata17/StataSE-64.exe` (Stata 17).
  Entre les deux versions, un arrondi peut changer d'un dixième dans un CSV
  (vu le 28/10/2026 dans `hhi_topshare.csv`) : ne pas commiter ce bruit.
- Quarto : `"/c/Program Files/Quarto/bin/quarto.exe" render trade_dependencies_geodep.qmd`.
  Erreur « os error 32 » : fichier verrouillé par Dropbox ou un autre programme, relancer.
- Dropbox : les fichiers « en ligne uniquement » ne se déplacent pas bien en ligne de commande
  (copie fichier par fichier) ; déplacer de gros dossiers sur dropbox.com.
