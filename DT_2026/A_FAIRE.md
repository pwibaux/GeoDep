# DT 2026 : prochaines étapes (état au mardi 27/10/2026)

Fichier dans le dépôt git GeoDep (`DT_2026/`), partagé avec la co-autrice.
Papier : `trade_dependencies_geodep.qmd`. Code R des figures : `R/`.
Chaîne Stata (CSV, tableaux, chiffres du texte), dans la Dropbox :
`Dependence/01_GeoDep_database/Program` (master : `01_GeoDep_database/0_master.do`).

## Où on en est

- Squelette complet : chaque section a ses tableaux et figures, avec titre
  au-dessus et source + note en dessous. Compile en HTML et PDF.
- Chiffres du texte : jamais à la main. Deux façons :
  - `{{< var nom >}}` : noms dans `_variables.yml` (écrit par `28_dt_numbers.do`) ;
  - R en ligne (`` `r n_m("EUN", "C1")` ``) pour les Venn : fonctions `venn_n`,
    `venn_pct`, `venn_median` de `R/venn_criteria.R`, raccourcis `n_m`, `pct_m`,
    `med_m` (importations) et `n_x`, `pct_x`, `med_x` (exportations).
- Rédigé : Literature (2), Venn import et export (3.1, 3.2), Strategic sectors
  (3.3), intro de la section 4 et Coverage (4.1).
- Liste Dual use corrigée (27/10) : les positions HS4 entières sont remplacées
  par leurs sous-positions HS6 (940 -> 965 produits). Documenté dans le README.
- Rédigé aussi : Sensitivity analysis (4.4).
- Scripts R déplacés de la chaîne Stata vers `R/` ;
  chaque CSV n'est écrit qu'une fois.

## Journée du 27/10/2026 (en bref)

- 4.4 Sensitivity rédigée : sens économique du seuil 0,4 (le premier partenaire
  fait la majorité du commerce : 55-56% pour C1, 60% pour C2), puis figure 9
  (courbes régulières, C2 le plus influent, classement des pays stable).
  Bornes théoriques (HHI, racine du HHI) retirées de la figure 8 et du texte.
- Question ouverte : figure 8 en un seul panneau (4 médianes) au lieu de 4 ?
- Migration Dropbox -> git faite et vérifiée (mêmes CSV, tableaux et chiffres
  qu'avant ; tri ajouté avant l'export de first_origin_M et first_destination_X).
- Dropbox rangée (Graph, Map, GeoDep_Clemence dans `99_old`) ;
  `17_compare_with_app.do` testé depuis le nouvel emplacement : sans erreur.
- Vérifier sur le poste de Pauline que son nom d'utilisateur est `wibaux` ou
  `paulinewibaux` (en-têtes des do-files, `R/copy_pdf.R`).

## À faire : rédaction

1. Sections encore vides (`<!-- Text -->`) : Introduction (texte autour de la
   figure), Methodology (intro), Variables (4.2, fin), Trade dependencies
   import et export (4.3), Concluding remarks.
2. Revue de littérature : cases "TBC" du tableau comparatif (OECD 2025,
   Méjean et Rousseaux, Arriola et al.). La ligne Bonnet et al. est remplie
   d'après la note DG Trade (Chief Economist Note n° 2, mars 2026).
   La ligne "OECD (2025)" n'a pas de référence dans `references.bib`.
3. Bibliographie : ajouter UNCTAD SDG Pulse 2025 (liste CRM, citée en texte
   simple dans 3.3). Décider si l'on garde la colonne VoxEU de Bonnet et al.
   (`bonnet2026openness`, citée seulement dans la liste d'ouverture de 2) ou
   si l'on ne cite que la note (`bonnet2026foreign`).
4. Annexe "Additional figures" : vide. La supprimer, ou y mettre des figures.

## À faire : coordination avec Pauline

- Cartes (juste avant les tableaux de 4.3) et décomposition sectorielle des
  importations : c'est elle ; la prévenir de la figure par secteur stratégique
  (`fig-sector-dep`, `R/sector_dep.R`) pour éviter les doublons.
- Application de cartes : mettre à jour les données et republier ; les textes
  de `geodep_shiny/app.R` (git) (l. 152-194) gardent les anciens libellés de
  `import_dpt`, `share_M_odpt`, `export_opt`, `share_X_odpt`. La correction
  Dual use change aussi `sect_dual_use` et deux chiffres d'interdépendances.
- Réorganisation du dossier `Dependence` : faite (voir plus bas).

## Organisation des dossiers (faite le 27/10/2026)

```
Dropbox : Dependence/
├ 01_GeoDep_database/      chaîne Stata (ex GeoDep_2024), BACI dans Data/raw/BACI_HS22
├ 02_working_paper/        PDF du document de travail (copiés par Quarto)
├ 03_other_output/         PolicyBrief, PB_tables, Lettre, Graphique_du_Cepii, Panorama…
├ JRC_BioTech/             projet à part (nom inchangé : chemins écrits dans ses do-files)
├ 05_literature/           Biblio, Press
├ 06_admin/                Demandes, Discussion, Stage, Pgm de travail 2024-2025
└ 99_old/                  Data (ancien), Program, Table, Graph, Map, GeoDep_Clemence,
                           Writing_old, DT_GeoDep_2026, Writing_DT_2026…

Git : Documents/GitHub/GeoDep/
├ geodep_shiny/            application de Pauline (plus vraiment valide)
└ DT_2026/                 ce document (voir README.md)
```

Comment les deux se parlent (un seul sens) :
- Stata écrit dans le clone git : `data/*.csv`, `tables/*.md`, `_variables.yml`
  (chemin `$wp`, trouvé seul dans `Documents/GitHub/GeoDep/DT_2026`) ;
- les autres sorties Stata (.tex, graphiques) restent dans `01_GeoDep_database/Output` ;
- Quarto copie le PDF dans `Dependence/02_working_paper` (`R/copy_pdf.R`, chemin selon l'utilisateur).
- À deux : `git pull` avant de lancer Stata, commit + push après.

Reste à faire :
- `geodep_shiny` lit encore `GeoDep_2024/...` (Dropbox) : à corriger si l'appli
  repart, sinon l'archiver (tag git puis suppression).
- Si le dépôt devient public : vérifier la licence BACI pour les données dérivées ;
  `renv` pour figer les packages R.

## Avant de finaliser

- Relancer le master une fois, puis recompiler le document.

## Décisions déjà prises

- Pas d'interdépendances dans le papier.
- Secteurs stratégiques : importations seulement (logique d'approvisionnement ;
  côté export, le risque est la perte de marché, indépendant du caractère
  stratégique).
- Figure par secteur : nombre de produits dépendants + part du secteur
  dépendante (Dual use n'est pas le plus dépendant en part ; Health et
  Agrifood le sont).
- Venn : grille 2 × 2 (pas d'onglets), échelle compressée, survol en HTML.
- Robustesse aux seuils : traitée dans Sensitivity, pas une contribution.
- Liste Dual use : positions HS4 entières -> toutes leurs sous-positions HS6.
- Code R : tidyverse, simple à lire, commentaires de 2 lignes au plus.
- Un seul document : le document "base de données" (DT_GeoDep_2026) est archivé.
- Document de travail dans git (hors Dropbox) ; la base reste dans la Dropbox.
