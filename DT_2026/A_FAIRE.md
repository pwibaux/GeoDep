# DT 2026 : prochaines étapes (état au mardi 03/11/2026)

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

## Journée du 29/10/2026 (en bref)

- Propositions de réécriture (Tables 2, 5, 6, Figures 4 à 7, introduction) dans
  `propositions_2026-10-29.md` (non commité). Reportés dans le qmd le 29/10 :
  Figures 4 à 7, Table 6, paragraphe des résultats de l'introduction, S19
  (Coverage), égalités du premier fournisseur (Variables). Restent : Tables 2
  et 5 (Stata), sources des listes en `.bib`, table d'annexe.
- Chiffres de ces paragraphes ajoutés dans 22 (recoupements des listes), 23
  (sections 5b et 6d : sur-représentation de la Chine, C1/C2 par secteur) et
  24 (Table 6). Lancés, vérifiés contre R, rien ne change dans `Data/final`.
- Décisions : pas de parts sur les couples pays x produit mis en commun dans le
  texte (médianes ou % d'importateurs) ; pas de phrase sur la « gravité » ;
  tableaux par fournisseur hors du WP (graphique ou carte plus tard).

## Journées du 30/10 au 03/11/2026 (en bref)

- Literature (fait, dans le qmd, à commiter) :
  - paragraphe en deux : production domestique (Commission, Baur-Flach, Vicard,
    nous 2024 = commerce ; Méjean-Rousseaux et Bonnet 2026 = Prodcom), puis
    arbitrage **profondeur de la mesure vs nombre de pays** (HS6 = niveau le plus
    fin harmonisé pour tous les pays) ; concentration de l'offre mondiale
    (Bonneau-Nakaa, Commission, Arriola, nous 2024, Jean et al., Méjean-Rousseaux) ;
  - phrase de positionnement : on prend l'autre côté de l'arbitrage (commerce seul, HS6) ;
  - `tbl-comparison` : cases TBC remplies (Méjean-Rousseaux, Arriola) ; ligne
    "OECD (2025)" supprimée (= Arriola et al. 2024) ; on cite la note Bonnet
    (`bonnet2026foreign`), plus la colonne VoxEU.
- Exercice de réplication : critères des autres études appliqués à BACI 2024
  pour l'UE. Tout est dans `replication_methodes_2026-10-30.md` (scripts dans
  `replication_R/`). Messages principaux :
  - la part de la Chine dépend surtout de la **mesure** : en nombre de produits
    ~45-50 %, en valeur importée de Chine ~55-58 %, en valeur des produits dont
    la Chine est 1er fournisseur ~70 % (c'est la mesure du MERICS) ;
  - GeoDep donne 10-15 points de plus à la Chine pour deux raisons : C2
    (concentration mondiale) et l'UE traitée comme un seul pays (la Commission
    compte les exportations intra-UE) ;
  - seulement 289 produits en commun avec la Commission ; GeoDep est inclus
    dans Baur-Flach et Arriola.
- Décisions sur la réplication :
  - section courte dans le WP : tableau GeoDep / Commission / MERICS seulement
    (études sur l'UE entière, critères calculables avec BACI) ; les autres
    études expliquées dans le texte ; Bonnet en note (exposition calculée avec le
    commerce, seuil 0,325) ;
  - MERICS : UE comme un seul exportateur, sans lissage ; Chine seule (sans HKG/MAC) ;
  - la discussion production vs commerce reste HORS du WP (trop de paramètres
    changent à la fois) ;
  - emplacement (03/11) : dans `## Sensitivity analysis`, en deux sous-parties :
    `### Thresholds` (texte et figures 8-9 actuels) puis
    `### Comparison with existing methods {#sec-comparison}`, avec une phrase
    de transition (« Beyond the thresholds, the results also depend on the
    criteria themselves… »).
  - Brouillon en anglais de la section : §5bis du document de réplication
    (chiffres tapés à la main : à NE PAS recopier tels quels).

## À faire : rédaction

0. **Prochaine étape : section de comparaison.**
   1. Do-file Stata (par ex. `29_dt_replication.do`) qui reproduit le tableau et
      les chiffres du §5bis vers `_variables.yml` / `tables/`. Demander avant
      d'écrire et vérifier que Pauline ne travaille pas sur les do-files.
   2. Insérer la section dans Sensitivity avec `{{< var >}}`, recompiler.
   3. Ajouter le label `{#sec-literature}` à `# Literature`.
   4. Encore ouvert : ligne ou note « GeoDep sans persistance (C4) » (561
      produits) ; où mettre « GeoDep inclus dans Baur-Flach et Arriola ».
1. Sections encore vides (`<!-- Text -->`) : Introduction (texte autour de la
   figure), Methodology (intro), Variables (4.2, fin), Trade dependencies
   import et export (4.3), Concluding remarks.
2. Discussion des choix de méthode (dans le texte) : C3 utilise les
   exportations comme proxy de la production (réexportations) ; l'UE comme un
   seul exportateur change C2 ; nombre de produits vs valeur ; C2 et C3 côté
   export plus fragiles ; Hong Kong et Singapour comme « 1er fournisseur ».
3. Bibliographie : ajouter UNCTAD SDG Pulse 2025 (liste CRM), règlement
   double usage 2021/821 et sa table de correspondance du 1/1/2026, FAOSTAT
   engrais, Traité sur la Charte de l'énergie. Vérifier la phrase Agrifood
   « synthesises almost all existing national definitions » (semble décrire la
   liste CRM de la CNUCED).
4. Annexe "Additional figures" : vide. La supprimer, ou y mettre des figures.
5. Prévenir Pauline : do-files 22, 23 et 24 modifiés le 29/10 (nouveaux
   chiffres, table d'annexe, format de la Table 2) ; un `git pull` suffit.

## À faire : coordination avec Pauline

- Cartes (juste avant les tableaux de 4.3) et décomposition sectorielle des
  importations : c'est elle ; la prévenir de la figure par secteur stratégique
  (`fig-sector-dep`, `R/sector_dep.R`) pour éviter les doublons.
- Application de cartes (`geodep_shiny/`) : c'est Pauline qui s'en occupe.
  Vérifié le 28/10 :
  - noms des variables : déjà à jour dans `preparation.R` et `app.R`
    (`first_o`, `share_M_odpt`, `first_d`, `share_X_odpt`, `dependent_M/X`) ;
  - chemins : `update_data.R` (l. 12) et `export_maps.R` (l. 10) pointent
    encore vers `GeoDep_2024` ; proposé : `DROPBOX_DIR` = dossier `Dependence`
    de chacun, et le code n'ajoute que `01_GeoDep_database/...` ;
  - descriptions des variables du README téléchargeable (`app.R`, l. 387-429) :
    différentes des labels de `15_geodep_final.do` (à aligner) ;
  - relancer `update_data.R` + `preparation.R` (correction Dual use :
    `sect_dual_use`), puis republier.
- Mon `geodep_shiny/.Renviron` (local, hors git) suit déjà la convention
  proposée : il ne marchera qu'après la correction des chemins par Pauline.
  Le `.Renviron` ne passe pas par git : le créer aussi sur mon autre poste
  (`lefebvre`), dans `geodep_shiny/.Renviron` :
  `DROPBOX_DIR=C:/Users/lefebvre.LENOVOM90117/CIREM Dropbox/kévin LEFEBVRE/Dependence`
  (et y cloner le dépôt dans `Documents/GitHub/GeoDep`, pour que Stata trouve `$wp`).
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
- `geodep_shiny` lit encore `GeoDep_2024/...` (Dropbox) : corrections confiées
  à Pauline (voir "Coordination avec Pauline").
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
