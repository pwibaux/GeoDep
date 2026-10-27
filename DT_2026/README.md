# DT_2026

GeoDep working paper (2026): `trade_dependencies_geodep.qmd`.

## Où sont les choses

| Dossier / fichier | Contenu | Écrit par |
|---|---|---|
| `trade_dependencies_geodep.qmd` | texte du document | nous |
| `R/` | code des graphiques et des chiffres calculés en R | nous |
| `data/` | CSV lus par le code R | **Stata uniquement** |
| `tables/` | tableaux `.md` inclus tels quels | **Stata uniquement** |
| `_variables.yml` | chiffres du texte (`{{< var nom >}}`) | **Stata uniquement** |
| `references.bib`, `cmidrule.lua`, `_quarto.yml` | bibliographie, mise en forme | nous |
| `A_FAIRE.md` | liste des tâches | nous |

Ne jamais modifier à la main `data/`, `tables/` ni `_variables.yml` : ils sont réécrits par
`01_GeoDep_database/Program/2_analysis/2*.do` (Dropbox), qui retrouve ce dossier seul s'il est cloné
dans `Documents/GitHub/GeoDep` (sinon, définir `global wp "..."` en tête du do-file).

## Relancer Stata à deux

1. `git pull`
2. lancer `0_master.do` (ou les do-files `2_analysis`)
3. vérifier les changements (`git diff data tables _variables.yml`), puis commit et push

## Compiler

```
quarto render trade_dependencies_geodep.qmd
```

Packages R : tidyverse, sf, ggiraph, eulerr. Les sorties (HTML, PDF, `*_files/`) ne sont pas
versionnées. Après chaque compilation, `R/copy_pdf.R` copie le PDF dans
`Dependence/02_working_paper` (Dropbox). Il faut pour cela un fichier `.Renviron` (non versionné)
dans ce dossier, avec le chemin du dossier Dependence :

```
DEPENDENCE_DIR="C:/Users/.../Dropbox/.../Dependence"
```
