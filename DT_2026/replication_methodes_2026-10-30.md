# Appliquer les méthodes de la littérature aux données GeoDep (UE, 2024)

*Document de travail, 30/10/2026. Calculs exploratoires en R (scripts dans `replication_R/` :
`replicate_methods.R`, `overlap.R`, `variants.R`, `merics_smooth.R`, `bonnet2023.R`,
`bonnet2024.R`, `bn_central.R`, `bn_fr.R`, `merics_detail.R` ; les lancer depuis ce dossier),
hors du pipeline Stata : aucun de ces chiffres ne doit être recopié à la main dans le qmd.*

**Résumé des décisions du 30/10** : on ne compare GeoDep qu'aux études qui portent sur l'UE dans
son ensemble et dont les critères se calculent avec BACI, soit la Commission (2021) et MERICS
(2024) dans le tableau, Bonnet et al. (2026) en note (exposition par le commerce, seuil converti
de 0,325). Arriola, Méjean-Rousseaux, Bonneau-Nakaa, Jaravel-Méjean et Baur-Flach sont expliqués
dans le texte. MERICS : UE comme un seul exportateur, pas de lissage. Chine seule, sans Hong Kong
ni Macao. La question production / commerce n'entre pas dans le WP (§5.5). Projet de section en
anglais au §5bis.

## 1. Pourquoi cet exercice

Les études sur les dépendances de l'UE donnent des chiffres très différents, en particulier sur
le poids de la Chine : 52 % (Commission, 2021), 47 % (Bonnet et al., 2026), ~70 % (MERICS,
Chimits 2024), 63 % (GeoDep). Ces écarts peuvent venir de quatre sources à la fois :

1. **les critères** (quels filtres, quels seuils) ;
2. **les données** (BACI ou Comext, HS6 ou CN8, révision HS) ;
3. **l'année** (2018, 2019, 2022, 2023, 2024) ;
4. **la mesure** de la part de la Chine (en nombre de produits ou en valeur, et quelle valeur).

En comparant les études publiées, on ne peut pas séparer ces quatre sources. L'exercice consiste
à **neutraliser les sources 2 et 3** : on applique les critères de chaque étude aux mêmes données
(BACI HS 2022, l'UE27 comme un seul importateur, l'année 2024), puis on calcule la part de la
Chine avec **les trois mesures**. Ce qui reste comme écart vient des critères (source 1) ou de la
mesure (source 4).

## 2. Cadre commun à toutes les réplications

- **Données** : BACI HS 2022, version 202601, année 2024 (2022 et 2023 pour les variantes avec
  persistance). C'est le fichier `imports_hs22.dta` du pipeline, donc avec les mêmes exclusions
  que GeoDep (chapitre 93 et les trois codes de gaz).
- **Importateur** : l'UE27 consolidée (`EUN`). Les importations de l'UE sont les flux
  extra-UE. Le flux `EUN → EUN` est le commerce intra-UE ; il sert pour les critères qui en ont
  besoin (Commission, Bonnet).
- **Univers** : les 5 575 produits HS6 que l'UE importe de l'extérieur en 2024 (le même univers
  que la base GeoDep pour `EUN`).
- **Premier fournisseur** : le pays d'origine qui a la plus grande valeur d'importations
  extra-UE du produit (Chine = `CHN` seule, sans Hong Kong ni Macao).
- **Concentration mondiale**, quand une méthode l'utilise : HHI des exportations mondiales, avec
  l'UE comme un seul exportateur et sans les flux intra-UE, comme notre C2.

### Les trois mesures de la part de la Chine

Pour un ensemble de produits dépendants :

| Mesure | Définition | Qui l'utilise |
|:--|:--|:--|
| (a) Nombre | % des produits dépendants dont la Chine est le premier fournisseur | GeoDep, Bonnet (« 47 % des produits ») |
| (b) Valeur chinoise | importations depuis la Chine / importations totales, sur ces produits | Commission (« 52 % de la valeur »), Bonnet (« environ la moitié de la valeur ») |
| (c) Valeur des produits « chinois » | valeur totale des produits dont la Chine est le premier fournisseur / valeur de tous les produits dépendants | MERICS (« ~70 % des dépendances en valeur ») |

La mesure (c) est toujours la plus haute : elle compte toute la valeur d'un produit dès que la
Chine en est le premier fournisseur, y compris la part fournie par les autres pays.

## 3. Méthode par méthode : l'étude d'origine et ce qu'on reproduit

### 3.1 GeoDep (référence)

- **Critères** : C1 HHI des importations > 0,4 ; C2 HHI des exportations mondiales > 0,4 ;
  C3 importations / exportations > 1 ; C4 C1-C3 vrais au moins 2 années sur 3.
- Pour l'UE, C3 compare les importations **extra-UE** aux exportations **extra-UE** : l'UE est
  traitée comme un pays.
- Deux lignes dans le tableau : la base publique (avec C4) et 2024 seule (sans C4), pour
  comparer à des méthodes qui n'ont pas de persistance.

### 3.2 Commission européenne (2021)

- **Étude d'origine** : BACI, HS6, environ 5 000 produits, année 2019. Trois critères :
  1. HHI des importations extra-UE > 0,4 ;
  2. importations extra-UE / importations totales de l'UE (extra + intra) > 0,5 ;
  3. importations extra-UE / exportations totales de l'UE (extra + intra) > 1. Les exportations
     totales servent de proxy de la capacité de production de l'UE.
  Résultat : environ 390 produits dépendants. Ensuite, filtre sur les « écosystèmes sensibles »
  (137 produits, la Chine pèse 52 % de leur valeur). La concentration mondiale (HHI > 0,4, hors
  pays de l'UE) sert seulement à qualifier ces 137 produits, pas à les identifier.
- **Ce qu'on reproduit** : les trois critères à l'identique, sur 2024.
- **Ce qui diffère** : on n'applique pas le filtre des écosystèmes (non transparent, et différent
  de nos listes stratégiques). Les chiffres sont donc à comparer aux ~390, pas aux 137.
- **Variante** : on ajoute la persistance de GeoDep (2 années sur 3), proche de la mise à jour
  d'Arjona et al. (2023), qui exige que le produit soit dépendant plusieurs années de suite. Leur
  règle exacte (fenêtre de 4 ans et classement des 10 % les plus dépendants) n'est pas reproduite.

### 3.3 Bonnet et al. (2026), DG Trade

- **Étude d'origine** : Comext et Prodcom, HS6 2017, 5 361 produits, 2017-2023. Deux critères :
  HHI des importations extra-UE > 0,4 et « exposition » > 0,4, où exposition = importations
  extra-UE / (importations extra-UE + production de l'UE). Seuils contrôlés par une analyse en
  clusters. La Chine regroupe Chine, Hong Kong et Macao. Résultat : 835 produits en 2023, la Chine
  fournit 47 % des produits et environ la moitié de la valeur.
- **Ce qu'on reproduit** : la version « commerce » de l'exposition, que les auteurs utilisent
  eux-mêmes pour les 11 % de produits sans données Prodcom (leur équation 3) : importations
  extra-UE / (importations extra-UE + exportations extra-UE + exportations intra-UE) > 0,4.
- **Ce qui diffère** : pas de données de production ; Chine sans Hong Kong ni Macao ; pas
  d'exclusion des produits à très faible valeur (< 10 000 €, 1 produit en 2023) ; BACI au lieu de
  Comext ; HS 2022 au lieu de HS 2017.
- **Production ou commerce, sur leur année (2023)** :

  | Version | Produits dépendants | Chine 1er fournisseur |
  |:--|--:|--:|
  | Publiée (Prodcom, plus commerce pour 11 % des produits) | 835 | 394 (47 %) |
  | Exposition par le commerce > 0,4 | 689 | 324 (47,0 %) |
  | Exposition « imputée » > 0,4, soit exposition par le commerce > 0,325 | 874 | 416 (47,6 %) |

  La dernière ligne applique à tous les produits leur propre formule d'imputation
  (exposition = 1,287 × exposition par le commerce − 0,0186) : un seuil de 0,4 sur l'exposition
  correspond à 0,325 sur l'exposition par le commerce. Avec cette conversion, le commerce seul donne
  presque le même nombre de produits (874 contre 835). Avec le seuil de 0,4 appliqué directement,
  on en trouve 17 % de moins. La part de la Chine ne bouge pas (47 %).
  Limites : on ne peut pas isoler l'effet de la production, car d'autres différences se cumulent
  (BACI ou Comext, révision HS) et la liste de produits de Bonnet n'est pas publiée.
- **Même question pour les critères de la Commission (2023)** : Bonnet et al. (note 33) remplacent
  les exportations par la production dans le 3e critère et trouvent 339 produits. Avec les
  exportations (méthode d'origine), on en trouve 468 sur nos données. Ici, la production réduit la
  liste, alors qu'avec l'exposition de Bonnet elle l'augmente : l'effet de la production dépend de
  la façon dont on l'introduit.
- **Choix pour le tableau (décidé le 30/10)** : seuil de 0,325 (critère converti avec leur
  formule), le seuil de 0,4 en note (2024 : 720 produits, Chine 47,9 % / 56,8 % / 69,1 %).

### 3.4 MERICS (Chimits, 2024)

- **Étude d'origine** : HS6 révision 1996 (5 113 produits), 2000-2022, l'UE consolidée sans le
  commerce intra-UE. Trois critères :
  1. importations ≥ 2 × exportations ;
  2. un fournisseur pèse au moins 30 % des importations ;
  3. HHI des exportations mondiales ≥ 0,25.
  Les dépendances sont lissées sur deux ans. Résultat : 421 produits dépendants de la Chine pour
  l'UE en 2022 ; ~70 % des dépendances en valeur viennent de produits fournis surtout par la Chine.
- **Ce qu'on reproduit** : les trois critères, sur 2024.
- **Ce qui diffère** : pas de lissage sur deux ans (la règle exacte n'est pas claire) ; HS 2022
  au lieu de HS 1996 ; on ne sait pas si leur HHI mondial traite l'UE comme un seul exportateur.
- **Choix retenu (30/10)** : l'UE comme un seul exportateur, pour rester dans l'optique GeoDep.
  Test : avec les pays de l'UE comptés séparément (sans flux intra-UE), on obtient 956 produits au
  lieu de 970, tous inclus dans les 970, et la Chine reste à 50 % / 53 % / 71 %. Le choix ne
  change rien.
- **Lissage (30/10)** : pas de lissage, 2024 seule. Test : avec les valeurs de 2023 et 2024
  cumulées, 926 produits (864 en commun avec 2024 seule) et la Chine à 51 % / 53 % / 74 % ; en
  exigeant les deux années, 784 produits et la Chine à 54 % / 54 % / 74 %. Le lissage réduit un peu
  la liste et augmente un peu le poids de la Chine, sans changer les conclusions.
- **Vérifié dans le PDF** (Dropbox `05_literature/Biblio/MERICS_2024.pdf`) : UE consolidée, sans
  commerce intra-UE ; HS 1996, 5 113 catégories ; lissage = agrégat de deux années (« the 2021
  dependencies are determined based on an aggregate of 2020 and 2022 data »). MERICS ne donne pas
  le nombre total de dépendances de l'UE, seulement des parts (voir §4, Validation).

### 3.5 Arriola et al. (2024), OCDE

- **Étude d'origine** : BACI HS6, 1997-2021 en moyennes sur trois ans. Une dépendance est un
  **lien bilatéral** (pays × partenaire × produit) : HHI des importations du pays ≥ 0,2 et part du
  partenaire ≥ 10 %. Pas de critère de production ni de concentration mondiale.
- **Ce qu'on peut reprendre** :
  - les deux critères (HHI des importations ≥ 0,2, part du partenaire ≥ 10 %) ;
  - les moyennes sur trois ans (2022-2024 sont dans le pipeline) : 9 636 liens et 4 278 produits,
    contre 9 873 liens et 4 413 produits sur 2024 seule, la Chine à 25 % des liens ;
  - leur indicateur descriptif de concentration « excessive » : HHI des importations > 2 × HHI
    mondial, avec HHI mondial ≥ 0,2 et HHI des importations ≥ 0,4.
- **Ce qu'on ne peut pas reprendre** : les parties input-output (TiVA, ICIO) et le modèle d'équilibre
  général ; leur liste de secteurs « stratégiques » (IMF / Atlantic Council, en ISIC à 3 chiffres),
  différente de nos listes ; et, dans leur présentation, les pays de l'UE pris un par un avec les
  liens intra-UE (on garde l'UE comme un bloc).
- **Problème de fond** : au niveau du produit, le critère de part bilatérale ne filtre rien. Si le
  HHI est ≥ 0,2, le premier fournisseur a forcément au moins 20 % des importations, donc au moins
  un lien passe le seuil de 10 %. Une liste de *produits* « à la Arriola » se réduit donc à
  « HHI des importations ≥ 0,2 », soit 79 % des produits importés par l'UE. Le critère bilatéral
  sert seulement à désigner les partenaires dont on dépend. La méthode est conçue pour compter des
  liens, pas pour dresser une liste de produits : elle donne une borne haute, pas une liste
  comparable aux autres.
- **Concentration « excessive »** : 399 produits (7 % des produits, mais 2 % de la valeur), dont
  10 seulement en commun avec GeoDep ; la Chine est premier fournisseur de 22 % d'entre eux. C'est
  logique : l'indicateur repère les produits où l'offre mondiale est diversifiée mais où l'UE
  s'approvisionne quand même auprès de peu de pays, c'est-à-dire l'inverse de notre C2. On mesure
  une concentration *évitable*, pas une dépendance.

### 3.6 Méthodes retirées du tableau (décision du 30/10)

**Règle de sélection.** L'exercice porte sur l'UE ; on compare donc GeoDep aux études qui
identifient elles aussi les dépendances **de l'UE dans son ensemble**, et dont on peut reproduire
les critères avec BACI (pas de tableaux input-output ni de données d'entreprises).

| Étude | Porte sur l'UE ? | Reproductible avec BACI ? | Dans le tableau |
|:--|:--|:--|:--|
| Commission (2021), Vicard et Wibaux (2023) | Oui | Oui | Oui |
| Bonnet et al. (2026) | Oui | Oui, avec l'exposition calculée sur le commerce | Oui |
| MERICS (Chimits, 2024) | Oui | Oui | Oui |
| Méjean et Rousseaux (2024) | Oui | Non (Prodcom, stickiness) ; la partie reproductible est la Commission | Non |
| Arriola et al. (2024) | En partie (UE27 parmi d'autres) | Oui, mais au niveau produit, les critères se réduisent à un HHI ≥ 0,2 | Non |
| Bonneau et Nakaa (2020) | Non (France et 5 autres pays) | Oui, mais chiffres non retrouvés | Non |
| Jaravel et Méjean (2021) | Non (France) | Non (données d'entreprises) | Non |
| Baur et Flach (2022) | Non (Allemagne) | Non (tableaux input-output) | Non |

Détail pour les études hors UE :

- **Jaravel et Méjean (2021)** : France, données douanières par entreprise. Majorité
  d'importations extra-UE, HHI > 0,5, et une entreprise française fait au moins 90 % des
  importations. Le critère qui fait l'originalité de la méthode (la granularité des entreprises)
  demande des données d'entreprises. **Retirée.**
- **Baur et Flach (2022)** : Allemagne, importations depuis la Chine. Biens parmi les 3 premiers
  intrants des 5 premiers secteurs allemands (tableaux input-output), HHI > 0,33, importations /
  exportations > 1. Sans le filtre input-output, il reste « GeoDep sans C2 avec un seuil plus bas »
  (1 457 produits pour l'UE, qui contiennent tous les produits GeoDep). **Retirée.**
- **Bonneau et Nakaa (2020)** : France, BACI 2018. Importations françaises extra-UE > 50 %, HHI
  des importations extra-UE > 0,5, et risque de « centralité » > 2,5. 121 produits concentrés
  (1 % de la valeur importée), dont 12 vulnérables (lampes LED, simulateurs de vol, plantes
  artificielles, meubles de jardin, horlogerie, gravures, antiquités). **Retirée**, car on
  n'arrive pas à retrouver leurs chiffres, alors que tous les critères se calculent avec BACI :
  - *Centralité* : la formule de Korniyenko et al. (2017, annexe I.A) se calcule. Pour chaque
    produit, la centralité de chaque pays exportateur est la somme, sur ses clients, de ses
    exportations divisées par l'importation moyenne par fournisseur du client ; la « centralité »
    du produit est l'écart-type de ces scores entre pays, centré-réduit sur l'ensemble des produits
    (z-score, p. 31). Le seuil de 2,5 ne peut être qu'un z-score : en valeur brute, 98,6 % des
    produits le dépassent. En z-score, 3,1 % des produits le dépassent. Mais Bonneau et Nakaa
    disent qu'à ce seuil « le premier pays exportateur assure environ les deux tiers des
    exportations mondiales », alors que sur nos données, autour de z = 2,5, le premier exportateur
    n'a que 39 % (médiane).
  - *Produits concentrés pour la France* : avec leurs deux premiers critères, on trouve 803
    produits en 2024 (10,6 % de la valeur importée), contre 121 en 2018 (1 %). L'écart est trop
    grand pour venir seulement de l'année ; il doit y avoir un filtre ou une définition qui n'est
    pas décrite dans la note. Les variantes testées (HHI sur tous les partenaires, part extra-UE
    de l'UE, seuil de valeur) ne donnent pas 121 non plus.
  - Leur méthode est par ailleurs le point de départ de la Commission (2021), qui reprend leurs
    deux premiers critères en remplaçant la centralité par le ratio importations / exportations.

**Possible plus tard** : refaire la Commission, Bonnet et MERICS pour les États-Unis (MERICS
publie aussi un chiffre pour eux).

### 3.7 Non reproductible

- **Méjean et Rousseaux (2024)** : critères de la Commission + absorption (Prodcom) +
  « stickiness » des relations entre entreprises (données d'entreprises). La partie reproductible
  est la Commission avec données poolées sur 5 ans, déjà couverte par 3.2.
- **Lefebvre et Wibaux (2024)** : c'est la méthode GeoDep appliquée aux produits stratégiques.

## 4. Résultats (UE, 2024)

Rappel : la Chine est le premier fournisseur de 35,4 % des 5 575 produits importés et pèse
23,7 % de la valeur des importations extra-UE.

| Méthode | Produits dépendants | % des produits | % de la valeur importée | Chine (a) nombre | Chine (b) valeur chinoise | Chine (c) valeur des produits « chinois » | En commun avec GeoDep |
|:--|--:|--:|--:|--:|--:|--:|--:|
| GeoDep (base publique, avec C4) | 468 | 8,4 | 8,8 | 62,6 | 67,1 | 80,3 | 468 |
| GeoDep, 2024 seule | 561 | 10,1 | 9,5 | 59,2 | 65,3 | 78,9 | 468 |
| Commission 2021 | 497 | 8,9 | 8,9 | 44,3 | 57,6 | 68,4 | 289 |
| Commission 2021 + persistance | 409 | 7,3 | 8,2 | 46,7 | 59,3 | 70,2 | 264 |
| Bonnet 2026 (exposition par le commerce > 0,325) | 885 | 15,9 | 14,2 | 49,3 | 55,4 | 68,2 | 418 |
| MERICS | 970 | 17,4 | 19,2 | 50,0 | 52,7 | 70,9 | 415 |

Bonneau-Nakaa, Jaravel-Méjean et Baur-Flach sont retirés du tableau (§3.6).

**Arriola et al. (2024) est retiré du tableau (décision du 30/10)** : au niveau du produit, leurs
critères se réduisent à « HHI des importations ≥ 0,2 » (4 413 produits, 79 % des produits), et ce
n'est pas ce qui fait l'intérêt de leur méthode (les liens bilatéraux). On l'explique dans le
texte, avec leur concentration « excessive », qui est l'inverse de notre C2 (§3.5).

### Validation

Sur nos données 2024, les réplications retrouvent les ordres de grandeur publiés :

- **MERICS** (chiffres vérifiés dans le PDF, Dropbox `MERICS_2024.pdf`, UE 2022) : mesure (c) =
  70,9 %, contre « roughly 70 percent » ; produits dont la Chine est le premier fournisseur = 485,
  soit 8,7 % des produits, contre 421 (8 % des catégories). En revanche, toutes origines
  confondues, on trouve plus de dépendances : 17,4 % des produits et 19,2 % de la valeur importée,
  contre 14 % des catégories et 12 % des importations en 2022 (MERICS ne donne pas le nombre). Pistes :
  l'année, HS 1996 (5 113 catégories) contre HS 2022 (5 575), et le lissage sur deux ans (leurs
  dépendances de 2021 sont calculées sur 2020 et 2022 cumulés), qui réduit la liste.
- **Bonnet** : la Chine fournit 47,9 % des produits, contre 47 % publié.
- **Commission** : 497 produits, contre ~390 en 2019 (autre année et autre révision du HS).

## 5. Ce qu'on apprend

### 5.1 Le poids de la Chine dépend d'abord de la mesure

Avec n'importe quelle méthode de la littérature, la part de la Chine est d'environ 45-50 % en
nombre (a), 55-58 % en valeur chinoise (b) et 68-71 % en valeur des produits « chinois » (c).
Le « 70 % » de MERICS et le « 47 % » de Bonnet ne se contredisent pas : ils décrivent presque les
mêmes produits avec deux mesures différentes.

### 5.2 GeoDep donne un poids plus élevé à la Chine : C2 et l'UE traitée comme un pays

À mesure égale, GeoDep donne 10 à 15 points de plus à la Chine (63 % / 67 % / 80 %). Deux choix
l'expliquent (version corrigée le 30/10 ; la première version n'invoquait que C2) :
1. **Le critère de concentration mondiale à 0,4 (C2)** : il sélectionne les produits où l'offre
   mondiale est concentrée, et la Chine domine précisément ces marchés (Jean et al., 2023). C'est
   cohérent avec la décomposition par C2 déjà dans le WP (35 % → 49 % → 63 % pour l'UE). C'est le
   seul écart avec MERICS, qui n'a pas de question intra-UE.
2. **L'UE traitée comme un pays dans C3** : la Commission compte les exportations intra-UE comme
   une substitution possible et exclut donc des produits très échangés dans le marché unique,
   dont la Chine est souvent le premier fournisseur (voir §5.3).

### 5.3 Inclusions et recoupements avec GeoDep

- **GeoDep est entièrement inclus dans Baur-Flach et dans Arriola.** C'est mécanique : leurs seuils
  de concentration des importations sont plus bas (0,33 et 0,2), et Baur-Flach utilise le même
  ratio importations / exportations que nous. Nos dépendances sont donc un sous-ensemble strict
  de ces listes plus larges.
- **GeoDep est presque inclus dans MERICS** (415 des 468 produits). Les 53 produits GeoDep absents
  de MERICS échouent tous sur le critère « importations ≥ 2 × exportations », plus exigeant que
  notre ratio > 1. Dans l'autre sens, 555 produits MERICS ne sont pas dans GeoDep, dont 416 parce
  que leur concentration mondiale est entre 0,25 et 0,4 : la Chine n'est premier fournisseur que
  de 42 % d'entre eux.
- **La Commission ne partage que 289 produits avec GeoDep**, et l'écart vient des deux côtés :
  - *208 produits de la Commission ne sont pas dans GeoDep* : 164 parce que l'offre mondiale n'est
    pas concentrée (C2 ≤ 0,4), 44 parce qu'ils ne sont pas persistants. La Chine n'est premier
    fournisseur que de 24 % de ces produits.
  - *179 produits GeoDep ne sont pas dans la Commission* : tous échouent sur le critère
    « importations extra-UE > exportations **totales** (extra + intra) », et 107 aussi sur
    « importations extra-UE > 50 % des importations totales ». Ce sont des produits que l'UE
    importe de l'extérieur plus qu'elle n'en exporte, mais qu'elle échange beaucoup en interne
    (médiane de 53 % d'intra-UE dans les importations totales). La Chine est premier fournisseur
    de 69 % d'entre eux.
  - Les deux méthodes font donc des choix opposés : la Commission considère le commerce intra-UE
    comme une capacité de substitution et ignore la concentration mondiale ; GeoDep traite l'UE
    comme un seul pays (le commerce intra-UE ne compte pas) et exige une offre mondiale
    concentrée. Les deux choix vont dans le même sens pour la Chine : ils augmentent son poids
    dans GeoDep.

### 5.4 Le nombre de dépendances dépend surtout des seuils

De 409 (Commission avec persistance) à 1 457 (Baur-Flach), et 4 413 avec les critères de
l'OCDE, trop larges pour l'UE (79 % des produits). Le niveau de GeoDep (468, 8,4 % des
produits) est proche de celui de la Commission.

### 5.5 Production ou commerce : pourquoi l'effet n'est pas net (non retenu pour le WP)

Discussion du 30/10. Conclusion : trop de paramètres changent en même temps, le message n'est pas
assez clair pour le WP. Au mieux, une phrase de prudence dans la discussion de C3.

- **Les comparaisons disponibles** (les chiffres « production » viennent des données de Bonnet
  et al., Comext HS 2017 ; les chiffres « commerce » des nôtres, BACI HS 2022 ; une partie de
  l'écart vient donc des données) :

  | Critère (2023) | Commerce | Production | Écart |
  |:--|--:|--:|--:|
  | Exposition de Bonnet (0,4 ; converti en 0,325 pour le commerce) | 874 | 835 | ~ +5 % |
  | 3e critère de la Commission (ratio > 1) | 468 | 339 (Bonnet, note 33) | ~ −28 % |

- **Pourquoi la production peut augmenter ou réduire les dépendances** : deux effets contraires.
  1. *Les exportations peuvent dépasser la production* (réexportations, transit intra-UE par
     Rotterdam, allers-retours dans les chaînes de valeur, réexportations hors UE). Le commerce
     surestime alors la production et sous-estime la dépendance : passer à la production
     *augmente* les dépendances. La formule d'imputation de Bonnet (pente de 1,287 > 1) le
     confirme en moyenne. Prodcom ne couvre en plus que l'industrie, avec des valeurs manquantes.
  2. *La production peut dépasser les exportations* (une grande partie de la production est
     consommée sur place). Le commerce sous-estime alors la production et surestime la
     dépendance : passer à la production *réduit* les dépendances (Méjean et Rousseaux, critère
     d'absorption ; Commission avec production, 339 contre 468).
  Le résultat net dépend du critère et du produit.
- **Critère de Bonnet moins restrictif** : exposition > 0,4 revient à M / Prod > 0,67, contre
  M / Prod > 1 pour la Commission. On ne peut pas dire si c'est ce qui explique l'écart.
- **Ce qui est solide** : dans le cas de Bonnet, la part de la Chine ne change pas (47 %) entre
  production et commerce.
- **Pour notre C3** (importations extra-UE / exportations extra-UE) : pas de double comptage
  intra-UE (effet 1 limité aux réexportations hors UE), mais l'effet 2 joue à plein, car les
  exportations extra-UE sont souvent bien plus faibles que la production. Notre C3 est donc
  probablement peu restrictif ; c'est C2 qui fait le tri. Personne n'a testé notre C3 avec la
  production, et on ne peut pas le faire sans Prodcom. À mentionner comme limite dans la
  discussion méthodologique (point 4 de la liste de tâches).
- **Correction faite dans le qmd (30/10)** : la phrase sur Méjean et Rousseaux disait que le proxy
  commercial « overstates domestic capacities ». C'est l'inverse (leur critère d'absorption
  retire des produits). Remplacée par « the trade-based proxy is imperfect: … although these
  criteria are meant to capture domestic production capacities ».

## 5bis. Projet de section pour le WP (version courte, 30/10)

*Chiffres tapés à la main pour la relecture : à remplacer par des `{{< var >}}` produits par
Stata avant d'entrer dans le qmd.*

> # How does GeoDep compare with existing methods? {#sec-comparison}
>
> The studies reviewed in @sec-literature report very different figures for the weight of China
> in EU dependencies, from about half to 70%. These differences can come from the criteria, but
> also from the data, the year, the product nomenclature and the way the weight of China is
> measured. To separate the role of the criteria, we apply the criteria of other studies to our
> own data: BACI 2024, at the HS6 level, with the EU as a single importer. The only differences
> left between the methods are then their criteria.
>
> **Selection of the methods.** Since GeoDep is applied here to the EU, we compare it with the
> studies that identify the dependencies of the EU as a whole and whose criteria can be computed
> with product-level trade data only. Two methods meet both conditions.
> @eucommission2021strategic, also used by @vicard2023eu, define a product as dependent when the
> HHI of extra-EU imports exceeds 0.4, extra-EU imports account for more than half of total EU
> imports, and extra-EU imports exceed total EU exports, including intra-EU exports. We apply
> these three criteria without the final restriction to sensitive ecosystems, which is not fully
> documented. @chimits2024asymmetry, for MERICS, define a dependency when imports are at least
> twice as large as exports, one supplier accounts for at least 30% of imports, and the HHI of
> world exports is at least 0.25. We apply these criteria to a single year, without the two-year
> smoothing of the original study.[^smooth]
>
> The other studies do not meet these conditions. @jaravel2021intrants, @baur2022german and
> @bonneau2020vulnerabilite focus on individual countries (France, Germany and other member
> states), and the first two rely on firm-level data or input-output tables. @mejean2024identifying and @bonnet2026foreign cover the EU but
> integrate production data from Prodcom.[^bonnet] @arriola2024demystifying define dependencies
> as bilateral links rather than products: at the product level, their criteria reduce to an
> import HHI of at least 0.2, which 79% of the products imported by the EU meet.
>
> **Measuring the weight of China.** We report three measures, which correspond to those used in
> the literature: the share of dependent products for which China is the main supplier, as in
> GeoDep; the share of China in the imports of dependent products, as in
> @eucommission2021strategic; and the share of dependent products for which China is the main
> supplier, weighted by their total import value, as in @chimits2024asymmetry.
>
> [Table: GeoDep / Commission / MERICS — products, % of products, % of import value, China
> (three measures), products in common with GeoDep]
>
> | | Dependent products | % of products | % of import value | China: main supplier (% of products) | China: share of imports (%) | China: value of products it mainly supplies (%) | In common with GeoDep |
> |:--|--:|--:|--:|--:|--:|--:|--:|
> | GeoDep | 468 | 8.4 | 8.8 | 62.6 | 67.1 | 80.3 | 468 |
> | European Commission (2021) | 497 | 8.9 | 8.9 | 44.3 | 57.6 | 68.4 | 289 |
> | MERICS (2024) | 970 | 17.4 | 19.2 | 50.0 | 52.7 | 70.9 | 415 |
>
> The replication recovers the published figures for China. With the MERICS criteria, China is
> the main supplier of 485 EU dependencies, against 421 in 2022 in @chimits2024asymmetry, and
> these products make up 71% of the value of EU dependencies, against roughly 70%.
>
> Two results stand out. First, the weight of China depends above all on the measure. With the
> same criteria, it ranges from 44-50% of dependent products to 53-58% of their imports and
> 68-71% of the value of the products China mainly supplies. The figures reported in the
> literature are therefore less contradictory than they seem: the 70% of MERICS and the roughly
> 50% of the Commission describe largely similar sets of products with different measures.
>
> Second, whatever the measure, China weighs 10 to 15 points more in GeoDep than with the other
> methods. Two of our choices explain this. The first is the world concentration criterion (C2):
> most of the products identified by the Commission or MERICS but not by GeoDep fail it, and China
> is the main supplier of only a quarter of those identified by the Commission. The second is the
> treatment of the EU as a single country. The Commission counts intra-EU exports as a substitute
> for imports, and therefore excludes products that the EU trades heavily within the single market
> but imports from outside more than it exports. China is the main supplier of more than two thirds
> of these products.
>
> [^smooth]: The results change little with the EU counted as one exporter or as 27 in the world
> HHI (970 and 956 products), or with import and export values pooled over 2023-2024 (926
> products, with China the main supplier of 51% of them).
>
> [^bonnet]: @bonnet2026foreign use a trade-based exposure indicator for the products without
> production data. Applying their criteria with this indicator to all products, and converting
> their threshold with their own imputation formula, gives 885 dependent products in 2024, with
> China at 49%, 55% and 68% with the three measures, close to the Commission and MERICS.

## 6. Questions ouvertes

1. **Où mettre cet exercice dans le WP ?** Décidé : version courte, tableau à trois lignes
   (GeoDep, Commission, MERICS), autres études expliquées dans le texte (projet au §5bis).
   Reste à choisir l'emplacement : proposition, après les résultats sur les dépendances à
   l'importation. Ajouter un label `{#sec-literature}` à la revue de littérature.
2. **La persistance** : GeoDep a C4, pas la Commission ni MERICS. Sans C4, GeoDep donne 561
   produits et 59 % / 65 % / 79 % pour la Chine. En note, ou en 4e ligne du tableau ?
3. **Les inclusions (§5.3)** : GeoDep est inclus dans Baur-Flach et Arriola. Peut aller dans la
   discussion des choix méthodologiques (C2, et l'UE traitée comme un pays dans C3).
4. **Choix de réplication** :
   - ~~HHI mondial de MERICS~~ : **décidé**, l'UE comme un seul exportateur (test : aucun effet).
   - ~~Lissage sur deux ans de MERICS~~ : **décidé**, pas de lissage (test : peu d'effet).
   - ~~Chine seule ou avec Hong Kong et Macao~~ : **décidé**, Chine seule (choix discutable, à
     signaler en note).
   - ~~Garder Arriola dans le tableau~~ : **décidé**, retiré du tableau, expliqué dans le texte.
   - ~~Reproductions partielles~~ : **décidé**, retirées (§3.6). Le tableau garde GeoDep, la
     Commission, Bonnet (version commerce) et MERICS.
5. **Pipeline** : pour entrer dans le WP, les calculs doivent passer dans Stata (nouvelle section
   ou nouveau do-file, par exemple `29_dt_replication.do`), qui écrit le tableau dans `tables/` et
   les chiffres du texte dans le `.yml`. À faire seulement après accord, et après avoir vérifié
   que Pauline ne travaille pas sur les do-files.
6. **Autres pays** : refaire l'exercice pour les États-Unis (MERICS donne aussi un chiffre pour
   eux) ou pour la France (Bonneau-Nakaa) ?
