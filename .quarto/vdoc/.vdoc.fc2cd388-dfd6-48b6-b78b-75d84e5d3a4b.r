#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#| label: setup
# Carte du monde : même fonction que le document de travail (vue d'ouverture de l'Explorer).
# Carte de l'UE : R/eu_maps.R, chargé après (sa fonction show_map remplace celle du document).
source("../DT_2026/R/maps.R", encoding = "UTF-8")
source("R/eu_maps.R", encoding = "UTF-8")
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#| label: fig-world-imports
#| fig-cap: "Share of imported products that are dependent, by importer"
show_map(world_map(flow = "M"), name = "geodep_world_imports")
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#| label: fig-eu-imports
#| fig-cap: "Share of the European Union's dependent imports led by each exporter"
show_map(eu_map("M", strategic = TRUE), name = "geodep_eu_imports")
#
#
#
#
#
#
#
#
#
#
#
#
#
