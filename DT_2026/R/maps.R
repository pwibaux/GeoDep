# Carte du monde : part des produits importés (exportés) qui sont dépendants, par pays.
# Données, contours et couleurs : application geodep_shiny (1_Data/geodep_shiny_inputs.rds, app.R).
library(tidyverse)
library(sf)
library(ggiraph)

# Contours des pays (sans l'Antarctique, coupés au 180e méridien) et pays de l'UE
app_inputs <- readRDS("../geodep_shiny/1_Data/geodep_shiny_inputs.rds")
world      <- app_inputs$world_polygons %>% select(iso_a3, name)
eu_members <- app_inputs$eu_countries

# Part des produits dépendants par pays : même calcul que sector_map_data() dans app.R
# (dépendant = critères C1 à C3 en t et C4)
map_share <- function(d) {
  d %>%
    distinct(iso3, hs6, dep, val) %>%
    group_by(iso3) %>%
    summarise(n_prod = n(), n_dep = sum(dep),
              value = sum(val, na.rm = TRUE), value_dep = sum(val[dep], na.rm = TRUE), .groups = "drop") %>%
    mutate(share = 100 * n_dep / n_prod, share_value = 100 * value_dep / value)
}
map_data <- list(
  M = app_inputs$traded_by_country %>%
    transmute(iso3 = iso_d, hs6, dep = dependant_M_MC_t %in% 1 & c4_M_MC %in% 1, val = import_dpt) %>%
    map_share(),
  M_strat = app_inputs$traded_by_country %>%
    filter(sect_strategic %in% 1) %>%
    transmute(iso3 = iso_d, hs6, dep = dependant_M_MC_t %in% 1 & c4_M_MC %in% 1, val = import_dpt) %>%
    map_share(),
  X = app_inputs$traded_by_country_export %>%
    transmute(iso3 = iso_o, hs6, dep = dependant_X_MC_t %in% 1 & c4_X_MC %in% 1, val = export_opt) %>%
    map_share()
)
rm(app_inputs)

# Couleurs de l'application (app.R) : bleu pétrole, plus clair pour les exportations
map_col <- list(M = c("#DCEBEE", "#AED1D7", "#83B7C0", "#5B9CA8", "#3C8090", "#266476", "#16465A"),
                X = c("#E4EFF1", "#CAE1E5", "#B2D3D9", "#90BFC7", "#71ABB5", "#5395A2", "#387C8D"))
col_zero   <- "#C9C5BD"   # pays sans produit dépendant (0 %)
col_nodata <- "#EEECE8"
col_ocean  <- "#F7F8F8"
col_globe  <- "#D9D7D2"
ink        <- "#3A4048"
equal_earth <- "+proj=eqearth +lon_0=0 +datum=WGS84 +units=m +no_defs"

# Limites des classes : même règle que map_breaks() dans app.R (7 classes aux limites rondes,
# les pays les moins dépendants dans la plus claire)
map_breaks <- function(x, n_classes = 7) {
  pos  <- x[!is.na(x) & x > 0]
  top  <- max(pos)
  n_in <- n_classes - 1
  q10  <- quantile(pos, 0.10, names = FALSE)
  q75  <- quantile(pos, 0.75, names = FALSE)
  crowding <- function(w) max(tabulate(findInterval(pos, w) + 1, n_classes)) / length(pos)
  ok       <- function(w) w[1] <= q10 && w[n_in] >= q75 && w[n_in] < top
  best <- NULL; best_score <- Inf
  try_w <- function(w) {
    if (ok(w) && crowding(w) < best_score - 1e-9) { best <<- w; best_score <<- crowding(w) }
  }
  # Pas réguliers (0,5, 1, 2...), puis progressions 1-2-5 ou 1-3-10 si nettement meilleures
  for (step in c(0.01, 0.02, 0.025, 0.05, 0.1, 0.2, 0.25, 0.5, 1, 2, 2.5, 5, 10, 20, 25)) {
    for (k in 1:ceiling(q10 / step)) try_w(step * (k + 0:(n_in - 1)))
  }
  lin <- best; lin_score <- best_score
  best <- NULL; best_score <- Inf
  for (m in list(c(1, 2, 5), c(1, 3))) {
    cand <- sort(as.vector(outer(m, 10^(-3:2))))
    for (i in seq_len(length(cand) - n_in + 1)) try_w(cand[i:(i + n_in - 1)])
  }
  inner <- if (!is.null(best) && (is.null(lin) || best_score < 0.75 * lin_score)) best else lin
  if (is.null(inner)) inner <- pretty(c(0, top), n = n_in)[-1]
  top_lab <- if (top >= 10) ceiling(top) else ceiling(top * 10) / 10
  unique(c(0, inner[inner < top], top_lab))
}

world_map <- function(flow = "M") {
  d    <- map_data[[flow]]
  brk  <- map_breaks(d$share)
  n    <- length(brk) - 1
  ramp <- map_col[[flow]]
  cols <- if (n == length(ramp)) ramp else ramp[round(seq(1, length(ramp), length.out = n))]
  fmt  <- function(v) format(v, trim = TRUE, drop0trailing = TRUE)
  keys <- tibble(col = c(cols, col_zero, col_nodata),
                 lab = c(paste0(fmt(brk[1:n]), "–", fmt(brk[2:(n + 1)])), "Not dependent (0%)", "No data"))

  # Les pays de l'UE prennent la valeur de l'UE (même data_id : survolés ensemble)
  traded <- if (flow == "M") "Imported" else "Exported"
  m <- world %>%
    mutate(iso3 = if_else(iso_a3 %in% eu_members, "EUN", iso_a3),
           name = if_else(iso3 == "EUN", "European Union", name)) %>%
    left_join(d, by = "iso3") %>%
    mutate(cls  = cut(pmin(share, max(brk)), breaks = brk, include.lowest = TRUE, labels = FALSE),
           fill = case_when(is.na(share) ~ col_nodata,
                            share == 0   ~ col_zero,
                            TRUE         ~ cols[pmax(1, pmin(n, cls))]),
           tip  = if_else(is.na(share),
                          paste0("<b>", name, "</b><br>No data"),
                          paste0("<b>", name, "</b>",
                                 "<br>Dependent products: <b>", n_dep, "</b>",
                                 "<br>", traded, " products: ", format(n_prod, big.mark = ","),
                                 "<br>Share dependent: <b>", format(round(share, 1), nsmall = 1), "%</b>")))

  # Contour du globe, du 60e parallèle sud au pôle Nord
  lats  <- seq(-60, 90, by = 1)
  lons  <- seq(-180, 180, by = 1)
  ring  <- cbind(c(rep(-180, length(lats)), lons, rep(180, length(lats)), rev(lons)),
                 c(lats, rep(90, length(lons)), rev(lats), rep(-60, length(lons))))
  globe <- st_sfc(st_polygon(list(rbind(ring, ring[1, ]))), crs = 4326)

  old_s2 <- suppressMessages(sf_use_s2(FALSE))
  on.exit(suppressMessages(sf_use_s2(old_s2)))
  ggplot() +
    geom_sf(data = globe, fill = col_ocean, colour = col_globe, linewidth = 0.3) +
    geom_sf_interactive(data = m, aes(fill = fill, tooltip = tip, data_id = iso3),
                        colour = "white", linewidth = 0.08) +
    scale_fill_manual(values = setNames(keys$col, keys$col), breaks = keys$col, labels = keys$lab,
                      limits = keys$col, drop = FALSE, name = "Share of products dependent (%)") +
    coord_sf(crs = equal_earth, datum = NA, expand = FALSE) +
    guides(fill = guide_legend(nrow = 1, title.position = "top")) +
    theme_void(base_size = 10) +
    theme(legend.position = "bottom",
          legend.title = element_text(size = 9, face = "bold", colour = ink),
          legend.text  = element_text(size = 8, colour = "#444444"),
          legend.key.width = unit(14, "pt"), legend.key.height = unit(10, "pt"))
}

# HTML : carte interactive (chiffres au survol, zoom). PDF : carte fixe.
show_map <- function(p) {
  if (!knitr::is_html_output()) return(p)
  girafe(ggobj = p, width_svg = 9, height_svg = 5,
         options = list(opts_hover(css = paste0("stroke:", ink, ";stroke-width:0.8px;")),
                        opts_tooltip(css = "background:#ffffff;color:#2e2b36;padding:6px 9px;border:1px solid #ccc;border-radius:4px;font-size:13px;"),
                        opts_zoom(min = 1, max = 4),
                        opts_toolbar(saveaspng = FALSE)))
}

# Année des données : celle du document (_variables.yml), comme dans l'application
map_year <- function() {
  yaml::read_yaml("_variables.yml")$year
}

# Nombre de pays de la carte (pour la note)
map_n <- function(flow = "M") {
  nrow(map_data[[flow]])
}

# Chiffres du texte : part moyenne ou médiane entre pays (flow : "M", "M_strat" ou "X"),
# en nombre de produits ("share") ou en valeur ("share_value"), avec une décimale
dep_stat <- function(flow, var = "share", stat = median) {
  format(round(stat(map_data[[flow]][[var]]), 1), nsmall = 1)
}

# Nombre de pays dont la part (en valeur par défaut) dépasse un seuil
dep_n_above <- function(flow, threshold, var = "share_value") {
  sum(map_data[[flow]][[var]] > threshold)
}