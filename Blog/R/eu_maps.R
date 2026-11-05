# Cartes des partenaires de l'UE : part des produits dépendants de l'UE dont chaque pays est
# le premier fournisseur (importations) ou le premier client (exportations).
# Données, contours, classes et couleurs : application geodep_shiny (preparation.R, app.R).
library(tidyverse)
library(sf)
library(ggiraph)

app_inputs <- readRDS("../geodep_shiny/1_Data/geodep_shiny_inputs.rds")
world      <- app_inputs$world_polygons %>% select(iso_a3, name)
eu_members <- app_inputs$eu_countries

# Couleurs de l'application (app.R) : bleu pétrole, plus clair pour les exportations
map_col <- list(M = c("#DCEBEE", "#AED1D7", "#83B7C0", "#5B9CA8", "#3C8090", "#266476", "#16465A"),
                X = c("#E4EFF1", "#CAE1E5", "#B2D3D9", "#90BFC7", "#71ABB5", "#5395A2", "#387C8D"))
col_zero   <- "#C9C5BD"   # pays avec des données, premier partenaire d'aucun produit (0 %)
col_nodata <- "#EEECE8"
col_ocean  <- "#F7F8F8"
col_globe  <- "#D9D7D2"
ink        <- "#3A4048"   # pays sélectionné (l'UE)
equal_earth <- "+proj=eqearth +lon_0=0 +datum=WGS84 +units=m +no_defs"

# Classes : même règle que partner_breaks() dans app.R (paliers fixes jusqu'au maximum)
partner_breaks <- function(x, n_classes = 7) {
  steps <- c(0.1, 0.25, 0.5, 1, 2, 5, 10, 25, 50)
  pos   <- x[!is.na(x) & x > 0]
  below <- tail(steps[steps < max(pos)], n_classes - 1)
  unique(c(0, below, ceiling(max(pos))))
}

# Part des produits dépendants de l'UE menés par chaque partenaire : même calcul que
# partner_map_data() dans app.R. Pays avec des données mais sans produit mené : 0 %.
eu_partners <- function(flow = "M", strategic = FALSE) {
  if (flow == "M") {
    d <- app_inputs$dep_import_base %>% filter(iso_d == "EUN") %>% rename(partner = iso_o)
    if (strategic) d <- d %>% filter(sect_strategic %in% 1)
    with_data <- app_inputs$countries_with_data_import
  } else {
    d <- app_inputs$dep_export_base %>% filter(iso_o == "EUN") %>% rename(partner = iso_d)
    with_data <- app_inputs$countries_with_data_export
  }
  n_total <- n_distinct(d$hs6)
  d %>%
    group_by(partner) %>%
    summarise(n_led = n_distinct(hs6), .groups = "drop") %>%
    full_join(tibble(partner = with_data), by = "partner") %>%
    mutate(n_led = replace_na(n_led, 0), share = 100 * n_led / n_total)
}

eu_map <- function(flow = "M", strategic = FALSE) {
  d    <- eu_partners(flow, strategic)
  brk  <- partner_breaks(d$share)
  n    <- length(brk) - 1
  ramp <- map_col[[flow]]
  cols <- if (n == length(ramp)) ramp else ramp[round(seq(1, length(ramp), length.out = n))]
  fmt  <- function(v) format(v, trim = TRUE, drop0trailing = TRUE)
  keys <- tibble(col = c(cols, col_zero, col_nodata, ink),
                 lab = c(paste0(fmt(brk[1:n]), "–", fmt(brk[2:(n + 1)])),
                         "Not dependent (0%)", "No data", "Selected country"))

  # Les pays de l'UE forment un seul pays (même data_id : survolés ensemble)
  m <- world %>%
    mutate(iso3 = if_else(iso_a3 %in% eu_members, "EUN", iso_a3),
           name = if_else(iso3 == "EUN", "European Union", name)) %>%
    left_join(d, by = c("iso3" = "partner")) %>%
    mutate(cls  = cut(pmin(share, max(brk)), breaks = brk, include.lowest = TRUE, labels = FALSE),
           fill = case_when(iso3 == "EUN"  ~ ink,
                            is.na(share)   ~ col_nodata,
                            share == 0     ~ col_zero,
                            TRUE           ~ cols[pmax(1, pmin(n, cls))]),
           tip  = case_when(iso3 == "EUN" ~ "<b>European Union</b><br>Selected country",
                            is.na(share)  ~ paste0("<b>", name, "</b><br>No data"),
                            TRUE          ~ paste0("<b>", name, "</b><br>Leads ", n_led,
                                                   " dependent products (", format(round(share, 1), nsmall = 1), "%)")))

  # Contour du globe, du 60e parallèle sud au pôle Nord, et étiquette de l'UE (comme l'application)
  lats  <- seq(-60, 90, by = 1)
  lons  <- seq(-180, 180, by = 1)
  ring  <- cbind(c(rep(-180, length(lats)), lons, rep(180, length(lats)), rev(lons)),
                 c(lats, rep(90, length(lons)), rev(lats), rep(-60, length(lons))))
  globe <- st_sfc(st_polygon(list(rbind(ring, ring[1, ]))), crs = 4326)
  label <- st_sf(lab = "European Union", geometry = st_sfc(st_point(c(10, 50)), crs = 4326))

  old_s2 <- suppressMessages(sf_use_s2(FALSE))
  on.exit(suppressMessages(sf_use_s2(old_s2)))
  ggplot() +
    geom_sf(data = globe, fill = col_ocean, colour = col_globe, linewidth = 0.3) +
    geom_sf_interactive(data = m, aes(fill = fill, tooltip = tip, data_id = iso3),
                        colour = "white", linewidth = 0.08, show.legend = TRUE) +
    geom_sf_label(data = label, aes(label = lab), size = 2.6, colour = ink, fill = "white",
                  linewidth = 0.2, fontface = "bold") +
    scale_fill_manual(values = setNames(keys$col, keys$col), breaks = keys$col, labels = keys$lab,
                      limits = keys$col, drop = FALSE, name = "Share (%)") +
    coord_sf(crs = equal_earth, datum = NA, expand = FALSE) +
    guides(fill = guide_legend(nrow = 2, byrow = TRUE, title.position = "top")) +
    theme_void(base_size = 10) +
    theme(legend.position = "bottom",
          legend.title = element_text(size = 9, face = "bold", colour = ink),
          legend.text  = element_text(size = 8, colour = "#444444"),
          legend.key.width = unit(14, "pt"), legend.key.height = unit(10, "pt"))
}

# Carte interactive (chiffres au survol). Barre d'outils : plein écran et téléchargement en PNG.
show_map <- function(p, name = "geodep_map") {
  girafe(ggobj = p, width_svg = 9, height_svg = 5.4,
         options = list(opts_hover(css = paste0("stroke:", ink, ";stroke-width:0.8px;")),
                        opts_tooltip(css = "background:#ffffff;color:#2e2b36;padding:6px 9px;border:1px solid #ccc;border-radius:4px;font-size:13px;"),
                        opts_toolbar(pngname = name, hidden = c("selection", "zoom"))))
}
