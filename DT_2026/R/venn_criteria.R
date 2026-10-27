# Diagrammes de Venn des critères C1, C2, C3 (et C4 au centre), un par grande économie.
# Données : data/venn_M.csv et venn_X.csv (23 et 24). Utilisé par DT_2026.
library(tidyverse)
library(sf)
library(ggiraph)

fmt <- function(x) format(x, big.mark = ",", trim = TRUE)

country_name <- c(EUN = "European Union", CHN = "China", USA = "United States", JPN = "Japan")

# Les 7 zones (1 = dans le cercle) et leur couleur : cercles superposés en transparence
venn_zones <- tribble(
  ~key,  ~crit, ~zone_name,           ~fill,
  "100", "C1",  "C1 only",            "#EBE0CA",
  "010", "C2",  "C2 only",            "#D5D5E0",
  "001", "C3",  "C3 only",            "#F6DBCD",
  "110", NA,    "C1 and C2, not C3",  "#CDC8C9",
  "101", NA,    "C1 and C3, not C2",  "#EECDB5",
  "011", NA,    "C2 and C3, not C1",  "#E4C8BF",
  "111", NA,    "C1, C2 and C3",      "#E0C2B5")

c0 <- 0.32   # rayon minimal de la zone centrale
r4 <- 0.17   # rayon du cercle C4

# Point de la zone le plus éloigné de ses bords (pour y écrire le chiffre)
label_point <- function(zone) {
  pts <- st_sample(zone, 400, type = "regular")
  if (length(pts) == 0) return(st_coordinates(st_point_on_surface(zone))[1, 1:2])
  dist <- as.numeric(st_distance(pts, st_cast(st_boundary(zone), "MULTILINESTRING")))
  st_coordinates(pts[which.max(dist)])[1, 1:2]
}

# Forme d'une zone : les cercles qui la couvrent, moins les autres (et moins C4 au centre)
zone_shape <- function(key, disc, inner) {
  inside <- str_split(key, "")[[1]] == "1"
  shape <- reduce(disc[inside], st_intersection)
  for (k in which(!inside)) shape <- st_difference(shape, disc[[k]])
  if (all(inside)) shape <- st_difference(shape, inner)
  shape
}

# Un pays : formes des zones et chiffres
venn_country <- function(code, d, circles) {
  s  <- d %>% filter(iso == code)
  ci <- circles %>% filter(iso == code)
  n4 <- sum(s$k4)
  name <- country_name[[code]]

  disc  <- map(1:3, ~ st_buffer(st_point(c(ci$cx[.x], ci$cy[.x])), ci$r[.x], nQuadSegs = 50))
  inner <- st_buffer(st_point(c(0, 0)), r4, nQuadSegs = 50)

  zones <- venn_zones %>%
    left_join(s %>% select(key, n), by = "key") %>%
    left_join(ci %>% select(crit, total), by = "crit") %>%
    mutate(n = replace_na(n, 0),
           geometry = st_sfc(map(key, zone_shape, disc = disc, inner = inner))) %>%
    filter(!st_is_empty(geometry)) %>%
    mutate(country = name,
           id = paste(code, key),
           tip = paste0(name, "<br>", zone_name, ": <b>", fmt(n), "</b> products"),
           tip = if_else(key == "111",
                         paste0(tip, "<br>of which ", fmt(n4), " also meet C4 (dependent)"), tip))

  # Cercle C4 : les produits dépendants
  dep <- tibble(key = "dep", fill = "#7D8291", country = name, id = paste(code, "dep"),
                tip = paste0(name, "<br>Dependent (C1 to C4): <b>", fmt(n4), "</b> products"),
                geometry = st_sfc(inner))

  # Chiffres affichés : total de chaque cercle, zone centrale, C4 ; le reste au survol
  labels <- zones %>%
    filter(key %in% c("100", "010", "001", "111")) %>%
    mutate(xy = map(geometry, label_point),
           x = map_dbl(xy, 1),
           y = map_dbl(xy, 2),
           lab = if_else(key == "111", fmt(n), paste0(crit, "\n", fmt(total)))) %>%
    select(country, key, x, y, lab) %>%
    add_row(country = name, key = "dep", x = 0, y = 0, lab = fmt(n4))

  list(shapes = bind_rows(zones, dep), labels = labels)
}

venn <- function(file, id, labels) {
  d <- read_csv(file, show_col_types = FALSE) %>%
    rename(iso = all_of(id)) %>%
    mutate(key = paste0(k1, k2, k3))

  # Nombre de produits dans chaque cercle
  circles <- d %>%
    group_by(iso) %>%
    summarise(C1 = sum(n * k1), C2 = sum(n * k2), C3 = sum(n * k3)) %>%
    pivot_longer(C1:C3, names_to = "crit", values_to = "total")

  # Surface d'un cercle proportionnelle à la racine carrée du nombre de produits
  scale <- 1.25 / max(circles$total)^(1/4)
  circles <- circles %>%
    mutate(angle = recode(crit, C1 = 150, C2 = 30, C3 = 270) * pi / 180,
           r  = pmax(scale * total^(1/4), c0 + 0.1),
           cx = (r - c0) * cos(angle),
           cy = (r - c0) * sin(angle))

  parts  <- map(names(country_name), venn_country, d = d, circles = circles)
  shapes <- map(parts, "shapes") %>% bind_rows() %>% st_as_sf() %>%
    mutate(country = factor(country, levels = country_name))
  texts  <- map(parts, "labels") %>% bind_rows() %>%
    mutate(country = factor(country, levels = country_name))

  fills <- c(deframe(select(venn_zones, key, fill)), dep = "#7D8291")

  ggplot() +
    geom_sf_interactive(data = shapes, aes(fill = key, tooltip = tip, data_id = id),
                        colour = "white", linewidth = 0.4) +
    scale_fill_manual(values = fills, breaks = c("100", "010", "001"),
                      labels = labels, name = NULL) +
    geom_text(data = filter(texts, key %in% c("100", "010", "001")), aes(x, y, label = lab),
              size = 3, fontface = "bold", lineheight = 0.9) +
    geom_text(data = filter(texts, key == "111"), aes(x, y, label = lab), size = 2.8) +
    geom_text(data = filter(texts, key == "dep"), aes(x, y, label = lab), colour = "white", size = 2.6) +
    facet_wrap(~country, ncol = 2) +
    coord_sf(datum = NA) +
    theme_void() +
    theme(legend.position = "bottom",
          strip.text = element_text(face = "bold", size = 11, margin = margin(b = 4)))
}

# Note sous la figure : nombre de produits de chaque économie, et année
venn_total <- function(file, id) {
  read_csv(file, show_col_types = FALSE) %>%
    rename(iso = all_of(id)) %>%
    group_by(iso) %>%
    summarise(n = sum(n)) %>%
    arrange(match(iso, names(country_name))) %>%
    summarise(text = paste(country_name[iso], fmt(n), collapse = ", ")) %>%
    pull(text)
}

# Chiffres du texte (fichier venn_M_all.csv ou venn_X_all.csv : tous les pays)
venn_read <- function(file, id) {
  read_csv(file, show_col_types = FALSE) %>%
    rename(iso = all_of(id), C1 = k1, C2 = k2, C3 = k3, C4 = k4)
}

# Produits de chaque pays qui remplissent tous les critères de crit (ex : c("C1", "C2"))
venn_count <- function(d, crit) {
  d %>%
    mutate(ok = if_all(all_of(setdiff(crit, "C4")), ~ .x == 1),
           n = if ("C4" %in% crit) C4 else n) %>%
    group_by(iso) %>%
    summarise(n = sum(n * ok))
}

# Part (%) des produits remplissant crit, parmi ceux remplissant of (tous les produits si vide)
venn_shares <- function(file, id, crit, of = NULL) {
  d <- venn_read(file, id)
  venn_count(d, crit) %>%
    left_join(venn_count(d, of), by = "iso", suffix = c("", "_of")) %>%
    mutate(pct = 100 * n / n_of)
}

# Nombre de produits d'un pays
venn_n <- function(file, id, code, crit = NULL) {
  venn_count(venn_read(file, id), crit) %>% filter(iso == code) %>% pull(n) %>% fmt()
}

# Part (%) d'un pays
venn_pct <- function(file, id, code, crit, of = NULL) {
  venn_shares(file, id, crit, of) %>% filter(iso == code) %>% pull(pct) %>% round()
}

# Part (%) médiane des autres pays (hors 4 principales économies)
venn_median <- function(file, id, crit, of = NULL) {
  venn_shares(file, id, crit, of) %>%
    filter(!iso %in% names(country_name)) %>%
    pull(pct) %>% median(na.rm = TRUE) %>% round()
}

# Nombre d'autres pays
venn_n_other <- function(file, id) {
  venn_read(file, id) %>% filter(!iso %in% names(country_name)) %>% distinct(iso) %>% nrow()
}

venn_year <- function(file) {
  read_csv(file, show_col_types = FALSE) %>% pull(year) %>% first()
}

# HTML : la zone survolée s'éclaire, les autres s'estompent. PDF : figure fixe.
show_venn <- function(p) {
  if (!knitr::is_html_output()) return(p)
  girafe(ggobj = p, width_svg = 8, height_svg = 8.4,
         options = list(opts_hover(css = "stroke:#2e2b36;stroke-width:1.5px;"),
                        opts_hover_inv(css = "opacity:0.35;"),
                        opts_tooltip(css = "background:#ffffff;color:#2e2b36;padding:6px 9px;border:1px solid #ccc;border-radius:4px;font-size:13px;"),
                        opts_toolbar(saveaspng = FALSE)))
}
