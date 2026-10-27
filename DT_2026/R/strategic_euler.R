# Diagramme d'Euler des listes stratégiques : produits HS6 communs à plusieurs listes.
# Données : data/strategic_overlap.csv (22_dt_lists_variables.do). Utilisé par DT_2026.
library(tidyverse)
library(sf)
library(ggiraph)

fmt <- function(x) format(x, big.mark = ",", trim = TRUE)

list_name  <- c(sect_crm = "CRM", sect_dual_use = "Dual use", sect_health = "Health",
                sect_energy = "Energy", sect_agrifood = "Agrifood")
col_list   <- c("CRM" = "#DCC8A0", "Dual use" = "#B4B4C8", "Health" = "#F0BEA5",
                "Agrifood" = "#BEC89B", "Energy" = "#B9A596")
col_shared <- c("#D7D7DC", "#A5A8B4")   # zone commune à 2 listes, à 3 listes ou plus
euler_min  <- 20                         # en dessous : pas de chiffre (survol en HTML)
# Point de départ du placement : 6 donne la même disposition qu'avant la correction Dual use
euler_seed <- 6

# "CRM", "Dual use", "Health" -> "CRM, Dual use and Health"
and_list <- function(s) {
  if (length(s) == 1) return(s)
  paste(paste(head(s, -1), collapse = ", "), "and", tail(s, 1))
}

# Effectifs de chaque combinaison de listes, au format d'eulerr ("CRM&Dual use" = 97)
euler_counts <- function(file) {
  d <- read_csv(file, show_col_types = FALSE) %>%
    mutate(row = row_number())

  keys <- d %>%
    pivot_longer(all_of(names(list_name)), names_to = "list", values_to = "inside") %>%
    filter(inside == 1) %>%
    group_by(row) %>%
    summarise(key = paste(list_name[list], collapse = "&"))

  d %>%
    inner_join(keys, by = "row") %>%
    select(key, n) %>%
    deframe()
}

# Point de la zone le plus éloigné de ses bords (pour y écrire le chiffre)
label_point <- function(zone) {
  pts <- st_sample(zone, 400, type = "regular")
  if (length(pts) == 0) return(st_coordinates(st_point_on_surface(zone))[1, 1:2])
  dist <- as.numeric(st_distance(pts, st_cast(st_boundary(zone), "MULTILINESTRING")))
  st_coordinates(pts[which.max(dist)])[1, 1:2]
}

# Forme d'une zone : les cercles des listes qui la couvrent, moins les autres
euler_shape <- function(inside, disc) {
  shape <- reduce(disc[inside], st_intersection)
  for (o in setdiff(names(disc), inside)) shape <- st_difference(shape, disc[[o]])
  shape
}

euler_plot <- function(file) {
  v <- euler_counts(file)
  set.seed(euler_seed)                   # placement reproductible
  fit <- eulerr::euler(v, shape = "circle")

  # Un cercle par liste, placé par eulerr
  e <- fit$ellipses
  disc <- map(seq_len(nrow(e)), ~ st_buffer(st_point(c(e$h[.x], e$k[.x])), e$a[.x], nQuadSegs = 90)) %>%
    set_names(rownames(e))
  min_area <- 1e-6 * sum(map_dbl(disc, st_area))

  # Nombre de produits de chaque liste entière (pour le survol)
  list_total <- function(l) sum(v[map_lgl(str_split(names(v), "&"), ~ l %in% .x)])

  zones <- tibble(key = names(v), n = v) %>%
    mutate(inside = str_split(key, "&"),
           geometry = st_sfc(map(inside, euler_shape, disc = disc))) %>%
    filter(!st_is_empty(geometry), st_area(geometry) >= min_area) %>%
    mutate(single = lengths(inside) == 1,
           list = if_else(single, map_chr(inside, 1), ""),
           fill = case_when(single ~ col_list[list],
                            lengths(inside) == 2 ~ col_shared[1],
                            TRUE ~ col_shared[2]),
           tip  = paste0(map_chr(inside, and_list), " only: <b>", fmt(n), "</b> products"),
           tip  = if_else(single, paste0(tip, "<br>Whole list: ", fmt(map_dbl(list, list_total)), " products"), tip),
           xy = map(geometry, label_point),
           x = map_dbl(xy, 1),
           y = map_dbl(xy, 2),
           lab = if_else(n >= euler_min, fmt(n), ""))

  shapes <- zones %>% select(key, fill, tip, geometry) %>% st_as_sf()
  texts  <- zones %>% select(single, list, x, y, lab)

  ggplot() +
    geom_sf_interactive(data = shapes, aes(fill = fill, tooltip = tip, data_id = key),
                        colour = "white", linewidth = 0.6) +
    scale_fill_identity() +
    geom_text(data = filter(texts, single), aes(x, y, label = list), vjust = -0.3,
              size = 3.4, fontface = "bold", colour = "#3C3C46") +
    geom_text(data = filter(texts, single), aes(x, y, label = lab), vjust = 1.3,
              size = 3, colour = "#3C3C46") +
    geom_text(data = filter(texts, !single), aes(x, y, label = lab), size = 2.8, colour = "#3C3C46") +
    coord_sf(datum = NA) +
    theme_void()
}

# HTML : la zone survolée s'éclaire, les autres s'estompent. PDF : figure fixe.
show_euler <- function(p) {
  if (!knitr::is_html_output()) return(p)
  girafe(ggobj = p, width_svg = 7, height_svg = 5.5,
         options = list(opts_hover(css = "stroke:#2e2b36;stroke-width:1.5px;"),
                        opts_hover_inv(css = "opacity:0.35;"),
                        opts_tooltip(css = "background:#ffffff;color:#2e2b36;padding:6px 9px;border:1px solid #ccc;border-radius:4px;font-size:13px;"),
                        opts_toolbar(saveaspng = FALSE)))
}

# Note sous la figure : combinaisons trop petites pour être dessinées avec des cercles
euler_missing <- function(file) {
  v <- euler_counts(file)
  set.seed(euler_seed)
  fit <- eulerr::euler(v, shape = "circle")

  # eulerr ne range pas les listes dans le même ordre : on compare des noms triés
  sort_key <- function(k) map_chr(str_split(k, "&"), ~ paste(sort(.x), collapse = "&"))
  fitted <- tibble(sorted = sort_key(names(fit$fitted.values)), fitted = fit$fitted.values)

  miss <- tibble(key = names(v), n = v) %>%
    mutate(sorted = sort_key(key)) %>%
    left_join(fitted, by = "sorted") %>%
    filter(n > 0, round(fitted) == 0) %>%
    mutate(text = paste0(map_chr(str_split(key, "&"), and_list), " (", fmt(n), ")"))

  if (nrow(miss) == 0) return("")
  paste0("Combinations too small to be drawn with circles: ",
         paste(miss$text, collapse = "; "), ". ")
}
