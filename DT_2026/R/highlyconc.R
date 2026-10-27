# Figure : produits très concentrés entre l'UE et la Chine, le Japon, les États-Unis.
# Données : data/highlyconc.csv (21_dt_introduction.do). Utilisé par DT_2026.
library(tidyverse)

fmt <- function(x) format(x, big.mark = ",", trim = TRUE)

# Couleurs : côté UE, côté partenaire (barres puis titres)
col_side <- c(eu = "#7D8291", partner = "#DCC8A0")
col_head <- c(eu = "#5F6473", partner = "#A08C5F")

highlyconc <- function(file) {
  d <- read_csv(file, show_col_types = FALSE) %>%
    mutate(partner = recode(partner, CHN = "China", JPN = "Japan", USA = "United States"),
           partner = factor(partner, levels = c("United States", "Japan", "China")),
           flow    = factor(flow, levels = c("M", "X"), labels = c("Imports", "Exports")))

  # Côté UE à gauche (en négatif), côté partenaire à droite
  bars <- bind_rows(
    d %>% transmute(flow, partner, side = "eu", n = -bar_eu),
    d %>% transmute(flow, partner, side = "partner", n = bar_partner))

  # Demi-largeur de chaque panneau, avec de la place pour les chiffres
  width <- bars %>%
    group_by(flow) %>%
    summarise(m = max(abs(n)) * 1.3)

  edges <- bind_rows(
    width %>% transmute(flow, n = -m),
    width %>% transmute(flow, n = m))

  # Titres au-dessus des barres, au milieu de chaque côté
  heads <- tibble(
    flow  = factor(c("Imports", "Imports", "Exports", "Exports"), levels = c("Imports", "Exports")),
    side  = c("eu", "partner", "eu", "partner"),
    label = c("EU imports\nfrom partner", "Partner imports\nfrom EU",
              "EU exports\nto partner", "Partner exports\nto EU")) %>%
    left_join(width, by = "flow") %>%
    mutate(x = if_else(side == "eu", -m / 2, m / 2))

  ggplot(bars, aes(n, partner)) +
    geom_blank(data = edges, aes(n, 1)) +
    geom_col(aes(fill = side), width = 0.65) +
    geom_vline(xintercept = 0, colour = "grey30", linewidth = 0.4) +
    geom_text(aes(label = fmt(abs(n)), hjust = if_else(n < 0, 1.15, -0.15)), size = 3.2) +
    geom_text(data = heads, aes(x, 3.85, label = label, colour = side),
              size = 3.2, fontface = "bold", lineheight = 0.9) +
    facet_wrap(~flow, scales = "free_x") +
    scale_fill_manual(values = col_side, guide = "none") +
    scale_colour_manual(values = col_head, guide = "none") +
    scale_y_discrete(expand = expansion(add = c(0.6, 1.2))) +
    coord_cartesian(clip = "off") +
    theme_minimal(base_size = 11) +
    theme(panel.grid = element_blank(),
          axis.title = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_text(size = 10, colour = "grey20"),
          panel.spacing = unit(2.5, "lines"),
          strip.text = element_text(face = "bold", size = 11, margin = margin(b = 4)))
}

highlyconc_year <- function(file) {
  read_csv(file, show_col_types = FALSE) %>% pull(year) %>% first()
}
