# Sensibilité aux seuils : produits dépendants des 4 grandes économies quand un seuil varie.
# Données : data/sensitivity_curves.csv (25_dt_sensitivity.do). Utilisé par DT_2026.
library(tidyverse)
library(ggiraph)

fmt <- function(x) format(x, big.mark = ",", trim = TRUE)

country_name <- c(EUN = "European Union", CHN = "China", USA = "United States", JPN = "Japan")
col_country  <- c("European Union" = "#5F6473", "China" = "#E09B7D",
                  "United States" = "#8C8CAF", "Japan" = "#C3A569")
flow_lab     <- c(M = "Import dependencies", X = "Export dependencies")
crit_lab     <- c(C1 = "C1 threshold (HHI)", C2 = "C2 threshold (HHI)", C3 = "C3 threshold (ratio)")
base_thr     <- c(C1 = 0.4, C2 = 0.4, C3 = 1)

# Texte d'un seuil pour le survol : en gras si c'est celui qui varie
thr_text <- function(c, criterion, threshold) {
  if_else(criterion == c,
          paste0("<b>", c, " > ", threshold, "</b>"),
          paste0(c, " > ", base_thr[[c]]))
}

sensitivity_curves <- function(file) {
  d <- read_csv(file, show_col_types = FALSE) %>%
    mutate(threshold = round(threshold, 2),
           country = factor(country_name[iso], levels = country_name),
           tip = paste0(country, "<br>",
                        thr_text("C1", criterion, threshold), ", ",
                        thr_text("C2", criterion, threshold), ", ",
                        thr_text("C3", criterion, threshold),
                        "<br><b>", fmt(n), "</b> dependent products"),
           id = paste(flow, criterion, threshold, iso),
           flow = factor(flow_lab[flow], levels = flow_lab),
           criterion = factor(crit_lab[criterion], levels = crit_lab))

  # Ligne verticale : seuil de référence
  base <- tibble(criterion = factor(crit_lab, levels = crit_lab), x = base_thr)

  ggplot(d, aes(threshold, n, colour = country)) +
    geom_vline(data = base, aes(xintercept = x), colour = "grey70", linetype = "dashed", linewidth = 0.4) +
    geom_line(linewidth = 0.8) +
    geom_point_interactive(aes(tooltip = tip, data_id = id), size = 1.4) +
    facet_grid(flow ~ criterion, scales = "free", switch = "y") +
    scale_colour_manual(values = col_country, name = NULL) +
    scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05))) +
    labs(x = NULL, y = NULL) +
    theme_minimal(base_size = 10) +
    theme(panel.grid.minor = element_blank(),
          strip.text = element_text(face = "bold", size = 9.5),
          strip.placement = "outside",
          panel.spacing = unit(1, "lines"),
          legend.position = "bottom")
}

# HTML : le point survolé affiche le nombre de produits. PDF : figure fixe.
show_sensitivity <- function(p) {
  if (!knitr::is_html_output()) return(p)
  girafe(ggobj = p, width_svg = 8, height_svg = 5.2,
         options = list(opts_hover(css = "stroke:#2e2b36;stroke-width:1.5px;"),
                        opts_tooltip(css = "background:#ffffff;color:#2e2b36;padding:6px 9px;border:1px solid #ccc;border-radius:4px;font-size:13px;"),
                        opts_toolbar(saveaspng = FALSE)))
}

# Chiffres du texte : produits dépendants d'un pays pour un seuil (ex : "M", "C2", 0.3, "EUN")
sens_n <- function(file, fl, crit, thr, code) {
  read_csv(file, show_col_types = FALSE) %>%
    filter(flow == fl, criterion == crit, round(threshold, 2) == thr, iso == code) %>%
    pull(n)
}

# Variation (%) par rapport au seuil de référence, avec son signe (ex : "+41")
sens_change <- function(file, fl, crit, thr, code) {
  base <- sens_n(file, fl, crit, base_thr[[crit]], code)
  sprintf("%+d", round(100 * (sens_n(file, fl, crit, thr, code) / base - 1)))
}

sensitivity_year <- function(file) {
  read_csv(file, show_col_types = FALSE) %>% pull(year) %>% first()
}
