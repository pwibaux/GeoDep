# Part du premier partenaire selon le niveau du HHI (critères C1 et C2, importations et exportations).
# Données : data/hhi_topshare.csv (25_dt_sensitivity.do). Utilisé par DT_2026.
library(tidyverse)
library(ggiraph)

criterion_lab <- c(
  M_C1 = "Imports, C1: destination's imports across origins",
  M_C2 = "Imports, C2: world exports across exporters",
  X_C1 = "Exports, C1: origin's exports across destinations",
  X_C2 = "Exports, C2: world imports across importers")

# Tranches de HHI ; celles de moins de n_min produits sont écartées
hhi_read <- function(file, n_min = 10) {
  read_csv(file, show_col_types = FALSE) %>%
    mutate(bin = round(bin, 2)) %>%
    filter(n >= n_min)
}

hhi_topshare <- function(file, thr = 0.4) {
  d <- hhi_read(file) %>%
    mutate(criterion = factor(criterion_lab[criterion], levels = criterion_lab),
           tip = paste0("HHI: <b>", sprintf("%.2f", bin), "</b>",
                        "<br>Share of the main partner: <b>", round(p50), "%</b>"),
           id = paste(criterion, bin))

  at <- d %>% filter(bin == thr)

  ggplot(d, aes(bin)) +
    geom_ribbon(aes(ymin = p25, ymax = p75, fill = "Interquartile range"), alpha = 0.7) +
    geom_line(aes(y = p50, colour = "Median"), linewidth = 0.9) +
    geom_vline(xintercept = thr, colour = "#D2967D", linewidth = 0.5) +
    geom_point(data = at, aes(y = p50), colour = "#7D8291", size = 2.2) +
    geom_label(data = at, aes(y = p50, label = paste0(round(p50), "%")), hjust = 1.15, vjust = -0.2,
               size = 3, linewidth = 0, fill = "white", colour = "#3C3C46") +
    # Points invisibles sur la médiane : ils apparaissent au survol, avec les chiffres
    geom_point_interactive(aes(y = p50, tooltip = tip, data_id = id), size = 3,
                           colour = "#7D8291", alpha = 0) +
    facet_wrap(~criterion, ncol = 2) +
    scale_colour_manual(values = c("Median" = "#7D8291"), name = NULL) +
    scale_fill_manual(values = c("Interquartile range" = "#DCC8A0"), name = NULL) +
    guides(colour = guide_legend(order = 1), fill = guide_legend(order = 2)) +
    scale_x_continuous(breaks = seq(0, 1, 0.2), expand = expansion(mult = 0.01)) +
    scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 25), expand = expansion(mult = 0.01)) +
    labs(x = "HHI", y = "Share of the main partner (%)") +
    theme_minimal(base_size = 10) +
    theme(panel.grid.minor = element_blank(),
          strip.text = element_text(face = "bold", size = 9.5, hjust = 0),
          panel.spacing = unit(1.2, "lines"),
          legend.position = "bottom")
}

# HTML : le point survolé apparaît avec ses chiffres. PDF : figure fixe.
show_hhi <- function(p) {
  if (!knitr::is_html_output()) return(p)
  girafe(ggobj = p, width_svg = 8, height_svg = 6,
         options = list(opts_hover(css = "fill-opacity:1;stroke-opacity:1;stroke:#ffffff;stroke-width:1px;"),
                        opts_tooltip(css = "background:#ffffff;color:#2e2b36;padding:6px 9px;border:1px solid #ccc;border-radius:4px;font-size:13px;"),
                        opts_toolbar(saveaspng = FALSE)))
}

# Part du premier partenaire au seuil, pour un critère (ex : "M_C1")
# stat : "p50" (médiane), "p25" ou "p75" (quartiles)
hhi_at <- function(file, criterion, thr = 0.4, stat = "p50") {
  code <- criterion
  hhi_read(file) %>%
    filter(criterion == code, bin == thr) %>%
    pull(all_of(stat)) %>%
    round()
}

hhi_year <- function(file) {
  read_csv(file, show_col_types = FALSE) %>% pull(year) %>% first()
}
