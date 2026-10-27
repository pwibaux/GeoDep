# Premier partenaire des produits dépendants : médiane et quartiles entre pays, plus 4 grandes économies.
# Données : data/first_origin_M.csv (23) et first_destination_X.csv (24). Utilisé par DT_2026.
library(tidyverse)
library(ggiraph)

country_name <- c(EUN = "European Union", CHN = "China", USA = "United States", JPN = "Japan")

# Les deux sens d'échange : colonnes du fichier et mots des libellés
fo_flow <- list(
  M = list(unit = "iso_d", partner = "iso_o", name = "name_o", country = "importer",
           role = "main supplier", other = "Other origins", other_low = "other origins",
           another = "another origin", whose = "the origin"),
  X = list(unit = "iso_o", partner = "iso_d", name = "name_d", country = "exporter",
           role = "main importer", other = "Other destinations", other_low = "other destinations",
           another = "another destination", whose = "the destination"))

# Part de chaque partenaire pour chaque pays : les premiers partenaires mondiaux, puis "OTH"
fo_shares <- function(file, flow, n_partners) {
  f <- fo_flow[[flow]]
  d <- read_csv(file, na = "", show_col_types = FALSE) %>%
    select(unit = all_of(f$unit), partner = all_of(f$partner), name = all_of(f$name), n) %>%
    group_by(unit) %>%
    mutate(share = 100 * n / sum(n)) %>%
    ungroup()

  # Premiers partenaires au niveau mondial
  top <- d %>%
    group_by(partner) %>%
    summarise(n = sum(n), name = first(name)) %>%
    arrange(desc(n)) %>%
    slice_head(n = n_partners)

  # Un partenaire absent compte pour 0 % ; "OTH" = tous les autres partenaires
  g <- expand_grid(partner = top$partner, unit = unique(d$unit)) %>%
    left_join(select(d, partner, unit, share), by = c("partner", "unit")) %>%
    mutate(share = replace_na(share, 0))

  other <- g %>%
    group_by(unit) %>%
    summarise(share = 100 - sum(share)) %>%
    mutate(partner = "OTH")

  # Un pays n'est jamais son propre partenaire : on l'écarte de sa propre ligne
  g <- bind_rows(g, other) %>%
    filter(partner != unit)

  list(g = g, top = c(top$partner, "OTH"),
       name = c(set_names(top$name, top$partner), OTH = f$other))
}

first_origin <- function(file, flow = "M", n_partners = 3) {
  f <- fo_flow[[flow]]
  s <- fo_shares(file, flow, n_partners)
  name <- s$name
  median_lab <- paste("Median", f$country)

  # Médiane et quartiles entre pays, pour chaque partenaire
  st <- s$g %>%
    group_by(partner) %>%
    summarise(p25 = quantile(share, 0.25), p50 = median(share), p75 = quantile(share, 0.75)) %>%
    mutate(label = factor(name[partner], levels = rev(name)),
           who = if_else(partner == "OTH", f$other_low, as.character(label)),
           tip = paste0(str_to_title(f$role), ": <b>", who, "</b>",
                        "<br>", median_lab, ": <b>", round(p50), "%</b> of its dependent products",
                        "<br>Interquartile range: ", round(p25), "% to ", round(p75), "%"))

  # Symboles : UE, Chine, États-Unis, Japon
  mk <- s$g %>%
    filter(unit %in% names(country_name)) %>%
    mutate(label = factor(name[partner], levels = rev(name)),
           country = factor(country_name[unit], levels = country_name),
           who = if_else(partner == "OTH", f$another, as.character(label)),
           tip = paste0("<b>", country, "</b>: ", round(share), "% of its dependent products",
                        "<br>have ", who, " as ", f$role))

  ggplot(st, aes(y = label)) +
    geom_linerange(aes(xmin = p25, xmax = p75, colour = "Interquartile range"), linewidth = 3.2) +
    geom_point_interactive(aes(x = p50, fill = median_lab, tooltip = tip, data_id = partner),
                           shape = 21, size = 3.4, colour = "white", stroke = 0.6) +
    geom_point_interactive(data = mk, aes(x = share, y = label, shape = country, tooltip = tip,
                                          data_id = paste(unit, partner)),
                           size = 2.2, colour = "#3C3C46", stroke = 0.7) +
    scale_colour_manual(values = c("Interquartile range" = "#E6DCC3"), name = NULL) +
    scale_fill_manual(values = set_names("#7D8291", median_lab), name = NULL) +
    scale_shape_manual(values = c("European Union" = 4, "China" = 3, "United States" = 2, "Japan" = 1),
                       name = NULL) +
    guides(fill = guide_legend(order = 1), colour = guide_legend(order = 2),
           shape = guide_legend(order = 3, nrow = 1)) +
    scale_x_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.03)),
                       labels = function(x) paste0(x, "%")) +
    labs(x = paste0("Share of the ", f$country, "'s dependent products for which ", f$whose,
                    " is the ", f$role), y = NULL) +
    theme_minimal(base_size = 10) +
    theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_blank(),
          axis.text.y = element_text(size = 10, colour = "#3C3C46"),
          legend.position = "bottom", legend.box = "horizontal")
}

# HTML : graphique interactif (chiffres au survol). PDF : figure fixe.
show_first_origin <- function(p) {
  if (!knitr::is_html_output()) return(p)
  girafe(ggobj = p, width_svg = 8, height_svg = 3.6,
         options = list(opts_hover(css = "stroke:#2e2b36;stroke-width:1.5px;"),
                        opts_tooltip(css = "background:#ffffff;color:#2e2b36;padding:6px 9px;border:1px solid #ccc;border-radius:4px;font-size:13px;"),
                        opts_toolbar(saveaspng = FALSE)))
}

# Pour le texte : part moyenne entre pays (ex : partner = "CHN" ou "OTH")
first_origin_mean <- function(file, partner, flow = "M", n_partners = 3) {
  code <- partner
  fo_shares(file, flow, n_partners)$g %>%
    filter(partner == code) %>%
    summarise(m = round(mean(share))) %>%
    pull(m)
}

# Pour la note : nombre de pays et année
first_origin_n <- function(file, flow = "M") {
  read_csv(file, na = "", show_col_types = FALSE) %>%
    pull(fo_flow[[flow]]$unit) %>%
    n_distinct()
}

first_origin_year <- function(file) {
  read_csv(file, na = "", show_col_types = FALSE) %>% pull(year) %>% first()
}
