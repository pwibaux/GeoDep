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
# (top : partenaires imposés, ex : ceux de tous les produits pour les produits stratégiques)
fo_shares <- function(file, flow, n_partners, top = NULL) {
  f <- fo_flow[[flow]]
  d <- read_csv(file, na = "", show_col_types = FALSE) %>%
    select(unit = all_of(f$unit), partner = all_of(f$partner), name = all_of(f$name), n) %>%
    group_by(unit) %>%
    mutate(share = 100 * n / sum(n)) %>%
    ungroup()

  # Premiers partenaires au niveau mondial
  if (is.null(top)) {
    top <- d %>%
      group_by(partner) %>%
      summarise(n = sum(n), name = first(name)) %>%
      arrange(desc(n)) %>%
      slice_head(n = n_partners)
  }

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

  list(g = g, top = c(top$partner, "OTH"), top_tbl = top,
       name = c(set_names(top$name, top$partner), OTH = f$other))
}

# file_strat : produits stratégiques, en seconde ligne sous chaque partenaire (importations)
first_origin <- function(file, flow = "M", n_partners = 3, file_strat = NULL) {
  f <- fo_flow[[flow]]
  s <- fo_shares(file, flow, n_partners)
  name <- s$name
  median_lab <- paste("Median", f$country)

  g <- mutate(s$g, scope = "All products", what = "dependent products")
  if (!is.null(file_strat)) {
    gs <- fo_shares(file_strat, flow, n_partners, top = s$top_tbl)$g
    g <- bind_rows(g, mutate(gs, scope = "Strategic products", what = "dependent strategic products"))
  }
  two <- n_distinct(g$scope) == 2

  # Ligne de chaque partenaire (le premier en haut) ; produits stratégiques juste en dessous
  g <- g %>%
    mutate(y = match(partner, rev(s$top)) + if_else(scope == "All products", 0.18, -0.18) * two,
           label = name[partner])

  # Médiane et quartiles entre pays, pour chaque partenaire
  st <- g %>%
    group_by(partner, label, scope, what, y) %>%
    summarise(p25 = quantile(share, 0.25), p50 = median(share), p75 = quantile(share, 0.75),
              .groups = "drop") %>%
    mutate(who = if_else(partner == "OTH", f$other_low, label),
           tip = paste0(if (two) paste0(scope, "<br>") else "",
                        str_to_title(f$role), ": <b>", who, "</b>",
                        "<br>", median_lab, ": <b>", round(p50), "%</b> of its ", what,
                        "<br>Interquartile range: ", round(p25), "% to ", round(p75), "%"))

  # Symboles : UE, Chine, États-Unis, Japon
  mk <- g %>%
    filter(unit %in% names(country_name)) %>%
    mutate(country = factor(country_name[unit], levels = country_name),
           who = if_else(partner == "OTH", f$another, label),
           tip = paste0("<b>", country, "</b>: ", round(share), "% of its ", what,
                        "<br>have ", who, " as ", f$role))

  # Légende : une seule ligne (tous les produits) ou une couleur par ligne
  if (two) {
    band <- c("All products" = "#E6DCC3", "Strategic products" = "#F3D5C4")
    dot  <- c("All products" = "#7D8291", "Strategic products" = "#D98E6E")
    leg  <- "Median and interquartile range:"
    st   <- mutate(st, band = scope)
  } else {
    band <- c("Interquartile range" = "#E6DCC3")
    dot  <- set_names("#7D8291", median_lab)
    leg  <- NULL
    st   <- mutate(st, band = "Interquartile range", scope = median_lab)
  }

  ggplot(st, aes(y = y)) +
    geom_linerange(aes(xmin = p25, xmax = p75, colour = band), linewidth = if (two) 2.6 else 3.2) +
    geom_point_interactive(aes(x = p50, fill = scope, tooltip = tip, data_id = paste(partner, scope)),
                           shape = 21, size = if (two) 3 else 3.4, colour = "white", stroke = 0.6) +
    geom_point_interactive(data = mk, aes(x = share, y = y, shape = country, tooltip = tip,
                                          data_id = paste(unit, partner, scope)),
                           size = 2.2, colour = "#3C3C46", stroke = 0.7) +
    scale_y_continuous(breaks = seq_along(s$top), labels = name[rev(s$top)],
                       expand = expansion(add = 0.5)) +
    scale_colour_manual(values = band, name = leg) +
    scale_fill_manual(values = dot, name = leg) +
    scale_shape_manual(values = c("European Union" = 4, "China" = 3, "United States" = 2, "Japan" = 1),
                       name = NULL) +
    # Deux lignes : médiane et barre fusionnées en une seule légende (même titre, même ordre)
    guides(fill = guide_legend(order = 1), colour = guide_legend(order = if (two) 1 else 2),
           shape = guide_legend(order = 3, nrow = 1)) +
    scale_x_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.03)),
                       labels = function(x) paste0(x, "%")) +
    labs(x = paste0("Share of the ", f$country, "'s dependent products for which ", f$whose,
                    " is the ", f$role), y = NULL) +
    theme_minimal(base_size = 10) +
    theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_blank(),
          axis.text.y = element_text(size = 10, colour = "#3C3C46"),
          legend.position = "bottom", legend.box = if (two) "vertical" else "horizontal")
}

# HTML : graphique interactif (chiffres au survol). PDF : figure fixe.
show_first_origin <- function(p, height = 3.6) {
  if (!knitr::is_html_output()) return(p)
  girafe(ggobj = p, width_svg = 8, height_svg = height,
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
