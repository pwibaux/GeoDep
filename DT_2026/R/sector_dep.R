# Produits dépendants par secteur stratégique (barres) et part des produits du secteur (étiquettes).
# Données : data/sector_dep_M.csv (23_dt_import_dependencies.do). Utilisé par DT_2026.
library(tidyverse)
library(ggiraph)

country_name <- c(EUN = "European Union", CHN = "China", USA = "United States", JPN = "Japan")
sector_name  <- c(crm = "CRM", dual_use = "Dual use", health = "Health", agrifood = "Agrifood", energy = "Energy")
sector_col   <- c("CRM" = "#DCC8A0", "Dual use" = "#B4B4C8", "Health" = "#F0BEA5",
                  "Agrifood" = "#BEC89B", "Energy" = "#B9A596")

sector_dep <- function(file) {
  d <- read_csv(file, show_col_types = FALSE) %>%
    mutate(country = factor(country_name[iso_d], levels = country_name),
           sector  = factor(sector_name[sector], levels = sector_name),
           lab = paste0(format(round(share, 1), nsmall = 1, trim = TRUE), "%"),
           tip = paste0("<b>", country, " – ", sector, "</b>",
                        "<br>Dependent products: <b>", nb_dep, "</b>",
                        "<br>Imported products of the sector: ", nb_prod,
                        "<br>Share dependent: <b>", lab, "</b>"))

  ggplot(d, aes(x = sector, y = nb_dep, fill = sector)) +
    geom_col_interactive(aes(tooltip = tip, data_id = paste(iso_d, sector)), width = 0.75) +
    geom_text(aes(label = lab), vjust = -0.4, size = 2.8, colour = "#3C3C46") +
    facet_wrap(~ country, nrow = 1) +
    scale_fill_manual(values = sector_col, guide = "none") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
    labs(x = NULL, y = "Number of dependent HS6 products") +
    theme_minimal(base_size = 10) +
    theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1, colour = "#3C3C46"),
          strip.text = element_text(size = 10, face = "bold", colour = "#3C3C46"))
}

# HTML : graphique interactif (chiffres au survol). PDF : figure fixe.
show_sector_dep <- function(p) {
  if (!knitr::is_html_output()) return(p)
  girafe(ggobj = p, width_svg = 8, height_svg = 3.4,
         options = list(opts_hover(css = "stroke:#2e2b36;stroke-width:1.5px;"),
                        opts_tooltip(css = "background:#ffffff;color:#2e2b36;padding:6px 9px;border:1px solid #ccc;border-radius:4px;font-size:13px;"),
                        opts_toolbar(saveaspng = FALSE)))
}

sector_dep_year <- function(file) {
  read_csv(file, show_col_types = FALSE) %>% pull(year) %>% first()
}
