## =============================================================================
## GEODEP - SHINY APP
## =============================================================================

library(dplyr)
library(tidyr)
library(ggplot2)
library(shiny)
library(leaflet)
library(htmltools)
library(DT)
library(sf)
library(readr)
library(zip)
library(ggtext)
library(treemapify)
library(shinycssloaders)
library(bslib)
library(shinyWidgets)
## (here::i_am() removed: .Rproj files are not uploaded when publishing,
##  so it stopped the app on the server; all paths below are relative.)

## -----------------------------------------------------------------------
## 0. Load precomputed inputs
## -----------------------------------------------------------------------

geodep_inputs <- readRDS("1_Data/geodep_shiny_inputs.rds")

eu_countries               <- geodep_inputs$eu_countries
world_polygons                  <- geodep_inputs$world_polygons
iso_name_lookup                   <- geodep_inputs$iso_name_lookup
country_choices                     <- geodep_inputs$country_choices
country_choices_ui                    <- geodep_inputs$country_choices_ui
dep_import_base                         <- geodep_inputs$dep_import_base
dep_export_base                           <- geodep_inputs$dep_export_base
traded_by_country                           <- geodep_inputs$traded_by_country
traded_by_country_export                      <- geodep_inputs$traded_by_country_export
countries_with_data_import <- geodep_inputs$countries_with_data_import
countries_with_data_export <- geodep_inputs$countries_with_data_export
imports_final <- geodep_inputs$imports_final
exports_final <- geodep_inputs$exports_final

rm(geodep_inputs); gc()

## Sector names in sentence case (also used in the charts and the table)
sector_names <- list(sect_crm = "Critical raw materials", sect_dual_use = "Dual use",
                     sect_health = "Health", sect_agrifood = "Agrifood",
                     sect_energy = "Energy", sect_other = "Other")

## sector as used in a sentence ("Out of 52 health products...")
sector_adjectives <- c(all = NA, sect_strategic = "strategic", sect_crm = "critical raw material",
                       sect_dual_use = "dual-use", sect_health = "health",
                       sect_agrifood = "agrifood", sect_energy = "energy")

sector_choices_ui <- list(
  "All sectors" = "all",
  "Strategic sectors" = c(
    "All strategic sectors"        = "sect_strategic",
    "\u2003Critical raw materials" = "sect_crm",
    "\u2003Dual use"               = "sect_dual_use",
    "\u2003Health"                 = "sect_health",
    "\u2003Agrifood"               = "sect_agrifood",
    "\u2003Energy"                 = "sect_energy"
  )
)

## -----------------------------------------------------------------------
## Map settings
## -----------------------------------------------------------------------

## Projection: "equal_earth" (areas shown at their true relative size; the
## projection backed by the UN in 2026) or "mercator" (previous web-map look,
## with the grey basemap tiles).
MAP_PROJECTION <- "equal_earth"

## Colour classes (world view, no country selected): 7 classes with round limits,
## chosen from the data so that the colours stay contrasted:
##  - even steps (0.2, 0.5, 1, 2, 2.5...) by default, e.g. 6, 7, 8 ... 11%;
##  - a 1-2-5 or 1-3-10 progression (0.1, 0.3, 1, 3, 10%) only when it spreads
##    the countries clearly better (very skewed data, e.g. export values);
##  - the first limit stays at or below the 10th percentile, so the least
##    dependent countries (e.g. China for imports) get the lightest colour.
## The last label is the maximum, rounded up.
map_breaks <- function(x, n_classes = 7) {
  pos <- x[!is.na(x) & x > 0]
  if (length(pos) < 2) return(c(0, max(1, ceiling(max(c(pos, 0))))))
  top  <- max(pos)
  n_in <- n_classes - 1
  q10  <- quantile(pos, 0.10, names = FALSE)
  q75  <- quantile(pos, 0.75, names = FALSE)
  ## share of countries in the fullest class (lower = more contrast)
  crowding <- function(w) max(tabulate(findInterval(pos, w) + 1, n_classes)) / length(pos)
  ## keep the lowest countries (e.g. China) in the lightest class and the top quarter spread
  ok <- function(w) w[1] <= q10 && w[n_in] >= q75 && w[n_in] < top
  best <- NULL; best_score <- Inf; best_type <- ""
  try_w <- function(w, type) {
    if (!ok(w)) return()
    sc <- crowding(w)
    if (sc < best_score - 1e-9) { best <<- w; best_score <<- sc; best_type <<- type }
  }
  ## 1) even steps: 0.25, 0.5, 1, 2.5, 5...
  for (step in c(0.01, 0.02, 0.025, 0.05, 0.1, 0.2, 0.25, 0.5, 1, 2, 2.5, 5, 10, 20, 25)) {
    for (k in 1:ceiling(q10 / step)) try_w(step * (k + 0:(n_in - 1)), "linear")
  }
  lin_score <- best_score; lin <- best
  ## 2) log-like progressions (1-2-5 or 1-3-10), kept only if clearly better
  best <- NULL; best_score <- Inf
  for (m in list(c(1, 2, 5), c(1, 3))) {
    cand <- sort(as.vector(outer(m, 10^(-3:2))))
    for (i in seq_len(length(cand) - n_in + 1)) try_w(cand[i:(i + n_in - 1)], "log")
  }
  inner <- if (!is.null(best) && (is.null(lin) || best_score < 0.75 * lin_score)) best else lin
  if (is.null(inner)) inner <- pretty(c(0, top), n = n_in)[-1]
  top_lab <- if (top >= 10) ceiling(top) else ceiling(top * 10) / 10
  unique(c(0, inner[inner < top], top_lab))
}

## Partner map (a country is selected): shares are very skewed (one partner, often
## China, can lead 60% of the dependencies while most lead < 1%), so the limits
## are steps (0.1, 0.25, 0.5, 1, 2, 5, 10, 25, 50%), up to the actual maximum.
## A dominant partner then stands alone in the darkest class.
MAP_PARTNER_STEPS <- c(0.1, 0.25, 0.5, 1, 2, 5, 10, 25, 50)

partner_breaks <- function(x, n_classes = 7) {
  pos <- x[!is.na(x) & x > 0]
  if (length(pos) == 0) return(c(0, 1))
  top   <- ceiling(max(pos))
  below <- MAP_PARTNER_STEPS[MAP_PARTNER_STEPS < max(pos)]
  below <- utils::tail(below, n_classes - 1)
  unique(c(0, below, top))
}

## Petrol blue, light -> dark: one colour per class above (7 classes)
MAP_COLOURS <- c("#DCEBEE", "#AED1D7", "#83B7C0", "#5B9CA8", "#3C8090", "#266476", "#16465A")
## Lighter version used for export dependencies
MAP_COLOURS_EXPORT <- c("#E4EFF1", "#CAE1E5", "#B2D3D9", "#90BFC7", "#71ABB5", "#5395A2", "#387C8D")

## Equal Earth only: TRUE draws the oval outline of the globe; FALSE fills the
## whole rectangle with the sea colour and zooms to the land.
MAP_GLOBE_OUTLINE <- TRUE

COL_ZERO   <- "#C9C5BD"  # data, but not dependent on any product (0%)
COL_NODATA <- "#EEECE8"  # no data
COL_OCEAN  <- "#F7F8F8"  # sea (Equal Earth only)
COL_GLOBE  <- "#D9D7D2"  # outline of the globe (Equal Earth only)

## world_polygons arrive ready to draw: Antarctica removed and countries crossing
## the 180th meridian split there (see fix_antimeridian() in preparation.R)

## Equal Earth (EPSG:8857), drawn without basemap tiles
crs_equal_earth <- leafletCRS(
  crsClass    = "L.Proj.CRS",
  code        = "EPSG:8857",
  proj4def    = "+proj=eqearth +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs",
  resolutions = 2^(17:10)            # zoom 0 to 7
)

## Extent of the land in Equal Earth metres (used when MAP_GLOBE_OUTLINE is FALSE)
land_extent <- if (MAP_PROJECTION == "equal_earth") {
  bb <- st_bbox(st_transform(world_polygons, "+proj=eqearth +lon_0=0 +datum=WGS84 +units=m +no_defs"))
  as.numeric(bb[c("xmin", "xmax", "ymin", "ymax")])
} else NULL

## Outline of the globe from 60 degrees S to the North Pole (lng/lat vertices)
globe_outline <- local({
  lats <- seq(-60, 90, by = 1)
  lons <- seq(-180, 180, by = 1)
  list(
    lng = c(rep(-180, length(lats)), lons, rep(180, length(lats)), rev(lons)),
    lat = c(lats, rep(90, length(lons)), rev(lats), rep(-60, length(lons)))
  )
})

## Equal Earth is missing from the proj4 library bundled with the leaflet R package:
## this browser script adds it (same formulas as EPSG:8857, spherical version).
equal_earth_js <- "
(function () {
  // Equal Earth projection (Savric, Patterson & Jenny, 2018) for proj4js,
  // which the leaflet R package bundles without it.
  var A1 = 1.340264, A2 = -0.081106, A3 = 0.000893, A4 = 0.003796, M = Math.sqrt(3) / 2, S3 = Math.sqrt(3);
  function adjLon(x) { return Math.abs(x) <= Math.PI + 1e-12 ? x : x - (x < 0 ? -1 : 1) * 2 * Math.PI; }
  function register(p4) {
    if (!p4 || !p4.Proj || !p4.Proj.projections || p4.__eqearth) return;
    p4.Proj.projections.add({
      names: ['eqearth', 'Equal_Earth', 'Equal Earth'],
      init: function () {
        this.x0 = this.x0 || 0; this.y0 = this.y0 || 0; this.long0 = this.long0 || 0;
        this.R = 6371008.7714;
      },
      forward: function (p) {
        var lam = adjLon(p.x - this.long0), th = Math.asin(M * Math.sin(p.y)), t2 = th * th, t6 = t2 * t2 * t2;
        var d = A1 + 3 * A2 * t2 + t6 * (7 * A3 + 9 * A4 * t2);
        p.x = this.R * 2 * S3 * lam * Math.cos(th) / (3 * d) + this.x0;
        p.y = this.R * th * (A1 + A2 * t2 + t6 * (A3 + A4 * t2)) + this.y0;
        return p;
      },
      inverse: function (p) {
        // Points outside the globe (e.g. the corners of the map view) are clamped
        // to its edge, so Leaflet never receives an invalid latitude/longitude.
        var thMax = Math.asin(M), t2m = thMax * thMax, t6m = t2m * t2m * t2m;
        var yMax = thMax * (A1 + A2 * t2m + t6m * (A3 + A4 * t2m));
        var x = (p.x - this.x0) / this.R, y = (p.y - this.y0) / this.R, t2, t6, d;
        y = Math.max(-yMax, Math.min(yMax, y));
        var th = y;
        for (var i = 0; i < 12; i++) {
          t2 = th * th; t6 = t2 * t2 * t2;
          d = A1 + 3 * A2 * t2 + t6 * (7 * A3 + 9 * A4 * t2);
          var dth = (th * (A1 + A2 * t2 + t6 * (A3 + A4 * t2)) - y) / d;
          th -= dth;
          if (Math.abs(dth) < 1e-12) break;
        }
        th = Math.max(-thMax, Math.min(thMax, th));
        t2 = th * th; t6 = t2 * t2 * t2;
        d = A1 + 3 * A2 * t2 + t6 * (7 * A3 + 9 * A4 * t2);
        var lam = 3 * x * d / (2 * S3 * Math.cos(th));
        lam = Math.max(-Math.PI, Math.min(Math.PI, lam));
        p.x = lam + this.long0;
        p.y = Math.asin(Math.max(-1, Math.min(1, Math.sin(th) / M)));
        return p;
      }
    });
    p4.__eqearth = true;
  }
  // proj4 may already be loaded, or be loaded later by the leaflet widget:
  // register it in both cases, as soon as it exists.
  if (window.proj4) {
    register(window.proj4);
  } else {
    var _p4;
    Object.defineProperty(window, 'proj4', {
      configurable: true,
      get: function () { return _p4; },
      set: function (v) { _p4 = v; register(v); }
    });
  }
})();
"

## Point where the name label of a selected country is placed: inside its
## largest piece (e.g. mainland US rather than Alaska or the Aleutians);
## EU: central Europe. Areas are computed directly from the coordinates, so
## imperfect outlines or islands across the 180th meridian cannot move the label.
selection_label_point <- function(map_sf, iso) {
  if (iso == "EUN") return(c(10, 50))
  rows <- map_sf[map_sf$iso_plot == iso, ]
  if (nrow(rows) == 0) return(NULL)
  old_s2 <- suppressMessages(sf::sf_use_s2(FALSE))
  on.exit(suppressMessages(sf::sf_use_s2(old_s2)))
  g <- st_geometry(rows)
  g <- tryCatch(suppressWarnings(st_collection_extract(g, "POLYGON")), error = function(e) g)   # mixed shapes -> polygons
  g <- tryCatch(suppressWarnings(st_cast(g, "MULTIPOLYGON")), error = function(e) NULL)
  if (is.null(g)) return(NULL)
  m     <- st_coordinates(g)
  outer <- m[m[, "L1"] == 1, , drop = FALSE]                 # outer rings only
  key   <- paste(outer[, "L3"], outer[, "L2"])
  best  <- NULL; best_area <- -1
  for (k in unique(key)) {
    r <- outer[key == k, c("X", "Y"), drop = FALSE]
    n <- nrow(r); if (n < 4) next
    x <- r[, 1]; y <- r[, 2]
    cr <- x[-n] * y[-1] - x[-1] * y[-n]
    a  <- sum(cr) / 2
    if (abs(a) < 1e-10) next
    cx <- sum((x[-n] + x[-1]) * cr) / (6 * a)
    cy <- sum((y[-n] + y[-1]) * cr) / (6 * a)
    area <- abs(a) * cos(cy * pi / 180)
    if (area > best_area) { best_area <- area; best <- list(ring = r, centre = c(cx, cy)) }
  }
  if (is.null(best)) return(NULL)
  tryCatch({                                                  # a point surely inside the piece
    p <- suppressWarnings(st_point_on_surface(st_polygon(list(best$ring))))
    as.numeric(st_coordinates(p))[1:2]
  }, error = function(e) best$centre)
}

## Hidden copy of the EU outline, shown in the browser when hovering an EU member
add_eu_hover_outline <- function(map) {
  if (is.null(eu_outline)) return(map)
  addPolygons(map, data = eu_outline, layerId = "eu_hover_outline", group = "eu_hover",
              fill = FALSE, color = IFE$ink, weight = 2, opacity = 0,
              options = pathOptions(pane = "selection", interactive = FALSE))
}

## Outer outline of the EU (drawn when the EU is selected)
eu_outline <- (function() {
  old_s2 <- suppressMessages(sf::sf_use_s2(FALSE))
  on.exit(suppressMessages(sf::sf_use_s2(old_s2)))
  tryCatch({
    eu_geom <- st_geometry(world_polygons[world_polygons$iso_a3 %in% eu_countries, ])
    suppressWarnings(suppressMessages(st_union(st_make_valid(eu_geom))))
  }, error = function(e) NULL)
})()

## -----------------------------------------------------------------------
## 1. Small helpers
## -----------------------------------------------------------------------
to_eun <- function(iso3) {
  if_else(iso3 %in% eu_countries, "EUN", iso3)
}

iso_display_name <- function(iso3) {
  if_else(iso3 == "EUN", "European Union", iso3)
}

iso_name <- function(iso3) {
  matched <- iso_name_lookup$name[match(iso3, iso_name_lookup$iso_a3)]
  as.character(ifelse(is.na(matched), iso3, matched))
}

## Wraps a chart title to the width of the plot on screen (width_px, from
## session$clientData); without a width (PNG downloads) it uses default_chars
wrap_title <- function(x, width_px, font_pt, sep = "\n", default_chars = 64, char_em = 0.6) {
  n <- if (is.null(width_px) || is.na(width_px) || width_px <= 0) default_chars else
    max(20, floor(width_px / (font_pt * char_em)))
  paste(strwrap(x, width = n), collapse = sep)
}

## Country name for use inside a sentence: adds "the" where English needs it
## ("for which the United States of America is import-dependent")
iso_name_in_sentence <- function(iso3) {
  nm <- iso_name(iso3)
  needs_the <- grepl("^(United |European Union|Netherlands|Philippines|Bahamas|Gambia|Maldives|Seychelles|Comoros|Marshall Islands|Solomon Islands|Cayman Islands|Falkland|Faroe|Cook Islands|Virgin Islands|Turks and Caicos|Czech Republic|Dominican Republic|Central African Republic|Democratic Republic|Republic of)", nm)
  ifelse(needs_the, paste("the", nm), nm)
}

## -----------------------------------------------------------------------
## Helper: build README text for the zip download
## -----------------------------------------------------------------------
format_kb <- function(bytes) {
  format(round(bytes / 1000), big.mark = " ", scientific = FALSE)
}

generate_readme <- function(direction, selection, sector_filter, map_metric,
                            zip_filename, size_kb_unzipped, size_kb_zipped = NULL,
                            is_full = FALSE) {
  
  is_import <- direction == "import"
  
  unzipped_line <- paste0(size_kb_unzipped, " Kilobytes")
  zipped_line   <- if (is.null(size_kb_zipped)) "" else paste0(size_kb_zipped, " Kilobytes")
  
  if (is_full) {
    selection_line <- paste0(
      "Selection used for this extract : FULL DATASET \u2014 no country or sector filter applied",
      " | Direction = ", if (is_import) "Imports" else "Exports",
      " | Extraction date = ", Sys.Date()
    )
  } else {
    selection_desc <- if (length(selection) == 2) {
      paste0("Importer = ", iso_display_name(selection[1]), " (", selection[1], "), ",
             "Exporter = ", iso_display_name(selection[2]), " (", selection[2], ")")
    } else if (length(selection) == 1) {
      paste0(if (is_import) "Importer" else "Exporter", " = ",
             iso_display_name(selection[1]), " (", selection[1], ")")
    } else {
      "No country selected (full table for current filters)"
    }
    
    sector_label <- if (sector_filter == "all") "All sectors" else sector_filter
    metric_label <- if (map_metric == "count") "Share of number of products" else "Share of trade value"
    
    selection_line <- paste0(
      "Selection used for this extract : Direction = ", if (is_import) "Imports" else "Exports",
      " | ", selection_desc,
      " | Sector filter = ", sector_label,
      " | Map metric = ", metric_label,
      " | Extraction date = ", Sys.Date()
    )
  }
  
  if (is_import) {
    header <- paste0(
      "Name of the dataset : ", zip_filename, "\n",
      "Format : csv\n",
      "Delimiter : ,\n\n",
      "Release Date : \n\n", #to be modified
      "Weblink : https://www.cepii.fr/CEPII/fr/bdd_modele/bdd_modele_item.asp?id=41\n\n", #to be modified
      "DOI : \n\n", #to be modified
      "Size (unzipped) : ", unzipped_line, "\n\n",
      "Size (zipped) : ", zipped_line, "\n\n",
      "Software : this dataset was created using Stata 16\n\n",
      "Contents: GeoDep provides an assessment of trade dependencies for the year 2024 at the HS6 product level (revision 2022). GeoDep_M assesses import vulnerabilities by measuring concentration, substitutability, and strategic sector categorization (e.g., health, energy, agrifood, dual-use).\n\n",
      "List of Variables (GeoDep_M - Import Dependencies) :\n",
      "iso_d -- Importer (ISO 3-digit country code)\n",
      "hs6 -- Product category (HS6 product rev. 2022)\n",
      "year -- Year 2024\n",
      "import_dpt -- Total import value (in thousands current USD) of HS6 product p, to destination d, in year t\n",
      "c1_M -- Level of concentration of imports\n",
      "c2_M -- Level of concentration of world exports\n",
      "c3_M -- Substitutability of exports by domestic supply\n",
      "c4_M -- Criteria 4: c1 > 0.4 & c2 > 0.4 & c3 > 1 = 2/3 previous years\n",
      "first_odpt -- ISO code of the leading exporter in total imports of the HS6 product p, to destination d, in year t\n",
      "share_odpt -- Share of the leading exporter in total imports of the HS6 product p, to destination d, in year t\n",
      "dependent -- =1 if [c1 > 0.4 & c2 > 0.4 & c3 > 1 & c4 = 1], 0 otherwise\n",
      "sect_crm -- =1 if the HS6 product belongs to CRM UNCTAD list, 0 otherwise\n",
      "sect_dual_use -- =1 if the HS6 product belongs to Dual use EU list, 0 otherwise\n",
      "sect_health -- =1 if the HS6 product belongs to Health nomenclature list, 0 otherwise\n",
      "sect_energy -- =1 if the HS6 product belongs to Energy ECT list, 0 otherwise\n",
      "sect_agrifood -- =1 if the HS6 product belongs to Agrifood WB or Fertilisants FAO list, 0 otherwise\n",
      "sect_other -- =1 if the HS6 product is not strategic\n\n",
      "Additional useful information : Not applicable\n\n",
      "Example of 1 line : Not applicable\n\n",
      "Licence : Creative Commons BY NC SA\n\n",
      "Reference (Please cite when using this dataset) : \n\n", #to be modified
      selection_line, "\n"
    )
  } else {
    header <- paste0(
      "Name of the dataset : ", zip_filename, "\n",
      "Format : csv\n",
      "Delimiter : ,\n\n",
      "Release Date : \n\n", #to be modified
      "Weblink : https://www.cepii.fr/CEPII/fr/bdd_modele/bdd_modele_item.asp?id=41\n\n", #to be modified
      "DOI : \n\n", #to be modified
      "Size (unzipped) : ", unzipped_line, "\n\n",
      "Size (zipped) : ", zipped_line, "\n\n",
      "Software : this dataset was created using Stata 16\n\n",
      "Contents: GeoDep provides an assessment of trade dependencies for the year 2024 at the HS6 product level (revision 2022). GeoDep_X assesses export dependencies by measuring market concentration and substitutability by domestic demand.\n\n",
      "List of Variables (GeoDep_X - Export Dependencies) :\n",
      "iso_o -- Exporter (ISO 3-digit country code)\n",
      "hs6 -- Product category (HS6 product rev. 2022)\n",
      "year -- Year 2024\n",
      "export_opt -- Total export value (in thousands current USD) of HS6 product p, from origin o, in year t\n",
      "c1_X -- Level of concentration of exports\n",
      "c2_X -- Level of concentration of world imports\n",
      "c3_X -- Substitutability of exports by domestic demand\n",
      "c4_X -- Criteria 4: c1_X > 0.4 & c2_X > 0.4 & c3_X > 1 = 2/3 previous years\n",
      "first_dpto -- ISO code of the first destination in total exports of the HS6 product p, from origin o, in year t\n",
      "share_dpto -- Share of the first destination in total exports of the HS6 product p, from origin o, in year t\n",
      "dependent -- =1 if [c1_X > 0.4 & c2_X > 0.4 & c3_X > 1 & c4_X = 1], 0 otherwise\n\n",
      "Additional useful information : Not applicable\n\n",
      "Example of 1 line : Not applicable\n\n",
      "Licence : Creative Commons BY NC SA\n\n",
      "Reference (Please cite when using this dataset) : \n\n",  #to be modified
      selection_line, "\n"
    )
  }
  
  header
}

reference_files <- c("product_codes_HS22_V202601.csv", "country_codes_V202601.csv")

copy_reference_files <- function(tmp_dir, data_dir = "1_Data") {
  src_paths <- file.path(data_dir, reference_files)
  present   <- file.exists(src_paths)
  
  if (any(!present)) {
    warning("Missing reference file(s): ", paste(reference_files[!present], collapse = ", "))
  }
  
  Map(function(src, ok) {
    if (ok) file.copy(src, file.path(tmp_dir, basename(src)), overwrite = TRUE)
  }, src_paths, present)
  
  reference_files[present]
}

## -----------------------------------------------------------------------
## 2. UI
## -----------------------------------------------------------------------

## IFE logos (white, transparent), embedded here so they always display,
## whatever folder the app is started from (no need for files in www/)
LOGO_IFE_FULL <- paste0("data:image/png;base64,", paste0(c(
  "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",
  "k2+IJ8eUAAAAAElFTkSuQmCC"
), collapse = ""))

LOGO_IFE_MARK <- paste0("data:image/png;base64,", paste0(c(
  "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"
), collapse = ""))

## IFE-inspired palette (approximation: anthracite + warm paper + one accent).
## Change these values in one place if IFE's exact brand colours become available.
IFE <- list(
  ink       = "#3A4048",  # softened anthracite: top bar, titles, main buttons, 1st selected country
  chart     = "#6B7480",  # lighter slate for large filled areas in the charts
  ink_soft  = "#5A6069",  # secondary text
  paper     = "#F6F5F2",  # page background
  line      = "#E3E1DC",  # borders
  input     = "#CFCCC6",  # input borders
  accent    = "#7D0000",  # IFE red (left end of the IFE banner): main download button, links
  accent_dk = "#5C0000",
  brand_red    = "#7D0000",  # IFE banner gradient, left
  brand_petrol = "#21606E"   # IFE banner gradient, right
)

ife_theme <- bs_theme(
  version      = 5,
  bg           = "#FFFFFF",
  fg           = IFE$ink,
  primary      = IFE$ink,
  secondary    = IFE$ink_soft,
  danger       = IFE$accent,
  base_font    = font_google("Arimo", wght = c(400, 500, 600, 700)),
  heading_font = font_google("Merriweather", wght = c(400, 700)),
  font_scale   = 0.95,
  "border-radius"      = "6px",
  "border-color"       = IFE$line,
  "body-bg"            = IFE$paper,
  "link-color"         = IFE$accent,
  "card-border-color"  = IFE$line,
  "card-cap-bg"        = "#FFFFFF",
  "input-border-color" = IFE$input
)

## CSS variables come from the IFE list above; the rest of the stylesheet is plain CSS.
## (sprintf() is not used: R limits its format string to 8192 characters.)
ife_css_vars <- paste0(
  ":root {",
  " --ife-ink: ",       IFE$ink,       ";",
  " --ife-ink-soft: ",  IFE$ink_soft,  ";",
  " --ife-paper: ",     IFE$paper,     ";",
  " --ife-line: ",      IFE$line,      ";",
  " --ife-input: ",     IFE$input,     ";",
  " --ife-accent: ",    IFE$accent,    ";",
  " --ife-accent-dk: ", IFE$accent_dk, ";",
  " --ife-brand-red: ", IFE$brand_red, ";",
  " --ife-brand-petrol: ", IFE$brand_petrol, ";",
  " }"
)

ife_css <- HTML(paste0(ife_css_vars, "

  /* ---------- Top bar: IFE banner gradient (red to petrol) ---------- */
  :root { --bslib-navbar-default-bg: var(--ife-brand-red); --bslib-navbar-inverse-bg: var(--ife-brand-red); }
  .navbar, .navbar.navbar-default, .navbar.navbar-inverse, nav.navbar.navbar-static-top {
            background-color: var(--ife-brand-red) !important;
            background-image: linear-gradient(90deg, var(--ife-brand-red) 0%, var(--ife-brand-petrol) 100%) !important;
            border-bottom: none; padding-top: 14px; padding-bottom: 14px; }
  /* Same width as the page content, so the links line up with the right edge of the map */
  .navbar > .container-fluid { max-width: 1400px; margin: 0 auto; padding-left: 8px; padding-right: 8px; align-items: center; }
  .navbar .navbar-brand { display: flex; align-items: center; padding-top: 0; padding-bottom: 0; margin-top: 0; margin-bottom: 0; }
  .navbar-collapse { flex-grow: 1; justify-content: flex-end !important; }
  .navbar-nav { flex-grow: 1; align-items: center; gap: 4px; }
  .navbar-nav .bslib-nav-spacer { margin-left: auto !important; }
  .navbar .navbar-brand { color: #fff !important; }
  .navbar .nav-link { color: rgba(255,255,255,.82) !important; font-weight: 600; font-size: .9rem;
                      padding: 6px 12px !important; border-radius: 4px; }
  .navbar .nav-link:hover { color: #fff !important; background: rgba(255,255,255,.08); }
  .navbar .nav-link.active { color: #fff !important; box-shadow: inset 0 -2px 0 #fff; border-radius: 0; }
  /* thin divider between the two pages and the external links */
  .navbar .nav-link.nav-ext-first { margin-left: 10px; position: relative; }
  .navbar .nav-link.nav-ext-first::before { content: ''; position: absolute; left: -8px; top: 20%; height: 60%; border-left: 1px solid rgba(255,255,255,.3); }
  .navbar-toggler, .navbar .navbar-toggle { border-color: rgba(255,255,255,.45) !important; color: #fff !important; }
  /* older bslib versions draw the menu button with 3 bars: replace them with the white icon */
  .navbar .navbar-toggle .icon-bar { display: none !important; }
  .navbar .navbar-toggle { width: 44px; height: 38px; border: 1px solid rgba(255,255,255,.45); border-radius: 6px;
    background: transparent url('data:image/svg+xml,%3csvg xmlns=%27http://www.w3.org/2000/svg%27 viewBox=%270 0 30 30%27%3e%3cpath stroke=%27rgba(255,255,255,0.92)%27 stroke-linecap=%27round%27 stroke-width=%272%27 d=%27M4 7h22M4 15h22M4 23h22%27/%3e%3c/svg%3e') center / 24px no-repeat !important; }
  .navbar-toggler-icon { filter: none;
    background-image: url('data:image/svg+xml,%3csvg xmlns=%27http://www.w3.org/2000/svg%27 viewBox=%270 0 30 30%27%3e%3cpath stroke=%27rgba(255,255,255,0.92)%27 stroke-linecap=%27round%27 stroke-width=%272%27 d=%27M4 7h22M4 15h22M4 23h22%27/%3e%3c/svg%3e') !important; }
  .brand-wrap { display: flex; align-items: center; gap: 14px; }
  .brand-wrap img { height: 46px; width: auto; display: block; }
  .brand-wrap img.brand-mark { display: none; height: 38px; }
  .brand-sep  { width: 1px; height: 34px; background: rgba(255,255,255,.4); }
  @media (max-width: 575px) {                      /* phones: the ife mark only */
    .brand-wrap img.brand-full { display: none; }
    .brand-wrap img.brand-mark { display: block; }
  }
  .brand-name { font-family: 'Merriweather', Georgia, serif; font-weight: 700; font-size: 1.15rem; color: #fff; letter-spacing: .2px; }

  /* ---------- Title section ---------- */
  .hero { max-width: 1400px; margin: 0 auto; padding: 34px 8px 22px; }
  .hero-eyebrow { text-transform: uppercase; letter-spacing: .12em; font-size: .72rem;
                  font-weight: 600; color: var(--ife-accent); margin-bottom: 8px; }
  .hero h1 { font-size: 2.3rem; font-weight: 700; margin: 0 0 10px; color: var(--ife-ink); }
  .hero-lead { font-size: 1.05rem; color: var(--ife-ink-soft); max-width: none; line-height: 1.6; margin-bottom: 10px; }
  .hero-meta { font-size: .78rem; color: var(--ife-ink-soft); }
  .hero-cite { margin-top: 4px; }
  .hero-meta span + span::before { content: '\\00b7'; margin: 0 8px; }

  /* ---------- Layout ---------- */
  .atlas-wrap { max-width: 1400px; margin: 0 auto; padding: 0 8px 40px; }
  .bslib-sidebar-layout { border: 1px solid var(--ife-line); border-radius: 8px; background: #fff; }
  .bslib-sidebar-layout > .sidebar { background: #fff; border-right: 1px solid var(--ife-line); }
  .bslib-sidebar-layout > .main { background: var(--ife-paper); }

  /* View heading on the same row as the collapse arrow (bslib reserves a row above the
     content for the arrow; we remove it and keep the title clear of the arrow) */
  .bslib-sidebar-layout > .sidebar > .sidebar-content { padding-top: 14px !important; }
  .bslib-sidebar-layout > .sidebar .sidebar-title { padding-right: 40px; }

  .bslib-sidebar-layout > .sidebar .side-heading,
  .bslib-sidebar-layout > .sidebar .sidebar-title {
    text-transform: uppercase; letter-spacing: .1em; font-size: .68rem; font-family: 'Arimo', Arial, sans-serif;
    font-weight: 600; line-height: 1.2; color: var(--ife-ink-soft);
    margin: 4px 0 2px; padding-bottom: 6px; border-bottom: 1px solid var(--ife-line);
  }
  .bslib-sidebar-layout > .sidebar .side-heading { border-bottom: none; padding-bottom: 0; }
  /* Countries: same level as Sector; its fields are sub-fields */
  .side-group-label { font-weight: 600; font-size: .85rem; color: var(--ife-ink); margin-bottom: 8px; }
  .side-subgroup { display: flex; flex-direction: column; gap: 14px; }
  .side-subgroup .form-group { margin-bottom: 0; }
  .side-subgroup .control-label { font-weight: 500; font-size: .8rem; color: var(--ife-ink-soft); }
  .control-label, label { font-weight: 600; font-size: .85rem; color: var(--ife-ink); }

  /* Coloured dot next to each country field = colour of its outline on the map */
  #importer_select-label::before, #exporter_select-label::before {
    content: ''; display: inline-block; width: 9px; height: 9px; border-radius: 50%;
    margin-right: 6px; vertical-align: 1px; background: var(--ife-ink);
  }
  /* markers match the map: 1st country filled, 2nd country outlined */
  #importer_select-label::before, #exporter_select-label::before { border-radius: 2px; }
  #exporter_select-label::before { background: transparent; border: 2px solid var(--ife-ink);
                                   width: 10px; height: 10px; box-sizing: border-box; }

  /* name labels of the selected countries on the map */
  .leaflet-tooltip.sel-label { background: #fff; color: var(--ife-ink); border: 1px solid var(--ife-line);
    border-radius: 4px; padding: 2px 7px; font-size: 11.5px; box-shadow: 0 1px 3px rgba(0,0,0,.15); }
  .leaflet-tooltip.sel-label .sel-role { color: var(--ife-ink-soft); }

  /* Segmented toggles */
  .btn-group .btn-segmented { background: #fff; color: var(--ife-ink); border: 1px solid var(--ife-input); font-weight: 500; font-size: .85rem; }
  .btn-group .btn-segmented.active, .btn-group .btn-segmented:active,
  .btn-group .btn-check:checked + .btn-segmented, .btn-group input:checked + .btn-segmented {
    background: var(--ife-ink) !important; color: #fff !important; border-color: var(--ife-ink) !important; }

  /* Buttons */
  .btn { font-weight: 600; font-size: .85rem; border-radius: 6px; }
  .btn-primary { background: var(--ife-ink); border-color: var(--ife-ink); }
  .btn-primary:hover { background: #000; border-color: #000; }
  .btn-accent { background: var(--ife-accent); border-color: var(--ife-accent); color: #fff; }
  .btn-accent:hover, .btn-accent:focus { background: var(--ife-accent-dk); border-color: var(--ife-accent-dk); color: #fff; }
  .btn-outline-secondary, .btn-outline-ife { color: var(--ife-ink); border: 1px solid var(--ife-input); background: #fff; }
  .btn-outline-secondary:hover, .btn-outline-ife:hover,
  .btn-outline-secondary:focus, .btn-outline-ife:focus { background: var(--ife-paper); color: var(--ife-ink); border-color: var(--ife-ink-soft); }
  /* Reset selection: solid anthracite so it stands out in the white sidebar */
  .btn-reset { background: var(--ife-ink); color: #fff; border: 1px solid var(--ife-ink); font-size: .9rem; padding: 7px 12px; }
  .btn-reset:hover, .btn-reset:focus { background: #000; border-color: #000; color: #fff; }
  .btn-link-ife { color: var(--ife-accent); padding-left: 0; padding-right: 0; text-decoration: none; }
  .btn-link-ife:hover { color: var(--ife-accent-dk); text-decoration: underline; }

  .swap-wrap { display: flex; justify-content: center; margin: -8px 0 -6px; }
  #swap_button { border-radius: 50%; width: 34px; height: 34px; padding: 0; font-size: 1rem; line-height: 1; }
  .sidebar-actions { display: flex; gap: 8px; align-items: center; justify-content: space-between; }
  .sidebar-downloads { display: grid; gap: 8px; margin-top: 8px; padding-top: 14px; border-top: 1px solid var(--ife-line); }
  .sidebar-downloads .btn { width: 100%; }

  /* Selectize */
  .selectize-input { border-color: var(--ife-input); box-shadow: none; }
  .selectize-input.focus { border-color: var(--ife-ink); box-shadow: 0 0 0 2px rgba(58,64,72,.12); }
  .selectize-dropdown .optgroup-header { font-weight: 700; color: var(--ife-ink); }

  /* ---------- Cards (incl. the chart panels built in the server) ---------- */
  .card, .well { background: #fff; border: 1px solid var(--ife-line); border-radius: 8px; box-shadow: 0 1px 2px rgba(0,0,0,.04); }
  .well { padding: 20px 22px; margin-bottom: 20px; }
  .card-header { font-weight: 600; background: #fff; border-bottom: 1px solid var(--ife-line); }
  .card-help { font-size: .85rem; color: var(--ife-ink-soft); font-weight: 400; }
  .card-help p { margin: 0; }
  h3.table-heading { font-family: 'Arimo', Arial, sans-serif; font-size: 1rem; font-weight: 600; margin: 0; line-height: 1.4; }
  .well h4 { font-size: 1.05rem; }

  /* ---------- Leaflet ---------- */
  .leaflet-container { background: #FFFFFF; font-family: 'Arimo', Arial, sans-serif; }
  .leaflet-control.info.legend { font-size: 11px; padding: 8px 10px; line-height: 1.35; max-width: 300px;
                                 border-radius: 6px; box-shadow: 0 1px 4px rgba(0,0,0,.12); }
  .leaflet-control.info.legend strong { font-size: 11px; }
  .horizontal-legend { width: 290px; }
  .leaflet-tooltip { font-family: 'Arimo', Arial, sans-serif; border-radius: 6px; border-color: var(--ife-line); }

  /* ---------- DataTable ---------- */
  table.dataTable thead th { background: #fff; color: var(--ife-ink-soft); font-size: .72rem;
                             text-transform: uppercase; letter-spacing: .05em; border-bottom: 2px solid var(--ife-ink) !important; }
  table.dataTable tbody td { font-size: .85rem; font-variant-numeric: tabular-nums; }
  .dataTables_wrapper { overflow-x: auto; width: 100%; }
  .dataTables_wrapper .page-link { color: var(--ife-ink); }
  /* selected page: white number on the anthracite background */
  .dataTables_wrapper .pagination .page-item.active .page-link,
  .dataTables_wrapper .paginate_button.current, .dataTables_wrapper .paginate_button.current:hover {
    background: var(--ife-ink) !important; border-color: var(--ife-ink) !important; color: #fff !important; }

  /* ---------- Methodology page ---------- */
  .method-page { max-width: 860px; margin: 0 auto; padding: 36px 8px 40px; line-height: 1.7; }
  .method-page h2 { font-size: 1.9rem; margin-bottom: 18px; }
  .method-page h3 { font-size: 1.25rem; margin: 30px 0 10px; }
  .method-lead { font-size: 1.02rem; color: var(--ife-ink-soft); }
  .criteria { display: grid; grid-template-columns: repeat(2, minmax(0,1fr)); gap: 14px; margin: 18px 0 24px; }
  .criterion { background: #fff; border: 1px solid var(--ife-line); border-top: 3px solid var(--ife-ink); border-radius: 6px; padding: 14px 16px; }
  .criterion-num { font-size: .7rem; text-transform: uppercase; letter-spacing: .1em; color: var(--ife-accent); font-weight: 600; }
  .criterion h4 { font-family: 'Arimo', Arial, sans-serif; font-size: 1rem; font-weight: 700; margin: 4px 0 6px; }
  .criterion p { font-size: .88rem; margin: 0; color: var(--ife-ink-soft); line-height: 1.55; }
  .criterion .crit-rule { display: inline-block; margin-top: 8px; font-size: .78rem; font-family: ui-monospace, Menlo, Consolas, monospace;
                          background: var(--ife-paper); border-radius: 4px; padding: 2px 7px; color: var(--ife-ink); }
  .method-ref { background: #fff; border: 1px solid var(--ife-line); border-radius: 6px; padding: 12px 16px; font-size: .9rem; }

  /* ---------- Related publications page ---------- */
  .pub-list { display: grid; gap: 16px; margin-top: 22px; }
  .pub-card { background: #fff; border: 1px solid var(--ife-line); border-left: 3px solid var(--ife-brand-petrol);
              border-radius: 6px; padding: 18px 20px; }
  .pub-meta { font-size: .72rem; font-weight: 600; text-transform: uppercase; letter-spacing: .08em; color: var(--ife-accent); }
  .pub-card h3 { font-family: 'Merriweather', Georgia, serif; font-size: 1.15rem; margin: 6px 0 4px; line-height: 1.4; }
  .pub-authors { font-size: .9rem; color: var(--ife-ink-soft); margin-bottom: 10px; }
  .pub-card p { font-size: .92rem; margin: 0 0 12px; line-height: 1.6; }
  .pub-links { display: flex; gap: 16px; flex-wrap: wrap; font-size: .88rem; font-weight: 600; }
  .pub-links a { text-decoration: none; }
  .pub-links a:hover { text-decoration: underline; }

  /* ---------- Notifications / progress ---------- */
  #shiny-notification-panel .shiny-notification {
    background: #fff; border: 1px solid var(--ife-line); border-left: 4px solid var(--ife-ink);
    color: var(--ife-ink); border-radius: 6px; box-shadow: 0 2px 8px rgba(0,0,0,.08);
  }
  #shiny-notification-panel .progress { background: var(--ife-paper); height: 8px; border-radius: 4px; }
  #shiny-notification-panel .progress-bar { background: var(--ife-ink); }

  /* ---------- Footer ---------- */
  .site-footer { border-top: 1px solid var(--ife-line); padding: 18px 8px; font-size: .78rem; color: var(--ife-ink-soft);
                 max-width: 1400px; margin: 0 auto; display: flex; justify-content: space-between; flex-wrap: wrap; gap: 8px; }
  .site-footer a { color: var(--ife-ink-soft); }

  .card-footer.map-footer { background: #fff; border-top: 1px solid var(--ife-line); text-align: left; }

  /* ---------- Small screens: map keeps its shape, legend below, sidebar above ---------- */
  .map-legend-below { display: none; }
  @media (max-width: 900px) {
    #dependency_map { height: auto !important; aspect-ratio: 2.1 / 1; }
    #dependency_map .leaflet-control.info.legend { display: none; }
    .map-legend-below { display: block; padding: 10px 14px 12px; border-top: 1px solid var(--ife-line);
                        background: #fff; font-size: 11px; line-height: 1.35; }
    .map-legend-below .horizontal-legend { width: auto; max-width: 340px; }
  }
  @media (max-width: 991px) {
    .atlas-wrap .bslib-sidebar-layout { display: block !important; }
    .atlas-wrap .bslib-sidebar-layout > .sidebar { display: block !important; width: auto !important;
                                                   max-height: none !important; border-right: none;
                                                   border-bottom: 1px solid var(--ife-line); }
    .atlas-wrap .bslib-sidebar-layout > .sidebar > .sidebar-content { display: flex !important; }
    .atlas-wrap .bslib-sidebar-layout > .collapse-toggle { display: none !important; }
    .bslib-sidebar-layout > .sidebar .sidebar-title { padding-right: 0; }
    /* treemap and sector chart one below the other */
    .chart-pair { flex-direction: column; }
    .chart-pair-spacer { display: none; }
  }

  @media (max-width: 991px) {
    .navbar-nav { align-items: flex-start; padding-top: 8px; }
    .navbar .nav-link.nav-ext-first { margin-left: 0; }
    .navbar .nav-link.nav-ext-first::before { display: none; }
  }

  @media (max-width: 900px) {
    .criteria  { grid-template-columns: 1fr; }
    .hero h1   { font-size: 1.7rem; }
    .horizontal-legend { width: 230px; }
  }
"))

## Methodology page
methodology_ui <- function() {
  div(class = "method-page",
      div(class = "hero-eyebrow", "Methodology"),
      h2("How GeoDep identifies trade dependencies"),
      p(class = "method-lead",
        paste0("This app is built on the GeoDep database (IFE-CEPII), using the methodology described in ",
               "Cl\u00e9mence Huang, K\u00e9vin Lefebvre & Pauline Wibaux, \u201cTrade dependencies: GeoDep database\u201d, ",
               "IFE-CEPII Working Paper (forthcoming).")), #to be modified: add the link once the working paper is published
      
      h3("When is a product a dependency?"),
      p("A product (HS 6-digit) is classified as ", tags$strong("import-dependent"),
        " for a country only if it meets all four of the following criteria at once:"),
      div(class = "criteria",
          div(class = "criterion",
              div(class = "criterion-num", "Criterion 1"),
              h4("Import concentration"),
              p("A Herfindahl-Hirschman Index (HHI) computed on the country's import shares by origin exceeds 0.4, meaning its supply of the product is concentrated among few trading partners."),
              span(class = "crit-rule", "HHI imports > 0.4")),
          div(class = "criterion",
              div(class = "criterion-num", "Criterion 2"),
              h4("World export concentration"),
              p("An HHI computed on world export shares for that product also exceeds 0.4: few countries in the world can supply it, so switching to an alternative supplier is hard, not just currently avoided."),
              span(class = "crit-rule", "HHI world exports > 0.4")),
          div(class = "criterion",
              div(class = "criterion-num", "Criterion 3"),
              h4("Non-substitutability by domestic supply"),
              p("The ratio of the country's imports to its own exports of the product is above 1. Exports broadly proxy the domestic production that could be redirected to domestic demand; if imports exceed exports, domestic capacity cannot realistically cover the shortfall."),
              span(class = "crit-rule", "imports / exports > 1")),
          div(class = "criterion",
              div(class = "criterion-num", "Criterion 4"),
              h4("Persistence"),
              p("All three criteria above must hold in at least two of the last three years, so a one-off or temporary spike in concentration does not count as a structural dependency."),
              span(class = "crit-rule", "criteria 1–3 met in ≥ 2 of 3 years"))
      ),
      p("The same four criteria, applied symmetrically, define ", tags$strong("export-dependent"),
        " products: export concentration and world import concentration replace the import-side equivalents, and the roles of imports and exports are reversed in the non-substitutability ratio. Use the Imports / Exports toggle in the sidebar to switch between the two views."),
      
      h3("Sectors and coverage"),
      p("Sector groupings (critical raw materials, dual use, health, agrifood, energy, other) come from dedicated reference lists (UNCTAD, EU dual-use regulation, CEPII health nomenclature, FAO, World Bank) and are only available for ",
        tags$strong("import"), " dependencies (GeoDep_M); export dependencies (GeoDep_X) are not sector-tagged."),
      p("Figures reflect 2024 (the only year for which the dependency indicators are available in this dataset) and EU-27 member states are aggregated into a single entity (EUN)."),
      
      h3("Reading the map"),
      p("The map shows, for each country, the share of its traded products that are dependent (\u201cShare of products\u201d) or the share of its trade value in those products (\u201cShare of value\u201d); darker means more dependent. Countries in mid grey have no dependent product; countries in pale grey have no data."),
      p("Once a country is selected, the map shows its partners instead: how much of the country\u2019s dependent products each partner leads, as main supplier for imports or main destination for exports."),
      
      h3("Data and citation"),
      div(class = "method-ref",
          p(style = "margin-bottom: 6px;",
            "Data: GeoDep IFE-CEPII (2026), based on BACI (HS 2022). Licence: Creative Commons BY-NC-SA."),
          p(style = "margin-bottom: 6px;",  #to be modified: final reference once the working paper is out
            HTML("Please cite: Huang C., Lefebvre K. &amp; Wibaux P. (2026), \u201cTrade dependencies: GeoDep database\u201d, <em>IFE-CEPII Working Paper</em>, forthcoming.")),
          p(style = "margin: 0;",
            tags$a(href = "https://www.cepii.fr/CEPII/fr/bdd_modele/bdd_modele_item.asp?id=41", #to be modified
                   target = "_blank", "Dataset page and documentation"))
      ),
      div(style = "margin-top: 28px;",
          actionButton("back_to_atlas", "← Back to the explorer", class = "btn-outline-secondary"))
  )
}

## Related publications page
publication_card <- function(meta, title, authors, summary, pdf) {
  div(class = "pub-card",
      div(class = "pub-meta", meta),
      h3(title),
      div(class = "pub-authors", authors),
      p(summary),
      div(class = "pub-links",
          tags$a(href = pdf, target = "_blank", icon("file-pdf"), " Read the full text (PDF)")))
}

publications_ui <- function() {
  div(class = "method-page",
      h2("Related publications"),
      div(class = "pub-list",
          publication_card(
            meta     = "La Lettre du CEPII n°452 · February 2025",
            title    = "Trade Dependencies: While China Is in a Dominant Position, the European Union Is Not Without Advantages",
            authors  = "Pauline Wibaux",
            summary  = "The Covid-19 pandemic and the war in Ukraine have confronted economies with new risks and highlighted the issue of import dependencies. In this respect, China occupies a dominant position as the main supplier of its trading partners’ dependent products. While the United States and the European Union have the same level of dependency, the European Union stands out by also exporting a good number of products on which its partners depend. China is further consolidating its dominant position, as in recent years it has reduced its dependence, while the European Union and the United States have become more dependent on the Chinese economy.",
            pdf      = "https://www.cepii.fr/PDF_PUB/lettre/2025/let452ang.pdf"),
          publication_card(
            meta     = "CEPII Policy Brief n°2024-47 · September 2024",
            title    = "Import Dependencies: Where Does the EU Stand?",
            authors  = "Kevin Lefebvre & Pauline Wibaux",
            summary  = "Faced with growing competition and geopolitical tensions, major economies worry about the risks associated with over-reliance on the Chinese economy. Using detailed product-level import data, we compare both large countries’ trade dependencies and the extent to which they supply their trading partners with products they depend on. China stands out for its low number of dependent products. While the United States and the European Union have a similar number of trade dependencies, this number is larger for Japan. The sources of dependencies are common to all four countries, and lie in four sectors: chemicals, electronics, pharmaceuticals and the steel industry. The EU is heavily exposed to China: 61% of its import dependencies come from that country. This potential vulnerability is partially offset by the fact that the EU is China’s leading supplier for a fourth of its 47 import-dependent products. China is three times as exposed to the EU as it is to the US. The EU dependence on China increased between 2019 and 2022 owing to both an increase in the number of European dependencies on China and a reduction in the number of Chinese dependencies on the EU.",
            pdf      = "https://www.cepii.fr/PDF_PUB/pb/2024/pb2024-47.pdf"),
          publication_card(
            meta     = "CEPII Policy Brief n°2023-41 · June 2023",
            title    = "EU Strategic Dependencies: A Long View",
            authors  = "Vincent Vicard & Pauline Wibaux",
            summary  = "We analyze how trade dependencies have evolved over time to better understand how they relate to vulnerabilities. We focus on the EU-27 and apply the bottom-up approach laid out by the European Commission (EC) using trade data from 1996 to 2019. The number of dependent products has shown no clear pattern since the mid-1990s, nor has their sectoral composition. The geography of dependencies has, however, evolved towards dependencies concentrated on Chinese suppliers, but this shift had already occurred in 2010. The products identified as dependent exhibit significant churning over time: one out of five dependent products in 2018 was not identified as such one year later in 2019 and close to half of dependent products in 2014 were not identified as such five years later.",
            pdf      = "https://www.cepii.fr/PDF_PUB/pb/2023/pb2023-41.pdf")
      ),
      div(style = "margin-top: 28px;",
          actionButton("back_to_atlas_pub", "← Back to the explorer", class = "btn-outline-secondary"))
  )
}

## Explorer page
explorer_ui <- function() {
  tagList(
    div(class = "hero",
        h1("GeoDep \u2014 Trade dependency explorer"),
        p(class = "hero-lead",
          "Who depends on whom, for what? Explore import and export dependencies for every country, product by product."),
        div(class = "hero-meta",
            tags$span("Data: BACI, HS 2022"),
            tags$span("Year: 2024")),
        div(class = "hero-meta hero-cite",  #to be modified: final reference once the working paper is out
            HTML("Please cite: Huang C., Lefebvre K. &amp; Wibaux P. (2026), \u201cTrade dependencies: GeoDep database\u201d, <em>IFE-CEPII Working Paper</em>, forthcoming."))
    ),
    
    div(class = "atlas-wrap",
        layout_sidebar(
          fillable = FALSE,
          sidebar = sidebar(
            width = 300, bg = "#FFFFFF",
            ## phones: sidebar always shown above the map (tablets: same, via the CSS below 992px)
            open = list(desktop = "open", mobile = "always-above"),
            title = "View",   # bslib puts the title on the same row as the collapse arrow
            radioGroupButtons("dep_direction", "Dimension of dependencies",
                              choices  = c("↓ Imports" = "import", "↑ Exports" = "export"),
                              selected = "import", justified = TRUE, size = "sm",
                              status = "segmented"),
            radioGroupButtons("map_metric", "Map shows",
                              choices  = c("Share of products" = "count", "Share of value" = "value"),
                              selected = "count", justified = TRUE, size = "sm",
                              status = "segmented"),
            selectInput("sector_filter", "Sector",
                        choices = sector_choices_ui, selected = "all", width = "100%"),
            
            ## "Countries" is a field label like "Sector"; importer / exporter / slider
            ## are its sub-fields (smaller, grey labels, indented)
            div(class = "side-group",
                div(class = "side-group-label", "Countries"),
                div(class = "side-subgroup",
                    selectizeInput("importer_select", "Importer (destination)",
                                   choices = country_choices_ui, selected = "",
                                   options = list(placeholder = "Type a country name…"),
                                   width = "100%"),
                    div(class = "swap-wrap",
                        actionButton("swap_button", "⇅", class = "btn-outline-secondary",
                                     title = "Swap the two countries")),
                    selectizeInput("exporter_select", "Exporter (origin)",
                                   choices = country_choices_ui, selected = "",
                                   options = list(placeholder = "Type a country name…"),
                                   width = "100%"),
                    conditionalPanel(
                      condition = "(input.importer_select !== '' && input.exporter_select === '') || (input.importer_select === '' && input.exporter_select !== '')",
                      sliderInput("top_n_slider", "Top partners in treemap",
                                  min = 3, max = 10, value = 5, step = 1, width = "100%", ticks = FALSE)
                    ),
                )
            ),
            
            actionButton("reset_button", "Reset selection", class = "btn-reset w-100",
                         icon = icon("rotate-left")),
            
            div(class = "sidebar-downloads",
                conditionalPanel(
                  condition = "input.importer_select !== ''",
                  downloadButton("download_table", "Download selection (ZIP)", class = "btn-accent")
                ),
                downloadButton("download_full", "Download full dataset (ZIP)", class = "btn-outline-secondary")
            )
          ),
          
          ## ---- main column ----
          card(
            full_screen = TRUE,
            card_header(
              div(class = "card-help", uiOutput("intro_text", inline = FALSE))
            ),
            card_body(
              padding = 0,
              withSpinner(leafletOutput("dependency_map", height = "560px"),
                          type = 8, color = IFE$ink, size = 1.2),
              ## copy of the map legend, shown under the map on small screens only
              div(id = "map_legend_below", class = "map-legend-below")
            ),
            card_footer(
              class = "map-footer",
              downloadButton("download_map", "Download map (PNG)", class = "btn-outline-ife btn-sm")
            )
          ),
          
          uiOutput("partners_panel"),
          
          conditionalPanel(
            condition = "input.importer_select !== ''",
            card(
              card_header(uiOutput("table_heading")),
              card_body(
                withSpinner(DTOutput("dependency_table"), type = 8, color = IFE$ink, size = 1)
              )
            )
          )
        )
    ),
    
    div(class = "site-footer",
        tags$span("© IFE | CEPII — GeoDep (2026) · Licence CC BY-NC-SA"),
        tags$span(tags$a(href = "https://www.cepii.fr/CEPII/fr/bdd_modele/bdd_modele_item.asp?id=41",
                         target = "_blank", "Dataset page"),
                  " · ",
                  tags$a(href = "https://ife.eco/", target = "_blank", "ife.eco"))
    )
  )
}

ui <- page_navbar(
  id    = "main_nav",
  theme = ife_theme,
  title = div(class = "brand-wrap",
              tags$img(src = LOGO_IFE_FULL, class = "brand-full",
                       alt = "IFE - Institut Fran\u00e7ais d'\u00c9conomie"),
              tags$img(src = LOGO_IFE_MARK, class = "brand-mark", alt = "IFE"),
              div(class = "brand-sep"),
              span(class = "brand-name", "GeoDep")),
  window_title = "GeoDep — Trade dependency explorer | IFE",
  fillable = FALSE,
  header = tags$head(
    tags$style(ife_css),
    tags$script(HTML("Shiny.addCustomMessageHandler('scroll_top', function(m) { window.scrollTo(0, 0); });")),
    tags$script(HTML("Shiny.addCustomMessageHandler('map_legend', function(m) { var el = document.getElementById('map_legend_below'); if (el) el.innerHTML = m.html; });")),
    tags$script(HTML("Shiny.addCustomMessageHandler('set_label', function(m) { var el = document.getElementById(m.id + '-label'); if (el) el.textContent = m.label; });")),
    tags$script(HTML(equal_earth_js))
  ),
  nav_spacer(),   # pushes all the links to the right-hand side of the bar
  nav_panel("Explorer", value = "atlas", explorer_ui()),
  nav_panel("Methodology", value = "methodology", methodology_ui()),
  nav_panel("Publications", value = "publications", publications_ui()),
  nav_item(tags$a(class = "nav-link nav-ext-first", target = "_blank",
                  href = "https://www.cepii.fr/CEPII/fr/bdd_modele/bdd_modele_item.asp?id=41",
                  "Data \u2197")),
  nav_item(tags$a(class = "nav-link", target = "_blank",
                  href = "https://ife.eco/", "ife.eco \u2197"))
)

## -----------------------------------------------------------------------
## 3. Server
## -----------------------------------------------------------------------

server <- function(input, output, session) {
  selected_countries <- reactiveVal(character())
  suppress_input_sync <- reactiveVal(0L)  
  
  
  observeEvent(c(input$back_to_atlas, input$back_to_atlas_pub), {
    nav_select("main_nav", "atlas")
    session$sendCustomMessage("scroll_top", list())
  }, ignoreInit = TRUE)
  
  ## only the reset button clears the selection (changing the sector or the
  ## direction keeps the selected countries)
  observeEvent(input$reset_button, {
    selected_countries(character())
  })

  observeEvent(input$dep_direction, {
    if (input$dep_direction == "export") {
      updateSelectInput(session, "sector_filter",
                        choices = c("All sectors" = "all"),
                        selected = "all")
      ## (updateSelectizeInput(label = ...) fails on some Shiny versions:
      ##  "labelNode must be of length 1", so the label text is set directly)
      session$sendCustomMessage("set_label", list(id = "importer_select", label = "Exporter (origin)"))
      session$sendCustomMessage("set_label", list(id = "exporter_select", label = "Importer (destination)"))
    } else {
      updateSelectInput(session, "sector_filter",
                        choices = sector_choices_ui,
                        selected = "all")
      session$sendCustomMessage("set_label", list(id = "importer_select", label = "Importer (destination)"))
      session$sendCustomMessage("set_label", list(id = "exporter_select", label = "Exporter (origin)"))
    }
  }, ignoreInit = FALSE)
  
  exclude_choice <- function(choices, exclude_val) {
    if (is.list(choices)) {
      lapply(choices, function(grp) grp[grp != exclude_val])
    } else {
      choices[choices != exclude_val]
    }
  }
  
  prev_sel <- reactiveVal(character())
  observeEvent(selected_countries(), {
    sel <- selected_countries()
    imp <- if (length(sel) >= 1) sel[1] else ""
    exp <- if (length(sel) >= 2) sel[2] else ""
    
    p_sel <- prev_sel()
    p_imp <- if (length(p_sel) >= 1) p_sel[1] else ""
    p_exp <- if (length(p_sel) >= 2) p_sel[2] else ""
    
    changed_imp <- !identical(imp, p_imp)
    changed_exp <- !identical(exp, p_exp)
    
    choices_imp <- if (exp != "") exclude_choice(country_choices_ui, exp) else country_choices_ui
    choices_exp <- if (imp != "") exclude_choice(country_choices_ui, imp) else country_choices_ui
    
    need_imp_ui <- !identical(input$importer_select, imp)
    need_exp_ui <- !identical(input$exporter_select, exp)
    
    update_imp <- need_imp_ui || changed_exp
    update_exp <- need_exp_ui || changed_imp
    
    prev_sel(sel)
    
    if (update_imp || update_exp) {
      if (update_imp) {
        updateSelectizeInput(session, "importer_select",
                             choices = choices_imp, selected = imp)
      }
      if (update_exp) {
        updateSelectizeInput(session, "exporter_select",
                             choices = choices_exp, selected = exp)
      }
    }
    
  }, ignoreInit = TRUE)
  
  observeEvent(input$importer_select, {
    cur <- selected_countries()
    new_imp <- input$importer_select
    new_sel <- c(new_imp, if (length(cur) >= 2) cur[2] else NA)
    new_sel <- new_sel[!is.na(new_sel) & new_sel != ""]
    
    if (!identical(new_sel, cur)) {
      selected_countries(new_sel)
    }
  }, ignoreInit = TRUE)
  
  observeEvent(input$exporter_select, {
    cur <- selected_countries()
    imp <- if (length(cur) >= 1) cur[1] else NA
    new_sel <- c(imp, input$exporter_select)
    new_sel <- new_sel[!is.na(new_sel) & new_sel != ""]
    
    if (!identical(new_sel, cur)) {
      selected_countries(new_sel)
    }
  }, ignoreInit = TRUE)
  
  sector_map_data <- reactive({
    direction <- input$dep_direction
    
    base <- if (direction == "import") {
      traded_by_country |>
        rename(iso_plot = iso_d, dep1 = dependant_M_MC_t, dep2 = c4_M_MC, val = import_dpt)
    } else {
      traded_by_country_export |>
        rename(iso_plot = iso_o, dep1 = dependant_X_MC_t, dep2 = c4_X_MC, val = export_opt)
    }
    
    if (input$sector_filter != "all" && input$sector_filter %in% names(base)) {
      base <- base |> filter(.data[[input$sector_filter]] == 1)
    }
    
    total_traded <- base |>
      distinct(iso_plot, hs6, val) |>
      group_by(iso_plot) |>
      summarise(n_total = n(), total_value = sum(val, na.rm = TRUE), .groups = "drop")
    
    dependent <- base |>
      filter(dep1 == 1, dep2 == 1) |>
      distinct(iso_plot, hs6, val) |>
      group_by(iso_plot) |>
      summarise(n_dep = n(), dep_value = sum(val, na.rm = TRUE), .groups = "drop")
    
    total_traded |>
      left_join(dependent, by = "iso_plot") |>
      mutate(
        n_dep       = replace_na(n_dep, 0),
        dep_value   = replace_na(dep_value, 0),
        count_share = 100 * n_dep / n_total,
        value_share = 100 * dep_value / total_value
      ) |>
      select(iso_plot, count_share, value_share, n_dep, n_total, dep_value, total_value)
  }) |> bindCache(input$sector_filter, input$dep_direction)
  
  export_hover_data <- reactive({
    export_base <- dep_export_base
    
    export_top3 <- export_base |>
      group_by(iso_o, hs6) |>
      mutate(dest_share = imports / export_opt) |>
      ungroup() |>
      filter(dest_share > 0.5) |>
      group_by(iso_o, iso_d) |>
      summarise(dep_count = n(), .groups = "drop") |>
      arrange(iso_o, desc(dep_count)) |>
      group_by(iso_o) |>
      slice_head(n = 3) |>
      mutate(part = paste0(iso_name(iso_d), " (", dep_count, ")")) |>
      summarise(export_top3 = paste(part, collapse = "; "), .groups = "drop") |>
      rename(iso_plot = iso_o)
    
    export_top3
  })
  
  partner_map_data <- reactive({
    selection <- selected_countries()
    req(length(selection) >= 1)
    iso1 <- selection[1]
    
    if (input$dep_direction == "import") {
      base       <- dep_import_base |> filter(iso_d == iso1)
      partner_col <- "iso_o"
      total_col   <- "import_dpt"
      value_col   <- "imports"
    } else {
      base       <- dep_export_base |> filter(iso_o == iso1)
      partner_col <- "iso_d"
      total_col   <- "export_opt"
      value_col   <- "imports"
    }
    
    if (input$sector_filter != "all" && input$sector_filter %in% names(base)) {
      base <- base |> filter(.data[[input$sector_filter]] == 1)
    }
    
    if (nrow(base) == 0) return(NULL)
    
    base <- base |> rename(partner_iso = all_of(partner_col))
    
    totals <- base |>
      distinct(hs6, .data[[total_col]]) |>
      summarise(total_value = sum(.data[[total_col]], na.rm = TRUE), n_products = n())
    
    base |>
      group_by(hs6) |>
      mutate(partner_share = .data[[value_col]] / .data[[total_col]]) |>
      ungroup() |>
      group_by(partner_iso) |>
      summarise(
        supply_value = sum(.data[[value_col]], na.rm = TRUE),
        n_dominant   = n_distinct(hs6[partner_share > 0.5]),
        .groups      = "drop"
      ) |>
      mutate(
        iso_plot    = partner_iso,
        n_dep       = n_dominant,
        n_total     = totals$n_products,
        dep_value   = supply_value,
        total_value = totals$total_value,
        count_share = 100 * n_dominant / totals$n_products,
        value_share = 100 * supply_value / totals$total_value
      ) |>
      select(iso_plot, count_share, value_share, n_dep, n_total, dep_value, total_value)
  })
  
  active_map_data <- reactive({
    sel <- selected_countries()
    if (length(sel) == 0) {
      sector_map_data()
    } else {
      data <- partner_map_data()
      if (is.null(data)) {
        data <- tibble(
          iso_plot = character(), count_share = double(), value_share = double(),
          n_dep = double(), n_total = double(), dep_value = double(), total_value = double()
        )
      }
      data
    }
  })
  
  sector_map_sf <- reactive({
    counts <- active_map_data()
    sel    <- selected_countries()
    
    out <- world_polygons |>
      mutate(iso_plot = to_eun(iso_a3)) |>
      mutate(feature_id = paste0(iso_a3, "___", row_number())) |>
      left_join(counts, by = "iso_plot")
    
    
    if (length(sel) >= 1) {
      countries_with_data <- if (input$dep_direction == "import") {
        countries_with_data_import      # precomputed in preparation.R
      } else {
        countries_with_data_export
      }
      
      n_tot <- if (all(is.na(out$n_total))) 0 else max(out$n_total, na.rm = TRUE)
      
      out <- out |>
        mutate(
          has_data    = iso_plot %in% countries_with_data,
          count_share = if_else(is.na(count_share) & has_data, 0, count_share),
          value_share = if_else(is.na(value_share) & has_data, 0, value_share),
          n_dep       = if_else(is.na(n_dep)       & has_data, 0, n_dep),
          n_total     = if_else(is.na(n_total)     & has_data, n_tot, n_total)
        )
    }
    out
  })
  
  output$intro_text <- renderUI({
    if (input$dep_direction == "export") {
      p("Click a country to see its dependencies, then click a second one to focus on a trading partner.")
    } else {
      p("Click a country to see its dependencies, then click a second one to focus on a trading partner.")
    }
  })
  
  output$dependency_map <- renderLeaflet({
    if (MAP_PROJECTION == "equal_earth") {
      ## Area to fit in the frame: the whole globe, or just the land
      fit_js <- if (MAP_GLOBE_OUTLINE) {
        "var halfW = proj.project(L.latLng(0, 180)).x;
               var ext = [-halfW, halfW, proj.project(L.latLng(-60, 0)).y, proj.project(L.latLng(90, 0)).y];"
      } else {
        paste0("var ext = [", paste(land_extent, collapse = ", "), "];
               el.style.background = '", COL_OCEAN, "';")
      }
      m <- leaflet(options = leafletOptions(crs = crs_equal_earth, zoomControl = FALSE,
                                            minZoom = 0, maxZoom = 7,
                                            zoomSnap = 0.1, zoomDelta = 0.5,
                                            attributionControl = FALSE)) |>
        addMapPane("selection", zIndex = 450) |>   # outlines above the countries
        add_eu_hover_outline()
      if (MAP_GLOBE_OUTLINE) {
        m <- m |>
          addPolygons(lng = globe_outline$lng, lat = globe_outline$lat,
                      fillColor = COL_OCEAN, fillOpacity = 1,
                      color = COL_GLOBE, weight = 1, group = "globe",
                      options = pathOptions(interactive = FALSE))
      }
      m |>
        htmlwidgets::onRender(paste0(
          "function(el, x) {
             // 'this' is the htmlwidget instance; the Leaflet map is getMap()
             var map = (this && this.getMap) ? this.getMap() : this;
             L.control.zoom({ position: 'topleft' }).addTo(map);

             // Hovering any EU member shows the outline of the whole EU
             function euOutline() { return map.layerManager ? map.layerManager.getLayer('shape', 'eu_hover_outline') : null; }
             function hookEU(l) {
               if (!l || !l.options || !l.on || l._euHooked) return;
               if (String(l.options.className || '').indexOf('eu-member') < 0) return;
               l._euHooked = true;
               l.on('mouseover', function() { var o = euOutline(); if (o) o.setStyle({ opacity: 1 }); });
               l.on('mouseout',  function() { var o = euOutline(); if (o) o.setStyle({ opacity: 0 }); });
             }
             map.eachLayer(hookEU);
             map.on('layeradd', function(e) { hookEU(e.layer); });

             // Fit the globe (or the land) in the map area
             function fitIt() {
               map.invalidateSize();
               var size = map.getSize();
               if (size.x === 0 || size.y === 0) return;
               var proj = map.options.crs.projection;
               ", fit_js, "
               var res = Math.max((ext[1] - ext[0]) / (size.x * 0.97), (ext[3] - ext[2]) / (size.y * 0.94));
               var zoom = Math.floor((17 - Math.log2(res)) * 10) / 10;
               var center = proj.unproject(L.point((ext[0] + ext[1]) / 2, (ext[2] + ext[3]) / 2));
               map.setMinZoom(0);
               map.setView(center, zoom, { animate: false });
               map.setMinZoom(map.getZoom());        // no zooming out beyond the opening view
               mapExt = ext;
             }

             // Keep the globe in view: after a move or zoom, slide back if the view
             // goes past its edge (centred when the globe is smaller than the view).
             // Done in pixels, because Leaflet's maxBounds cannot describe the oval globe.
             var mapExt = null, clamping = false;
             function keepInside() {
               if (!mapExt || clamping) return;
               var crs = map.options.crs, s = crs.scale(map.getZoom());
               var a = crs.transformation.transform(L.point(mapExt[0], mapExt[3]), s);
               var b = crs.transformation.transform(L.point(mapExt[1], mapExt[2]), s);
               var minX = Math.min(a.x, b.x), maxX = Math.max(a.x, b.x);
               var minY = Math.min(a.y, b.y), maxY = Math.max(a.y, b.y);
               var pb = map.getPixelBounds();
               function shift(lo, hi, vlo, vhi) {
                 if (vhi - vlo >= hi - lo) return (lo + hi) / 2 - (vlo + vhi) / 2;
                 if (vlo < lo) return lo - vlo;
                 if (vhi > hi) return hi - vhi;
                 return 0;
               }
               var dx = shift(minX, maxX, pb.min.x, pb.max.x);
               var dy = shift(minY, maxY, pb.min.y, pb.max.y);
               if (Math.abs(dx) > 0.5 || Math.abs(dy) > 0.5) {
                 clamping = true;
                 map.once('moveend', function() { clamping = false; });
                 map.panBy([dx, dy], { animate: true, duration: 0.25 });
               }
             }
             map.on('moveend', keepInside);

             setTimeout(fitIt, 200);
             setTimeout(fitIt, 800);
             setTimeout(fitIt, 1500);

             window.addEventListener('resize', function() { setTimeout(fitIt, 100); });
             window.addEventListener('orientationchange', function() { setTimeout(fitIt, 300); });
           }"
        ))
    } else {
      leaflet(options = leafletOptions(zoomControl = FALSE, minZoom = 2, maxZoom = 6,
                                       maxBoundsViscosity = 1.0)) |>
        addMapPane("selection", zIndex = 450) |>
        add_eu_hover_outline() |>
        addProviderTiles(providers$Esri.WorldGrayCanvas,
                         options = providerTileOptions(noWrap = TRUE)) |>
        setMaxBounds(-180, -85, 180, 85) |>
        htmlwidgets::onRender(
          "function(el, x) {
             // 'this' is the htmlwidget instance; the Leaflet map is getMap()
             var map = (this && this.getMap) ? this.getMap() : this;
             L.control.zoom({ position: 'topleft' }).addTo(map);

             // Hovering any EU member shows the outline of the whole EU
             function euOutline() { return map.layerManager ? map.layerManager.getLayer('shape', 'eu_hover_outline') : null; }
             function hookEU(l) {
               if (!l || !l.options || !l.on || l._euHooked) return;
               if (String(l.options.className || '').indexOf('eu-member') < 0) return;
               l._euHooked = true;
               l.on('mouseover', function() { var o = euOutline(); if (o) o.setStyle({ opacity: 1 }); });
               l.on('mouseout',  function() { var o = euOutline(); if (o) o.setStyle({ opacity: 0 }); });
             }
             map.eachLayer(hookEU);
             map.on('layeradd', function(e) { hookEU(e.layer); });
             function fitIt() {
               map.invalidateSize();
               map.setMinZoom(0);
               map.fitBounds([[-58, -170], [83, 190]]);
               map.setMinZoom(map.getZoom());        // no zooming out beyond the opening view
             }
             setTimeout(fitIt, 200);
             setTimeout(fitIt, 800);
             setTimeout(fitIt, 1500);
             window.addEventListener('resize', function() { map.invalidateSize(); });
             window.addEventListener('orientationchange', function() {
               setTimeout(function() { map.invalidateSize(); }, 300);
             });
           }"
        )
    }
  })
  
  ## Colours, classes and legend title of the current map view: shared by the
  ## on-screen map and the PNG download, so that both always match
  map_style <- reactive({
    map_sf <- sector_map_sf()
    metric <- input$map_metric
    sel    <- selected_countries()
    ## selected countries are drawn last, so their border sits on top
    if (length(sel) >= 1) map_sf <- map_sf[order(map_sf$iso_plot %in% sel), ]
    direction_label <- if (input$dep_direction == "import") "import" else "export"
    partner_role    <- if (input$dep_direction == "import") "exporter" else "destination"
    
    fill_values <- if (metric == "count") map_sf$count_share else map_sf$value_share
    
    if (length(sel) >= 1) {
      iso1_label <- iso_name_in_sentence(sel[1])
      metric_label <- if (metric == "count") {
        paste0("Share of ", iso1_label, "'s dependent ", direction_label,
               "s led by each ", partner_role)
      } else {
        paste0("Share of ", iso1_label, "'s dependent ", direction_label,
               " trade value by ", partner_role)
      }
    } else {
      metric_label <- if (metric == "count") {
        "Share of products dependent"
      } else {
        "Share of trade value dependent"
      }
    }
    
    ## Classes for this view (see map_breaks)
    breaks    <- if (length(sel) >= 1) partner_breaks(fill_values) else map_breaks(fill_values)
    n_cls     <- length(breaks) - 1
    ramp      <- if (input$dep_direction == "export") MAP_COLOURS_EXPORT else MAP_COLOURS
    cls_cols  <- if (n_cls == length(ramp)) ramp else
      ramp[round(seq(1, length(ramp), length.out = n_cls))]
    cls_index <- cut(pmin(fill_values, max(breaks)), breaks = breaks,
                     include.lowest = TRUE, labels = FALSE)
    ## legend: the last label shows the real maximum when it is above the top limit
    max_val    <- suppressWarnings(max(fill_values, na.rm = TRUE))
    brk_labels <- breaks
    if (is.finite(max_val) && max_val > max(breaks)) brk_labels[length(brk_labels)] <- signif(max_val, 2)
    
    fill_col <- dplyr::case_when(
      is.na(fill_values) ~ COL_NODATA,
      fill_values == 0   ~ COL_ZERO,
      TRUE               ~ cls_cols[pmax(1, pmin(n_cls, cls_index))]
    )
    
    ## Selected countries: the 1st is filled in anthracite (its own cell is empty
    ## on its partner map); the 2nd keeps its data colour with a thin anthracite border
    is_sel1    <- length(sel) >= 1 & map_sf$iso_plot %in% sel[1]
    is_sel2    <- length(sel) >= 2 & map_sf$iso_plot %in% sel[2]
    fill_col[is_sel1] <- IFE$ink
    border_col <- ifelse(is_sel2, IFE$ink, "white")
    border_wt  <- ifelse(is_sel2, 2, 0.6)
    ## EU: member countries keep thin borders; the EU as a whole gets an outline (below)
    eu_selected <- "EUN" %in% sel
    if (eu_selected) border_wt[map_sf$iso_plot == "EUN"] <- 0.8
    list(map_sf = map_sf, metric = metric, sel = sel, fill_values = fill_values,
         metric_label = metric_label, breaks = breaks, n_cls = n_cls, cls_cols = cls_cols,
         brk_labels = brk_labels, fill_col = fill_col, border_col = border_col,
         border_wt = border_wt, eu_selected = eu_selected,
         direction_label = direction_label, partner_role = partner_role)
  })
  
  ## Static version of the map for the PNG download: always the whole world
  ## (the zoom is ignored), same colours, classes and selection as on screen
  make_map_plot <- function() {
    ms <- map_style()
    m  <- ms$map_sf
    old_s2 <- suppressMessages(sf::sf_use_s2(FALSE))
    on.exit(suppressMessages(sf::sf_use_s2(old_s2)))
    ee <- "+proj=eqearth +lon_0=0 +datum=WGS84 +units=m +no_defs"
    is_imp <- input$dep_direction == "import"
    sel    <- ms$sel
    
    m$fill <- ms$fill_col
    m$bcol <- ms$border_col
    m$lw   <- ifelse(ms$border_wt >= 2, 0.5, 0.08)          # Leaflet px -> ggplot mm
    
    ring  <- cbind(globe_outline$lng, globe_outline$lat)
    globe <- st_sfc(st_polygon(list(rbind(ring, ring[1, ]))), crs = 4326)
    
    ## legend keys: one per class, then the greys (and the selected country)
    fmt  <- function(v) format(v, trim = TRUE, drop0trailing = TRUE, scientific = FALSE)
    n    <- ms$n_cls
    keys <- data.frame(col = ms$cls_cols,
                       lab = paste0(fmt(ms$breaks[1:n]), "\u2013", fmt(ms$brk_labels[2:(n + 1)])))
    keys <- rbind(keys, data.frame(col = c(COL_ZERO, COL_NODATA), lab = c("Not dependent (0%)", "No data")))
    if (length(sel) >= 1) keys <- rbind(keys, data.frame(col = IFE$ink, lab = "Selected country"))
    
    ## title and subtitle from the current selection
    title_text <- if (length(sel) >= 1) ms$metric_label else
      paste0(if (is_imp) "Import" else "Export", " dependencies by country: ",
             if (ms$metric == "count") paste0("share of ", if (is_imp) "imported" else "exported", " products that are dependent")
             else paste0("share of ", if (is_imp) "import" else "export", " value in dependent products"))
    sector_lab <- switch(input$sector_filter, all = "All sectors", sect_strategic = "All strategic sectors",
                         sector_names[[input$sector_filter]])
    roles <- if (is_imp) c("importer", "exporter") else c("exporter", "importer")
    sel_txt <- if (length(sel) >= 1)
      paste0("Selected: ", paste0(iso_name(sel), " (", roles[seq_along(sel)], ")", collapse = " and "))
    subtitle_text <- paste(c(if (is_imp) paste("Sector:", sector_lab), sel_txt, "Year: 2024"), collapse = "  \u00b7  ")
    
    p <- ggplot() +
      geom_sf(data = globe, fill = COL_OCEAN, colour = COL_GLOBE, linewidth = 0.3) +
      geom_sf(data = m, aes(fill = fill, colour = bcol, linewidth = lw), show.legend = TRUE) +
      ## every class is listed, even when no country falls in it
      scale_fill_manual(values = setNames(keys$col, keys$col), breaks = keys$col, labels = keys$lab,
                        limits = keys$col, drop = FALSE, name = "Share (%)") +
      scale_colour_identity() +
      scale_linewidth_identity()
    
    if (ms$eu_selected && !is.null(eu_outline)) {
      p <- p + geom_sf(data = eu_outline, fill = NA, colour = IFE$ink, linewidth = 0.5)
    }
    
    ## name labels on the selected countries
    if (length(sel) >= 1) {
      pts <- lapply(sel, function(iso) selection_label_point(m, iso))
      ok  <- !vapply(pts, is.null, logical(1))
      if (any(ok)) {
        xy  <- do.call(rbind, pts[ok])
        lab <- st_sf(label = ifelse(sel == "EUN", "European Union", iso_name(sel))[ok],
                     geometry = st_sfc(lapply(seq_len(nrow(xy)), function(i) st_point(xy[i, ])), crs = 4326))
        p <- p + geom_sf_label(data = lab, aes(label = label), size = 3, colour = IFE$ink,
                               fill = "white", linewidth = 0.2, fontface = "bold")
      }
    }
    
    p +
      coord_sf(crs = ee, datum = NA, expand = FALSE) +
      labs(title = title_text, subtitle = subtitle_text, x = NULL, y = NULL,
           caption = paste0("Source: GeoDep IFE-CEPII (2026), based on BACI (HS 2022).\n",
                            "Please cite: Huang C., Lefebvre K. & Wibaux P. (2026), \u201cTrade dependencies: GeoDep database\u201d, ",
                            "IFE-CEPII Working Paper, forthcoming.")) +   #to be modified: final reference
      guides(fill = guide_legend(nrow = 2, byrow = TRUE, title.position = "top",
                                 override.aes = list(colour = NA, fill = keys$col))) +
      theme_void(base_size = 11) +
      theme(
        plot.title    = element_text(face = "bold", size = 15, colour = IFE$ink, margin = margin(b = 4)),
        plot.subtitle = element_text(size = 10.5, colour = "#666666", margin = margin(b = 10)),
        plot.caption  = element_text(hjust = 0, size = 8.5, colour = "#888888", face = "italic",
                                     margin = margin(t = 10), lineheight = 1.2),
        plot.title.position   = "plot",
        plot.caption.position = "plot",
        legend.position = "bottom",
        legend.justification = "left",
        legend.title  = element_text(size = 9.5, face = "bold", colour = IFE$ink),
        legend.text   = element_text(size = 9, colour = "#444444"),
        legend.key.width  = unit(14, "pt"),
        legend.key.height = unit(10, "pt"),
        plot.margin = margin(14, 16, 10, 16),
        plot.background = element_rect(fill = "white", colour = NA)
      )
  }
  
  output$download_map <- downloadHandler(
    filename = function() {
      sel <- selected_countries()
      paste0("GeoDep_map_", input$dep_direction, "_", input$map_metric,
             if (input$dep_direction == "import" && input$sector_filter != "all")
               paste0("_", sub("^sect_", "", input$sector_filter)),
             if (length(sel)) paste0("_", paste(sel, collapse = "_")), "_2024.png")
    },
    content = function(file) {
      ggsave(filename = file, plot = make_map_plot(), device = "png",
             width = 10, height = 6.6, dpi = 300, bg = "white")
    }
  )
  
  observe({
    ms <- map_style()
    map_sf <- ms$map_sf; metric <- ms$metric; sel <- ms$sel
    metric_label <- ms$metric_label; breaks <- ms$breaks; n_cls <- ms$n_cls
    cls_cols <- ms$cls_cols; brk_labels <- ms$brk_labels; fill_col <- ms$fill_col
    border_col <- ms$border_col; border_wt <- ms$border_wt; eu_selected <- ms$eu_selected
    
    ## Hover box: three lines (dependent count / total / share), adapted to the view
    is_imp   <- input$dep_direction == "import"
    flow_cap <- if (is_imp) "Imported" else "Exported"
    flow     <- if (is_imp) "imports" else "exports"
    fmt_n    <- function(v) format(round(ifelse(is.na(v), 0, v)), big.mark = " ", trim = TRUE)
    fmt_usd  <- function(k) {                       # values are in thousands of USD
      k <- ifelse(is.na(k), 0, k)
      ifelse(k >= 1e6, sprintf("$%.1f bn", k / 1e6),
             ifelse(k >= 1e3, sprintf("$%.0f m", k / 1e3), sprintf("$%.1f m", k / 1e3)))
    }
    fmt_pct  <- function(v) paste0(format(round(v, 1), nsmall = 1, trim = TRUE), "%")
    label_text <- with(map_sf, {
      header <- if_else(iso_plot == "EUN", "European Union (EU-27)", name)
      if (length(sel) == 0) {
        detail <- if (metric == "count") {
          paste0("Dependent products: ", fmt_n(n_dep),
                 "<br/>", flow_cap, " products: ", fmt_n(n_total),
                 "<br/>Share: ", fmt_pct(count_share))
        } else {
          paste0("Value of dependent ", flow, ": ", fmt_usd(dep_value),
                 "<br/>Total ", flow, ": ", fmt_usd(total_value),
                 "<br/>Share: ", fmt_pct(value_share))
        }
        detail <- ifelse(is.na(count_share), "No data", detail)
      } else {
        ## partner countries: one short line ("Leads 208 dependent products (44.9%)");
        ## the 2nd selected country also gets its role above it
        short <- if (metric == "count") {
          paste0("Leads ", fmt_n(n_dep), " dependent products (", fmt_pct(count_share), ")")
        } else {
          paste0(if (is_imp) "Supplies " else "Absorbs ", fmt_usd(dep_value), " (", fmt_pct(value_share), ")")
        }
        detail <- ifelse(is.na(count_share), "No data", short)
        if (length(sel) >= 2) {
          detail <- ifelse(iso_plot == sel[2],
                           paste0("Selected ", if (is_imp) "exporter" else "importer", "<br/>", short), detail)
        }
        detail <- ifelse(iso_plot == sel[1], "Selected country", detail)
      }
      paste0("<strong>", header, "</strong><br/>", detail)
    })
    label_arg <- lapply(label_text, HTML)
    
    fmt_brk <- function(v) format(v, trim = TRUE, drop0trailing = TRUE, scientific = FALSE)
    swatches_html <- paste0(
      "<div style='flex:1; height:12px; background:", cls_cols, ";'></div>",
      collapse = ""
    )
    labels_html <- paste0(
      "<div style='position:absolute; left:", 100 * (seq_along(breaks) - 1) / n_cls, "%; top:0; ",
      "transform:translateX(-50%); font-size:10px; white-space:nowrap; color:#5A6069;'>",
      fmt_brk(brk_labels), "</div>",
      collapse = ""
    )
    key_html <- function(col, label) {
      paste0("<span style='display:inline-flex; align-items:center; gap:5px;'>",
             "<i style='display:inline-block; width:12px; height:12px; background:", col,
             "; border:1px solid rgba(0,0,0,.08);'></i>", label, "</span>")
    }
    
    legend_html <- paste0(
      "<div class='horizontal-legend'>",
      "<div style='font-weight:600; margin-bottom:6px;'>", metric_label, " (%)</div>",
      "<div style='margin:0 10px;'>",
      "<div style='display:flex; gap:2px;'>", swatches_html, "</div>",
      "<div style='position:relative; height:16px; margin-top:2px;'>", labels_html, "</div>",
      "</div>",
      "<div style='display:flex; gap:14px; margin-top:4px;'>",
      key_html(COL_ZERO, "Not dependent (0%)"),
      key_html(COL_NODATA, "No data"),
      "</div>",
      "<div style='font-size:10px; color:#5A6069; margin-top:6px;'>",
      "Source: GeoDep IFE-CEPII (2026)</div>",
      "</div>"
    )
    
    ## EU members are drawn without the single-country hover border: hovering
    ## any of them shows the outline of the whole EU instead (see the map's JS)
    is_eu <- map_sf$iso_plot == "EUN"
    add_countries <- function(map, idx, eu) {
      if (!any(idx)) return(map)
      addPolygons(
        map,
        data        = map_sf[idx, ],
        group       = "countries",
        fillColor   = fill_col[idx],
        weight      = border_wt[idx],
        color       = border_col[idx],
        opacity     = 1,
        fillOpacity = if (MAP_PROJECTION == "equal_earth") 1 else 0.85,
        highlightOptions = if (eu) NULL else highlightOptions(
          weight      = 2,
          color       = IFE$ink,
          fillOpacity = 1,
          bringToFront = TRUE
        ),
        options      = pathOptions(className = if (eu) "eu-member" else ""),
        layerId      = map_sf$feature_id[idx],
        label        = label_arg[idx],
        labelOptions = labelOptions(
          style     = list("font-size" = "12px", "min-width" = "200px", "max-width" = "280px", "white-space" = "normal"),
          textsize  = "12px",
          direction = "auto"
        )
      )
    }
    
    proxy <- leafletProxy("dependency_map") |>
      clearGroup("countries") |>
      clearControls() |>
      add_countries(is_eu, eu = TRUE) |>
      add_countries(!is_eu, eu = FALSE) |>
      addControl(
        html      = HTML(legend_html),
        position  = "bottomleft",
        layerId   = "legend_main",
        className = "info legend"
      )
    session$sendCustomMessage("map_legend", list(html = legend_html))   # copy under the map (small screens)

    if (eu_selected && !is.null(eu_outline)) {
      proxy <- proxy |>
        addPolygons(data = eu_outline, group = "countries", fill = FALSE,
                    color = IFE$ink, weight = 2, opacity = 1,
                    options = pathOptions(pane = "selection", interactive = FALSE))
    }
    
    ## Name labels on the selected countries ("United States · Importer")
    if (length(sel) >= 1) {
      roles <- if (input$dep_direction == "import") c("Importer", "Exporter") else c("Exporter", "Importer")
      pts <- lapply(sel, function(iso) selection_label_point(map_sf, iso))
      ok  <- !vapply(pts, is.null, logical(1))
      if (any(ok)) {
        nm  <- ifelse(sel == "EUN", "European Union", iso_name(sel))
        txt <- paste0("<b>", nm, "</b><span class='sel-role'> \u00b7 ", roles[seq_along(sel)], "</span>")
        xy  <- do.call(rbind, pts[ok])
        proxy |>
          addLabelOnlyMarkers(
            lng = xy[, 1], lat = xy[, 2], group = "countries",
            label = lapply(txt[ok], HTML),
            labelOptions = labelOptions(noHide = TRUE, direction = "top", textOnly = FALSE,
                                        className = "sel-label", offset = c(0, -4))
          )
      }
    }
  })
  
  observeEvent(input$dependency_map_shape_click, {
    click <- input$dependency_map_shape_click
    raw_iso <- sub("___.*$", "", click$id)
    clicked_iso <- to_eun(raw_iso)
    current_selection <- selected_countries()
    
    if (length(current_selection) >= 2) {
      selected_countries(clicked_iso)
    } else if (clicked_iso %in% current_selection) {
      return(NULL)
    } else {
      selected_countries(c(current_selection, clicked_iso))
    }
  })
  
  observeEvent(input$swap_button, {
    current_selection <- selected_countries()
    if (length(current_selection) == 2) {
      selected_countries(rev(current_selection))
    }
  })
  
  sector_chart_data <- reactive({
    selection <- selected_countries()
    req(length(selection) >= 1)
    req(input$dep_direction == "import")
    
    iso1 <- selection[1]
    iso2 <- if (length(selection) >= 2) selection[2] else NA_character_
    
    sector_cols <- setdiff(unlist(sector_choices_ui, use.names = FALSE),
                           c("all", "sect_strategic"))
    
    base <- dep_import_base |>
      filter(iso_d == iso1) |>
      group_by(hs6) |>
      mutate(partner_share = imports / import_dpt) |>
      ungroup() |>
      mutate(is_dominant = !is.na(iso2) & iso_o == iso2 & partner_share > 0.5)
    
    if (nrow(base) == 0) return(NULL)
    
    dominant_by_hs6 <- base |>
      group_by(hs6) |>
      summarise(is_dominant = any(is_dominant), .groups = "drop")
    
    total_dep      <- nrow(dominant_by_hs6)
    total_dominant <- sum(dominant_by_hs6$is_dominant)
    
    sectors_by_hs6 <- base |>
      distinct(hs6, across(all_of(sector_cols)))
    
    strategic_hs6 <- sectors_by_hs6 |>
      filter(if_any(all_of(sector_cols), ~ .x == 1)) |>
      pull(hs6)
    
    total_strategic          <- length(unique(strategic_hs6))
    total_dominant_strategic <- sum(dominant_by_hs6$is_dominant[dominant_by_hs6$hs6 %in% strategic_hs6])
    
    ## all products imported by iso1 in each sector (denominator of the share)
    traded_by_sector <- traded_by_country |>
      filter(iso_d == iso1) |>
      distinct(hs6, across(all_of(sector_cols))) |>
      pivot_longer(cols = all_of(sector_cols), names_to = "sector_code", values_to = "flag") |>
      filter(flag == 1) |>
      count(sector_code, name = "n_traded")

    out <- dominant_by_hs6 |>
      left_join(sectors_by_hs6, by = "hs6") |>
      pivot_longer(cols = all_of(sector_cols), names_to = "sector_code", values_to = "flag") |>
      filter(flag == 1) |>
      group_by(sector_code) |>
      summarise(
        n_dep      = n(),
        n_dominant = sum(is_dominant),
        .groups    = "drop"
      ) |>
      left_join(traded_by_sector, by = "sector_code") |>
      mutate(Sector_Name    = unlist(sector_names[sector_code]),
             share_dominant = if_else(n_dep > 0, 100 * n_dominant / n_dep, 0),
             share_dep      = if_else(!is.na(n_traded) & n_traded > 0, 100 * n_dep / n_traded, NA_real_)) |>
      select(-sector_code)
    
    attr(out, "total_dep")               <- total_dep
    attr(out, "total_dominant")          <- total_dominant
    attr(out, "total_strategic")         <- total_strategic          
    attr(out, "total_dominant_strategic") <- total_dominant_strategic
    out
  })
  
  make_sector_chart <- function(width_px = NULL) {
    if (input$dep_direction != "import") return(NULL)
    
    df <- sector_chart_data()
    if (is.null(df) || nrow(df) == 0) return(NULL)
    
    selection <- selected_countries()
    iso1 <- selection[1]
    iso2 <- if (length(selection) >= 2) selection[2] else NA_character_
    direction_label <- "exporter"
    flow_label       <- "import"
    
    dominant_label <- if (!is.na(iso2)) paste0("Leading supplier: ", iso_name(iso2)) else "Other suppliers"
    iso1_txt <- iso_name_in_sentence(iso1)
    iso2_txt <- if (!is.na(iso2)) iso_name_in_sentence(iso2) else NA_character_
    ## same name at the start of a title: capital first letter (The United States...)
    iso1_start <- paste0(toupper(substr(iso1_txt, 1, 1)), substring(iso1_txt, 2))
    
    total_dep      <- attr(df, "total_dep")
    total_dominant <- attr(df, "total_dominant")
    total_strategic          <- attr(df, "total_strategic")            
    total_dominant_strategic <- attr(df, "total_dominant_strategic")  
    
    subtitle_text <- if (!is.na(iso2)) {
      paste0(
        "Out of ", total_strategic, " strategic products for which ", iso1_txt,
        " is import-dependent, there are ", total_dominant_strategic, " for which ",
        iso2_txt, " is the leading exporter"
      )
    } else {
      paste0(
        "Out of ", total_strategic, " strategic products for which ", iso1_txt,
        " is import-dependent"
      )
    }
    subtitle_text <- wrap_title(subtitle_text, width_px, font_pt = 11, default_chars = 90, char_em = 0.56)
    
    df_long <- df |>
      mutate(n_other = n_dep - n_dominant) |>
      select(Sector_Name, n_dep, n_dominant, n_other) |>
      pivot_longer(cols = c(n_dominant, n_other), names_to = "category", values_to = "n") |>
      mutate(category = if_else(category == "n_dominant", dominant_label, "Other"))
    
    sector_order <- df |> arrange(n_dep) |> pull(Sector_Name)
    df_long <- df_long |> mutate(Sector_Name = factor(Sector_Name, levels = sector_order))

    ## bar-end labels: dependent products (share of all products imported in the sector)
    df_totals <- df |>
      mutate(Sector_Name = factor(Sector_Name, levels = sector_order),
             total_label = if_else(is.na(share_dep), as.character(n_dep),
                                   paste0(n_dep, " (", format(round(share_dep, 1), nsmall = 1, trim = TRUE), "%)")))

    ## with one country the bar has a single segment: its count is shown at the bar end only
    segment_labels <- if (!is.na(iso2)) ifelse(df_long$n > 0, df_long$n, "") else ""
    
    ## a single strategic sector selected in the sidebar: its name (and bar) in IFE red
    sel_sector <- sector_names[[input$sector_filter]]
    if (is.null(sel_sector) || input$sector_filter == "sect_other") sel_sector <- NA_character_
    df_long <- df_long |>
      mutate(is_sel   = !is.na(sel_sector) & Sector_Name == sel_sector,
             fill_key = if (is.na(iso2)) if_else(is_sel, "Selected", category) else category)
    df_totals <- df_totals |> mutate(is_sel = !is.na(sel_sector) & Sector_Name == sel_sector)
    axis_labels <- function(x) ifelse(!is.na(sel_sector) & x == sel_sector,
                                      paste0("<b style='color:", IFE$accent, "'>", x, "</b>"),
                                      paste0("<b>", x, "</b>"))
    if (length(selection) >= 2) {
      title_text <- paste0(iso1_start, " ", flow_label, "-dependent products mainly supplied by ", iso2_txt)
    } else {
      title_text <- paste0(iso1_start, " ", flow_label, " dependencies by sector")
    }
    
    title_text <- wrap_title(title_text, width_px, font_pt = 15, default_chars = 64, char_em = 0.68)
    
    p <- ggplot(df_long, aes(x = Sector_Name, y = n, fill = fill_key)) +
      ## two countries: the red already shows the leading supplier, so the other sectors are faded
      geom_col(aes(alpha = if (!is.na(iso2) && !is.na(sel_sector)) is_sel else TRUE),
               width = 0.75, color = "white", linewidth = 0.4) +
      scale_alpha_manual(values = c(`TRUE` = 1, `FALSE` = 0.35), guide = "none") +
      scale_x_discrete(labels = axis_labels) +
      geom_text(
        aes(label = segment_labels),
        position = position_stack(vjust = 0.5),
        color = "white", fontface = "bold", size = 3.3
      ) +
      geom_text(
        data = df_totals, aes(x = Sector_Name, y = n_dep, label = total_label,
                              color = if_else(is_sel, IFE$accent, IFE$ink)),
        inherit.aes = FALSE, hjust = -0.12, fontface = "bold", size = 3.3
      ) +
      scale_color_identity() +
      scale_y_continuous(expand = expansion(mult = c(0, if (!is.null(width_px) && width_px < 450) 0.32 else 0.18))) +
      coord_flip(clip = "off") +
      labs(
        x = NULL,
        y = "Dependent products",
        title = title_text,
        subtitle = subtitle_text,
        fill = NULL,
        caption = paste(vapply(c("Source: GeoDep IFE-CEPII (2026)",
                                 "In brackets: share of dependent products in all products imported in the sector",
                                 "Note: sectors are not mutually exclusive, a product can belong to more than one sector"),
                               wrap_title, character(1), width_px = width_px, font_pt = 8.5,
                               default_chars = 200, char_em = 0.55),
                        collapse = "\n")
      ) +
      theme_minimal(base_size = 12) +
      theme(
        plot.title    = element_text(face = "bold", size = 15, color = IFE$ink,
                                     hjust = 0.5, lineheight = 1.15, margin = margin(b = 6)),
        plot.subtitle = element_text(size = 11, color = "#666666", hjust = 0.5, margin = margin(b = 15)),
        plot.title.position   = "plot",     # centred on the whole chart, not just the bars
        plot.caption.position = "plot",
        plot.caption  = element_text(hjust = 0, size = 8.5, color = "#888888", face = "italic",
                                     margin = margin(t = 14)),
        
        panel.grid.major.y = element_blank(),
        panel.grid.minor   = element_blank(),
        panel.grid.major.x = element_line(color = "#e5e5e5", linewidth = 0.5, linetype = "dashed"),
        
        axis.text.y.left = ggtext::element_markdown(face = "bold", color = "#333333", size = 11),
        axis.text.x   = element_text(color = "#555555"),
        axis.title.x  = element_text(color = "#444444", margin = margin(t = 12)),
        
        legend.position      = "bottom",
        legend.justification = "center",
        legend.margin        = margin(t = 4, b = 0),
        legend.text          = element_text(size = 11, color = "#333333")
      )
    if (!is.na(iso2)) {
      p <- p + scale_fill_manual(values = setNames(c(IFE$accent, IFE$chart),
                                                   c(dominant_label, "Other")))
    } else {
      p <- p + scale_fill_manual(values = c("Other" = IFE$chart, "Selected" = IFE$accent), guide = "none")
    }
    
    p
  }
  
  output$sector_chart <- renderPlot({
    make_sector_chart(session$clientData$output_sector_chart_width)
  })
  output$download_plot <- downloadHandler(
    filename = function() {
      selection <- selected_countries()
      req(length(selection) >= 1)
      iso1 <- selection[1]
      iso2 <- if (length(selection) >= 2) paste0("_", selection[2]) else ""
      direction <- input$dep_direction
      paste0("GeoDep_sector_chart_", direction, "_", iso1, iso2, "_2024.png")
    },
    content = function(file) {
      
      p <- make_sector_chart()
      req(p)
      
      ggsave(filename = file, plot = p, device = "png",
             width = 10, height = 6, dpi = 300, bg = "white")
    }
  )
  
  output$download_partner_plot <- downloadHandler(
    filename = function() {
      selection <- selected_countries()
      req(length(selection) >= 1)
      iso1      <- selection[1]
      direction <- input$dep_direction
      paste0("GeoDep_partner_chart_", direction, "_", iso1, "_2024.png")
    },
    content = function(file) {
      selection <- selected_countries()
      req(length(selection) >= 1)
      iso1      <- selection[1]
      direction <- input$dep_direction
      
      p <- tryCatch(make_partner_sector_chart(iso1, direction, input$top_n_slider,
                                              sector = input$sector_filter), error = function(e) NULL)
      req(p)
      
      ggsave(filename = file, plot = p, device = "png",
             width = 10, height = 6, dpi = 300, bg = "white")
    }
  )
  
  partner_sector_chart_data <- function(iso1, direction = "import", top_n = 10, sector = "all") {
    if (direction == "import") {
      base        <- imports_final |> filter(iso_d == iso1)
      partner_col <- "first_odpt"
    } else {
      base        <- exports_final |> filter(iso_o == iso1)
      partner_col <- "first_dpto"
    }
    
    if ("dependent" %in% names(base)) {
      base <- base |> filter(dependent == 1)
    }
    
    ## sector filter (import dependencies only: export ones are not sector-tagged)
    if (direction == "import" && sector != "all" && sector %in% names(base)) {
      base <- base |> filter(.data[[sector]] == 1)
    }
    
    if (nrow(base) == 0) return(NULL)
    
    df0 <- base |> distinct(hs6, partner = .data[[partner_col]])
    
    topN_partners <- df0 |>
      count(partner, sort = TRUE, name = "n_products") |>
      slice_head(n = top_n) |>
      pull(partner)
    
    if (length(topN_partners) == 0) return(NULL)
    
    partner_labels <- topN_partners
    
    df <- df0 |>
      mutate(partner_group = if_else(partner %in% topN_partners,
                                     partner, "ROW")) |>
      count(partner_group, name = "n_dep")
    
    if (nrow(df) == 0) return(NULL)
    
    df |>
      mutate(partner_group = factor(partner_group, levels = c(partner_labels, "ROW"))) |>
      arrange(partner_group)
  }
  
  make_partner_sector_chart <- function(iso1, direction = "import", top_n = 10, width_px = NULL,
                                        sector = "all") {
    df <- partner_sector_chart_data(iso1, direction, top_n, sector)
    if (is.null(df) || nrow(df) == 0) return(NULL)
    
    partner_levels <- levels(df$partner_group)
    n_partners     <- length(partner_levels) - 1
    
    ## petrol first (as on the map), IFE red second, then bright accents
    base_colors <- c("#1B7F8E", IFE$accent, "#F2A33A", "#4C9F38", "#4F5FB8",
                     "#26A6A0", "#A2479E", "#E5C230", "#2F4E6E", "#E07A5F")
    
    if (n_partners <= length(base_colors)) {
      fill_colors <- setNames(base_colors[seq_len(n_partners)], partner_levels[seq_len(n_partners)])
    } else {
      expanded_pal <- colorRampPalette(base_colors)(n_partners)
      fill_colors <- setNames(expanded_pal, partner_levels[seq_len(n_partners)])
    }
    fill_colors <- c(fill_colors, "ROW" = "#b0b0b0")
    ## dark labels on light boxes (yellow, amber...), white on the others
    lum <- function(hex) { v <- grDevices::col2rgb(hex)[, 1] / 255; sum(c(0.299, 0.587, 0.114) * v) }
    text_colors <- setNames(ifelse(vapply(fill_colors, lum, numeric(1)) > 0.6, IFE$ink, "white"), names(fill_colors))
    
    total_dep <- sum(df$n_dep)
    
    role_label   <- if (direction == "import") "import-dependent" else "export-dependent"
    partner_role <- if (direction == "import") "leading exporters" else "leading destinations"
    
    shown_isos   <- setdiff(partner_levels, "ROW")
    legend_pairs <- c(paste0(shown_isos, " = ", iso_name(shown_isos)),
                      "ROW = Rest of the World")
    legend_text  <- paste(legend_pairs, collapse = ", ")
    legend_text  <- paste(strwrap(legend_text, width = 80), collapse = "<br>")
    
    ## markdown subtitle: each wrapped line of the first sentence is put in bold separately
    ## sector named in the title and subtitle ("... import-dependent health products")
    sector_adj <- if (direction == "import") sector_adjectives[sector] else NA
    products   <- if (is.na(sector_adj)) "products" else paste(sector_adj, "products")
    sub_main <- paste0("Out of ", total_dep, " ", products, " for which ", iso_name_in_sentence(iso1),
                       " is ", role_label)
    sub_main <- strsplit(wrap_title(sub_main, width_px, font_pt = 10, default_chars = 200, char_em = 0.62), "\n")[[1]]
    subtitle_text <- paste0(
      paste0("**", sub_main, "**", collapse = "<br>"), "<br>",
      "Breakdown by Top ", n_partners, " ", partner_role
    )
    title_text <- wrap_title(paste0(sub("^the ", "The ", iso_name_in_sentence(iso1)), " ", role_label, " ", products),
                             width_px, font_pt = 15, default_chars = 200, char_em = 0.68)
    
    df <- df |>
      mutate(
        partner_label = if_else(partner_group == "ROW", "ROW", iso_name(as.character(partner_group))),
        pct      = 100 * n_dep / total_dep,
        lab_text = paste0(partner_label, "\n", n_dep, " (", round(pct, 1), "%)")
      )
    
    ggplot(df, aes(area = n_dep, fill = partner_group, label = lab_text)) +
      geom_treemap(color = "white", size = 1.5) +
      geom_treemap_text(
        aes(colour = text_colors[as.character(partner_group)]),
        place = "centre",
        grow = FALSE,
        reflow = TRUE,
        fontface = "bold",
        size = 11,
        lineheight = 0.9
      ) +
      labs(
        title = title_text,
        subtitle = subtitle_text,
        caption  = "Source: GeoDep IFE-CEPII (2026)"
      ) +
      scale_fill_manual(values = fill_colors, guide = "none") +
      scale_colour_identity() +
      theme_minimal(base_size = 12) +
      theme(
        plot.title    = element_text(face = "bold", hjust = 0.5, size = 15, color = IFE$ink,
                                     margin = margin(b = 4)),
        plot.subtitle = ggtext::element_markdown(hjust = 0.5, size = 10, color = "#666666",
                                                 margin = margin(b = 14), lineheight = 1.3),
        plot.caption  = element_text(hjust = 0, size = 8.5, color = "#888888", face = "italic",
                                     margin = margin(t = 12)),
        plot.caption.position = "plot",
        plot.margin   = margin(10, 10, 10, 10)
      )
  }
  
  output$table_heading <- renderUI({
    selection <- selected_countries()
    req(length(selection) >= 1)
    
    direction <- input$dep_direction
    iso1_name <- iso_name_in_sentence(selection[1])

    if (length(selection) == 1) {
      title_text <- if (direction == "import") {
        paste0("Products for which ", iso1_name, " is import-dependent")
      } else {
        paste0("Products for which ", iso1_name, " is export-dependent")
      }
    } else {
      iso2_name <- iso_name_in_sentence(selection[2])
      title_text <- if (direction == "import") {
        paste0("Products for which ", iso1_name, " is import-dependent and ", iso2_name, " is the leading exporter")
      } else {
        paste0("Products for which ", iso1_name, " is export-dependent and ", iso2_name, " is the first destination")
      }
    }
    h3(class = "table-heading", title_text)
  })
  
  output$partners_panel <- renderUI({
    selection <- selected_countries()
    if (length(selection) == 0) return(NULL)
    
    direction <- input$dep_direction
    
    if (direction == "import") {
      
      if (length(selection) == 1) {
        iso1 <- selection[1]
        plot_id <- "partner_sector_plot_1"
        
        local({
          iso_local <- iso1
          output[[plot_id]] <- renderPlot({
            p <- tryCatch(make_partner_sector_chart(iso_local, "import", input$top_n_slider,
                                                    session$clientData[[paste0("output_", plot_id, "_width")]],
                                                    sector = input$sector_filter),
                          error = function(e) NULL)
            req(p)
            p
          })
        })
        
        return(
          div(class = "well",
              div(class = "chart-pair", style = "display: flex; gap: 24px; align-items: stretch;",
                  div(style = "flex: 1; min-width: 0; display: flex; flex-direction: column;",
                      div(style = "min-height: 68px;",
                          h4(iso_name(iso1))
                      ),
                      withSpinner(plotOutput(plot_id, height = "320px"), type = 8, color = IFE$ink),
                      div(style = "text-align: left; margin-top: auto; padding-top: 10px;",
                          downloadButton("download_partner_plot", "Download chart (PNG)",
                                         class = "btn-outline-ife btn-sm")
                      )
                  ),
                  div(style = "flex: 1; min-width: 0; display: flex; flex-direction: column;",
                      div(class = "chart-pair-spacer", style = "min-height: 68px;"),
                      withSpinner(plotOutput("sector_chart", height = "320px"), type = 8, color = IFE$ink),
                      div(style = "text-align: left; margin-top: auto; padding-top: 10px;",
                          downloadButton("download_plot", "Download chart (PNG)",
                                         class = "btn-outline-ife btn-sm")
                      )
                  )
              )
          )
        )
      } else {
        return(
          div(class = "well",
              withSpinner(plotOutput("sector_chart", height = "320px"), type = 8, color = IFE$ink),
              div(style = "text-align: left; margin-top: 10px;",
                  downloadButton("download_plot", "Download chart (PNG)",
                                 class = "btn-outline-ife btn-sm")
              )
          )
        )
      }
      
    } else {
      
      make_export_plot_panel <- function(idx, plot_id, with_download = FALSE) {
        iso_local <- selection[idx]
        
        local({
          iso_fixed <- iso_local
          output[[plot_id]] <- renderPlot({
            p <- tryCatch(make_partner_sector_chart(iso_fixed, "export", input$top_n_slider,
                                                    session$clientData[[paste0("output_", plot_id, "_width")]]),
                          error = function(e) NULL)
            req(p)
            p
          })
        })
        
        div(style = "flex: 1; min-width: 0; display: flex; flex-direction: column;",
            withSpinner(plotOutput(plot_id, height = "320px"), type = 8, color = IFE$ink),
            if (with_download) {
              div(style = "text-align: left; margin-top: auto; padding-top: 10px;",
                  downloadButton("download_partner_plot", "Download chart (PNG)",
                                 class = "btn-outline-ife btn-sm")
              )
            }
        )
      }
      
      charts <- tryCatch({
        if (length(selection) == 1) {
          div(style = "display: flex;",
              make_export_plot_panel(1, "partner_export_plot_1", with_download = TRUE)
          )
        } else {
          div(style = "display: flex; gap: 24px; align-items: stretch;",
              make_export_plot_panel(1, "partner_export_plot_1", with_download = TRUE)
          )
        }
      }, error = function(e) {
        div("Unable to display the partner breakdown for this selection.")
      })
      
      
      div(class = "well", charts)
    }
  })
  
  filtered_dependency_data <- reactive({
    selection <- selected_countries()
    req(length(selection) >= 1)
    if (length(selection) == 2) {
      subject <- selection[1]
      partner <- selection[2]
      
      if (input$dep_direction == "import") {
        result <- dep_import_base |>
          filter(iso_d == subject, iso_o == partner) |>
          group_by(hs6) |>
          mutate(origin_share = imports / import_dpt) |>
          ungroup() |>
          filter(origin_share > 0.5)
        
        if (input$sector_filter != "all") {
          result <- result |> filter(.data[[input$sector_filter]] == 1)
        }
        
        result <- result |>
          mutate(share_odpt = origin_share * 100) |>
          select(
            `HS6 Product` = hs6,
            `Description` = Description,
            `Total Imports (World, k$)` = import_dpt,
            `Imports from Origin (k$)` = imports,
            `Share from Origin (%)` = share_odpt,
            starts_with("sect_"),
            -sect_strategic
          ) |>
          arrange(desc(`Imports from Origin (k$)`))
      } else {
        result <- dep_export_base |>
          filter(iso_o == subject, iso_d == partner) |>
          group_by(hs6) |>
          mutate(dest_share = imports / export_opt) |>
          ungroup() |>
          filter(dest_share > 0.5)
        
        result <- result |>
          mutate(share_dpto = dest_share * 100) |>
          select(
            `HS6 Product` = hs6,
            `Description` = Description,
            `Total Exports (World, k$)` = export_opt,
            `Exports to Destination (k$)` = imports,
            `Share to Destination (%)` = share_dpto
          ) |>
          arrange(desc(`Exports to Destination (k$)`))
      }
      return(result)
    }
    
    iso1 <- selection[1]
    
    if (input$dep_direction == "import") {
      base        <- dep_import_base |> filter(iso_d == iso1)
      total_col   <- "import_dpt"
      total_label <- "Total Imports (World, k$)"
      partner_label  <- "First exporter (% share)"
      partner_col <- "first_odpt"
      value_col   <- "imports"
      match_col   <- "iso_o"
    } else {
      base        <- dep_export_base |> filter(iso_o == iso1)
      total_col   <- "export_opt"
      total_label <- "Total Exports (World, k$)"
      partner_label  <- "First destination (% share)"
      partner_col <- "first_dpto"
      value_col   <- "imports"
      match_col   <- "iso_d"
    }
    
    if (input$sector_filter != "all" && input$sector_filter %in% names(base)) {
      base <- base |> filter(.data[[input$sector_filter]] == 1)
    }
    
    optional_cols <- intersect(
      c("Description", grep("^sect_", names(base), value = TRUE)),
      names(base)
    )
    
    optional_cols <- setdiff(optional_cols, "sect_strategic")
    
    share_lookup <- base |>
      filter(.data[[match_col]] == .data[[partner_col]]) |>
      mutate(partner_share = 100 * .data[[value_col]] / .data[[total_col]]) |>
      distinct(hs6, partner_share)
    
    result <- base |>
      mutate(!!partner_label := iso_name(.data[[partner_col]])) |>
      distinct(across(all_of(c("hs6", total_col, partner_label, optional_cols)))) |>
      left_join(share_lookup, by = "hs6") |>
      mutate(
        !!partner_label := if_else(
          !is.na(partner_share),
          paste0(.data[[partner_label]], " (", round(partner_share, 1), "%)"),
          .data[[partner_label]]
        )
      ) |>
      select(-partner_share) |>
      arrange(desc(.data[[total_col]]))
    
    colnames(result)[colnames(result) == "hs6"]      <- "HS6 Product"
    colnames(result)[colnames(result) == total_col] <- total_label
    
    result
  })
  
  output$dependency_table <- renderDT({
    data <- filtered_dependency_data()
    sect_cols <- c("sect_crm", "sect_dual_use", "sect_health", "sect_agrifood", "sect_energy", "sect_other")
    sect_labels <- c("Critical raw materials", "Dual use", "Health", "Agrifood", "Energy", "Other")
    
    existing_cols <- intersect(sect_cols, names(data))
    if (length(existing_cols) > 0) {
      if (nrow(data) > 0) {
        data$Sector <- apply(data[existing_cols], 1, function(row) {
          labels_to_use <- sect_labels[sect_cols %in% existing_cols]
          paste(labels_to_use[which(row == 1)], collapse = ", ")
        })
      } else {
        data$Sector <- character(0)
      }
      data <- data |> select(-starts_with("sect_"))
    }
    
    front_cols <- c(
      "HS6 Product",
      "Description",
      "Sector",
      "First exporter (% share)",
      "First destination (% share)",
      "Total Imports (World, k$)",
      "Total Exports (World, k$)"
    )
    existing_front <- intersect(front_cols, names(data))
    remaining_cols <- setdiff(names(data), existing_front)
    data <- data[, c(existing_front, remaining_cols), drop = FALSE]
    
    value_cols <- intersect(
      c("Total Imports (World, k$)", "Total Exports (World, k$)",
        "Imports from Origin (k$)", "Exports to Destination (k$)"),
      colnames(data)
    )
    share_col <- intersect(c("Share from Origin (%)", "Share to Destination (%)"), colnames(data))
    
    order_col <- if ("Imports from Origin (k$)" %in% colnames(data)) {
      "Imports from Origin (k$)"
    } else if ("Exports to Destination (k$)" %in% colnames(data)) {
      "Exports to Destination (k$)"
    } else {
      value_cols[1]
    }
    
    empty_msg <- if (length(selected_countries()) == 2) {
      "No product where the importer is dependent and this exporter is the leading supplier (>50%)."
    } else {
      "No dependent products found for this selection."
    }
    
    dt <- datatable(
      data,
      rownames  = FALSE,
      style     = "bootstrap5",
      class     = "table table-hover compact",
      selection = "none",
      options = list(
        pageLength = 10,
        language = list(
          search      = "Search:",
          lengthMenu  = "Show _MENU_ entries",
          info        = "Showing _START_ to _END_ of _TOTAL_ entries",
          paginate    = list(previous = "Previous", `next` = "Next"),
          emptyTable  = empty_msg
        ),
        order = list(list(which(colnames(data) == order_col) - 1, "desc"))
      )
    ) |>
      formatCurrency(
        columns  = value_cols,
        currency = "",
        interval = 3,
        mark     = ",",
        digits   = 0
      )
    
    if (length(share_col) == 1) {
      dt <- dt |> formatRound(columns = share_col, digits = 1)
    }
    
    dt
  })
  
  selection_full_data <- reactive({
    selection <- selected_countries()
    req(length(selection) >= 1)
    
    direction <- input$dep_direction
    iso1      <- selection[1]
    
    full_data <- if (direction == "import") {
      imports_final |> select(-sect_strategic, -Description)
    } else {
      exports_final |> select(-Description)
    }
    
    if (direction == "import") {
      result <- full_data |> filter(iso_d == iso1)
      
      if (input$sector_filter != "all" && input$sector_filter %in% names(result)) {
        result <- result |> filter(.data[[input$sector_filter]] == 1)
      }
      
      if (length(selection) == 2) {
        iso2   <- selection[2]
        result <- result |> filter(first_odpt == iso2)
      }
    } else {
      result <- full_data |> filter(iso_o == iso1)
      
      if (length(selection) == 2) {
        iso2   <- selection[2]
        result <- result |> filter(first_dpto == iso2)
      }
    }
    
    result
  })
  
  output$download_table <- downloadHandler(
    filename = function() {
      selection <- selected_countries()
      prefix    <- if (input$dep_direction == "import") "GeoDep_M" else "GeoDep_X"
      
      if (length(selection) == 2) {
        paste0(prefix, "_dependencies_from_", selection[2], "_to_", selection[1], "_",
               Sys.Date(), ".zip")
      } else if (length(selection) == 1) {
        paste0(prefix, "_dependencies_", selection[1], "_", input$dep_direction, "_",
               Sys.Date(), ".zip")
      } else {
        paste0(prefix, "_dependencies_", Sys.Date(), ".zip")
      }
    },
    contentType = "application/zip",
    content = function(file) {
      selection <- selected_countries()
      direction <- input$dep_direction
      prefix    <- if (direction == "import") "GeoDep_M" else "GeoDep_X"
      
      zip_filename <- if (length(selection) == 2) {
        paste0(prefix, "_dependencies_from_", selection[2], "_to_", selection[1], "_",
               Sys.Date(), ".zip")
      } else if (length(selection) == 1) {
        paste0(prefix, "_dependencies_", selection[1], "_", direction, "_",
               Sys.Date(), ".zip")
      } else {
        paste0(prefix, "_dependencies_", Sys.Date(), ".zip")
      }
      
      tmp_dir <- tempfile("geodep_export_")
      dir.create(tmp_dir)
      on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)
      
      csv_name  <- paste0(prefix, "_selection.csv")
      csv_path  <- file.path(tmp_dir, csv_name)
      
      write_excel_csv2(selection_full_data(), csv_path) 
      included_ref_files <- copy_reference_files(tmp_dir)
      
      size_kb_unzipped <- format_kb(file.info(csv_path)$size)
      
      readme_path <- file.path(tmp_dir, "README.txt")
      
      readme_text_draft <- generate_readme(
        direction        = direction,
        selection        = selection,
        sector_filter    = input$sector_filter,
        map_metric       = input$map_metric,
        zip_filename     = zip_filename,
        size_kb_unzipped = size_kb_unzipped,
        size_kb_zipped   = NULL
      )
      writeLines(readme_text_draft, readme_path, useBytes = TRUE)
      
      tmp_zip_path <- file.path(tmp_dir, "__probe.zip")
      zip::zip(
        zipfile = tmp_zip_path,
        files   = c(basename(csv_path), basename(readme_path), included_ref_files),
        root    = tmp_dir
      )
      size_kb_zipped <- format_kb(file.info(tmp_zip_path)$size)
      file.remove(tmp_zip_path)
      
      readme_text_final <- generate_readme(
        direction        = direction,
        selection        = selection,
        sector_filter    = input$sector_filter,
        map_metric       = input$map_metric,
        zip_filename     = zip_filename,
        size_kb_unzipped = size_kb_unzipped,
        size_kb_zipped   = size_kb_zipped
      )
      writeLines(readme_text_final, readme_path, useBytes = TRUE)
      
      zip::zip(
        zipfile = file,
        files   = c(basename(csv_path), basename(readme_path), included_ref_files),
        root    = tmp_dir
      )
    }
  )
  
  output$download_full <- downloadHandler(
    filename = function() {
      prefix <- if (input$dep_direction == "import") "GeoDep_M" else "GeoDep_X"
      paste0(prefix, "_full_", Sys.Date(), ".zip")
    },
    contentType = "application/zip",
    content = function(file) {
      withProgress(message = "Preparing full dataset", value = 0, {
        
        direction <- input$dep_direction
        prefix    <- if (direction == "import") "GeoDep_M" else "GeoDep_X"
        
        incProgress(0.1, detail = "Loading data")
        
        imports_final <- imports_final |>
          select(-sect_strategic,-Description)
        
        exports_final <- exports_final |>
          select(-Description)
        
        full_data <- if (direction == "import") imports_final else exports_final
        
        zip_filename <- paste0(prefix, "_full_", Sys.Date(), ".zip")
        
        tmp_dir <- tempfile("geodep_full_export_")
        dir.create(tmp_dir)
        on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)
        
        csv_name <- paste0(prefix, "_full.csv")
        csv_path <- file.path(tmp_dir, csv_name)
        
        incProgress(0.2, detail = "Writing CSV")
        write_excel_csv2(full_data, csv_path)
        included_ref_files <- copy_reference_files(tmp_dir)
        
        size_kb_unzipped <- format_kb(file.info(csv_path)$size)
        
        incProgress(0.2, detail = "Generating README")
        readme_path <- file.path(tmp_dir, "README.txt")
        
        readme_text_draft <- generate_readme(
          direction        = direction,
          selection        = character(),
          sector_filter    = "all",
          map_metric       = input$map_metric,
          zip_filename     = zip_filename,
          size_kb_unzipped = size_kb_unzipped,
          size_kb_zipped   = NULL,
          is_full          = TRUE
        )
        writeLines(readme_text_draft, readme_path, useBytes = TRUE)
        
        incProgress(0.2, detail = "Compressing (pass 1/2)")
        tmp_zip_path <- file.path(tmp_dir, "__probe.zip")
        zip::zip(
          zipfile = tmp_zip_path,
          files   = c(basename(csv_path), basename(readme_path), included_ref_files),
          root    = tmp_dir
        )
        size_kb_zipped <- format_kb(file.info(tmp_zip_path)$size)
        file.remove(tmp_zip_path)
        
        readme_text_final <- generate_readme(
          direction        = direction,
          selection        = character(),
          sector_filter    = "all",
          map_metric       = input$map_metric,
          zip_filename     = zip_filename,
          size_kb_unzipped = size_kb_unzipped,
          size_kb_zipped   = size_kb_zipped,
          is_full          = TRUE
        )
        writeLines(readme_text_final, readme_path, useBytes = TRUE)
        
        incProgress(0.2, detail = "Compressing (pass 2/2)")
        zip::zip(
          zipfile = file,
          files   = c(basename(csv_path), basename(readme_path), included_ref_files),
          root    = tmp_dir
        )
        
        incProgress(0.1, detail = "Done")
      })
    }
  )
}

shinyApp(ui = ui, server = server)

