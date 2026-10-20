## =============================================================================
## GEODEP - DATA PREPARATION FOR THE SHINY APP
## Builds 1_Data/geodep_shiny_inputs.rds, the single data file read by app.R.
## Run update_data.R first: it copies the GeoDep datasets from Dropbox into 1_Data/.
## =============================================================================

library(here)
library(dplyr)
library(readxl)
library(countrycode)
library(rnaturalearth)
library(sf)

here::i_am('geodep_shiny.Rproj')

DEP_YEAR <- 2024

## -----------------------------------------------------------------------
## 0. HS6 product descriptions
## -----------------------------------------------------------------------

hs6_description <- read_excel("1_Data/hs22_hs6.xlsx")
colnames(hs6_description)[2] <- "hs6"

## -----------------------------------------------------------------------
## 1. Imports (GeoDep_M)
## -----------------------------------------------------------------------
## One row per (importer, HS6): the destination's total import value (import_dpt),
## the four dependency criteria (c1_M to c4_M), the dependent flag, the identity
## and share of the leading supplier (first_odpt, share_odpt), and the sect_* flags.
## 1_Data/geodep_M_hs22.rds is written by update_data.R (copy of the Dropbox .dta)

imports_final <- readRDS("1_Data/geodep_M_hs22.rds") |>
  filter(year == DEP_YEAR) |>
  left_join(hs6_description, by = "hs6") |>
  select(-Classification,-`Parent Code`, -Level,-IsBasicLevel) |>
  mutate(sect_strategic = if_else(sect_crm == 1 | sect_dual_use == 1 | sect_health == 1 | sect_agrifood == 1 | sect_energy == 1, 1, 0)
    )

## -----------------------------------------------------------------------
## 2. Exports (GeoDep_X)
## -----------------------------------------------------------------------
## The symmetric export-side table: one row per (exporter, HS6), with export_opt,
## c1_X to c4_X, dependent, and the leading destination's identity and share
## (first_dpto, share_dpto). No sector columns.
## 1_Data/geodep_X_hs22.rds is written by update_data.R (copy of the Dropbox .dta)

exports_final <- readRDS("1_Data/geodep_X_hs22.rds") |>
  filter(year == DEP_YEAR) |>
  left_join(hs6_description, by = "hs6") |>
  select(-Classification,-`Parent Code`, -Level,-IsBasicLevel)

## -----------------------------------------------------------------------
## 3. Precomputation for the app
## -----------------------------------------------------------------------

eu_countries <- c("DEU", "AUT", "BEL", "BGR", "CYP", "HRV", "DNK", "ESP",
                  "EST", "FIN", "FRA", "GRC", "HUN", "IRL", "ITA", "LVA",
                  "LTU", "LUX", "MLT", "NLD", "POL", "PRT", "ROU", "SVK",
                  "SVN", "SWE", "CZE")

to_eun <- function(iso3) {
  if_else(iso3 %in% eu_countries, "EUN", iso3)
}

## World map polygons
world_polygons <- ne_countries(scale = "medium", returnclass = "sf") |>
  mutate(
    iso_a3 = case_when(
      !is.na(iso_a3)    & iso_a3    != "-99" & iso_a3    != "" ~ iso_a3,
      !is.na(iso_a3_eh) & iso_a3_eh != "-99" & iso_a3_eh != "" ~ iso_a3_eh,
      !is.na(adm0_a3)   & adm0_a3   != "-99" & adm0_a3   != "" ~ adm0_a3,
      TRUE ~ countrycode(name_long, origin = "country.name", destination = "iso3c")
    )
  ) |>
  filter(!is.na(iso_a3))

old_s2 <- sf::sf_use_s2()
sf::sf_use_s2(FALSE)

world_polygons <- world_polygons |>
  sf::st_wrap_dateline(options = c("WRAPDATELINE=YES", "DATELINEOFFSET=180")) |>
  sf::st_make_valid()

sf::sf_use_s2(old_s2)

iso_name_lookup <- world_polygons |>
  sf::st_drop_geometry() |>
  group_by(iso_a3) |>
  slice(1) |>
  ungroup() |>
  select(iso_a3, name) |>
  bind_rows(tibble(iso_a3 = "EUN", name = "European Union"))

## Countries whose outline crosses the 180th meridian (Russia, Fiji...) jump from
## +180 to -180 inside one ring, which draws a line across the whole map.
## Split them cleanly at the meridian (done here once, rather than at each app start).
fix_antimeridian <- function(x) {
  old_s2 <- suppressMessages(sf::sf_use_s2(FALSE))
  on.exit(suppressMessages(sf::sf_use_s2(old_s2)))
  geoms <- lapply(st_geometry(x), function(g) {
    polys <- if (inherits(g, "MULTIPOLYGON")) unclass(g) else list(unclass(g))
    polys <- lapply(polys, function(p) {
      crosses <- any(vapply(p, function(r) diff(range(r[, 1])) > 180, logical(1)))
      if (crosses) lapply(p, function(r) { neg <- r[, 1] < 0; r[neg, 1] <- r[neg, 1] + 360; r }) else p
    })
    st_multipolygon(polys)
  })
  st_geometry(x) <- st_sfc(geoms, crs = st_crs(x))
  st_wrap_dateline(x, options = c("WRAPDATELINE=YES", "DATELINEOFFSET=180"), quiet = TRUE)
}

## Antarctica takes a lot of room on the map and carries no data
world_polygons <- world_polygons |>
  filter(iso_a3 != "ATA") |>
  fix_antimeridian()

## Country menu: only countries that appear in the trade data
trade_isos <- c(
  imports_final$iso_d,
  imports_final$first_odpt,
  exports_final$iso_o,
  exports_final$first_dpto
) |>
  unique() |>
  na.omit() |>
  to_eun() |>
  unique()

country_choices_lookup <- iso_name_lookup |>
  filter(!iso_a3 %in% eu_countries) |>
  filter(iso_a3 %in% trade_isos)

country_choices <- setNames(country_choices_lookup$iso_a3, country_choices_lookup$name)
country_choices <- country_choices[order(names(country_choices))]
country_choices_ui <- c("Select a country..." = "", country_choices)

## Dependent products, with the leading partner
dep_import_base <- imports_final |>
  filter(dependent == 1) |>
  mutate(
    iso_d   = to_eun(iso_d),
    iso_o   = to_eun(first_odpt),
    imports = import_dpt * share_odpt / 100
  ) |>
  select(iso_d, iso_o, hs6, imports, import_dpt,
         Description, starts_with("sect_"), first_odpt)

dep_export_base <- exports_final |>
  filter(dependent == 1) |>
  mutate(
    iso_o   = to_eun(iso_o),
    iso_d   = to_eun(first_dpto),
    imports = export_opt * share_dpto / 100
  ) |>
  select(iso_o, iso_d, hs6, Description, imports, export_opt, first_dpto)

## All traded products per country (denominators of the map shares)
traded_by_country <- imports_final |>
  mutate(iso_d = to_eun(iso_d)) |>
  select(iso_d, hs6, dependant_M_MC_t = dependent, c4_M_MC = c4_M,
         import_dpt, starts_with("sect_"))

traded_by_country_export <- exports_final |>
  mutate(iso_o = to_eun(iso_o)) |>
  select(iso_o, hs6, dependant_X_MC_t = dependent, c4_X_MC = c4_X,
         export_opt)

## Countries (EU = EUN) with trade data in each direction: on a partner map, these
## are shown as 0% rather than "No data" when they lead none of the dependencies
countries_with_data_import <- sort(unique(traded_by_country$iso_d))
countries_with_data_export <- sort(unique(traded_by_country_export$iso_o))

## -----------------------------------------------------------------------
## 4. Save everything the app needs
## -----------------------------------------------------------------------

geodep_inputs <- list(
  eu_countries             = eu_countries,
  world_polygons           = world_polygons,
  iso_name_lookup          = iso_name_lookup,
  country_choices          = country_choices,
  country_choices_ui       = country_choices_ui,
  dep_import_base          = dep_import_base,
  dep_export_base          = dep_export_base,
  traded_by_country        = traded_by_country,
  traded_by_country_export = traded_by_country_export,
  countries_with_data_import = countries_with_data_import,
  countries_with_data_export = countries_with_data_export,
  imports_final            = imports_final,
  exports_final            = exports_final
)

saveRDS(geodep_inputs, "1_Data/geodep_shiny_inputs.rds")

cat("Precomputation done. Saved 1_Data/geodep_shiny_inputs.rds",
    "(", format(object.size(geodep_inputs), units = "MB"), ")\n")
