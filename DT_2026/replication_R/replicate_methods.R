# Run from this folder (source("replicate_methods.R") is relative). Exploratory, see
# ../replication_methodes_2026-10-30.md; not part of the Stata pipeline.
# Exploratory: apply the identification criteria of other studies to BACI HS22,
# EU27 as one importer (extra-EU flows), 2024. Not part of the pipeline.
suppressMessages({library(data.table); library(haven)})
db <- paste0(Sys.getenv("USERPROFILE"), "/CIREM Dropbox/kévin LEFEBVRE/Dependence/01_GeoDep_database/Data")
d <- as.data.table(read_dta(file.path(db, "temp/imports_hs22.dta")))
d[, year := as.integer(year)]

build <- function(y) {
  x <- d[year == y]
  extra <- x[iso_d == "EUN" & iso_o != "EUN"]
  p <- extra[, .(Mx = sum(imports),
                 hhi_m = sum((imports / sum(imports))^2),
                 top_o = iso_o[order(-imports, iso_o)][1],
                 top_sh = max(imports) / sum(imports),
                 M_chn = sum(imports[iso_o == "CHN"])), by = hs6]
  p <- merge(p, x[iso_o == "EUN" & iso_d == "EUN", .(Mi = sum(imports)), by = hs6], all.x = TRUE)
  p <- merge(p, x[iso_o == "EUN" & iso_d != "EUN", .(Xx = sum(imports)), by = hs6], all.x = TRUE)
  # world export HHI, EU as one exporter, no intra-EU flows (as GeoDep C2)
  w <- x[iso_o != iso_d, .(X = sum(imports)), by = .(hs6, iso_o)]
  w <- w[, .(hhi_w = sum((X / sum(X))^2)), by = hs6]
  p <- merge(p, w, all.x = TRUE)
  for (v in c("Mi", "Xx")) set(p, which(is.na(p[[v]])), v, 0)
  p[, Xt := Xx + Mi]                      # total EU exports = extra + intra
  p[, `:=`(
    ec    = hhi_m > 0.4 & Mx / (Mx + Mi) > 0.5 & Mx / Xt > 1,
    merics = Mx >= 2 * Xx & top_sh >= 0.3 & hhi_w >= 0.25,
    bonnet_tr = hhi_m > 0.4 & Mx / (Mx + Xx + Mi) > 0.4,
    bn_jm = Mx / (Mx + Mi) > 0.5 & hhi_m > 0.5,
    baur = hhi_m > 0.33 & Mx / Xx > 1,
    geodep_t = hhi_m > 0.4 & hhi_w > 0.4 & Mx / Xx > 1)]
  p[, year := y]
  list(p = p, extra = extra)
}
yrs <- lapply(2022:2024, build)
p <- yrs[[3]]$p; extra <- yrs[[3]]$extra

# EC with GeoDep persistence (2 of the 3 years, and in 2024)
ecp <- rbindlist(lapply(yrs, function(z) z$p[, .(hs6, year, ec)]))
ecp <- ecp[, .(n = sum(ec)), by = hs6]
p <- merge(p, ecp, all.x = TRUE)
p[, ec_pers := ec & n >= 2]

# GeoDep, from the public database
g <- as.data.table(read_dta(file.path(db, "final/geodep_M_hs22.dta")))
g <- g[iso_d == "EUN" & year == 2024 & dependent_M == 1, .(hs6, g_first = first_o)]
p[, geodep := hs6 %in% g$hs6]

N <- nrow(p); Mtot <- sum(p$Mx)
summ <- function(flag, label) {
  s <- p[get(flag) == TRUE]
  data.table(method = label, n = nrow(s), pct_products = 100 * nrow(s) / N,
             pct_value = 100 * sum(s$Mx) / Mtot,
             chn_count = 100 * mean(s$top_o == "CHN"),
             chn_value = 100 * sum(s$M_chn) / sum(s$Mx),
             common_geodep = sum(s$geodep))
}
out <- rbindlist(list(
  summ("geodep", "GeoDep (public database)"),
  summ("geodep_t", "GeoDep criteria, 2024 only"),
  summ("ec", "Commission 2021 (no ecosystem filter)"),
  summ("ec_pers", "Commission 2021 + persistence 2/3"),
  summ("bonnet_tr", "Bonnet 2026, trade-based exposure"),
  summ("merics", "MERICS (Chimits 2024)"),
  summ("bn_jm", "Bonneau-Nakaa / Jaravel-Mejean, partial"),
  summ("baur", "Baur-Flach, partial")))

# Arriola et al.: bilateral links, HHI >= 0.2 and partner share >= 0.1
ex <- merge(extra, p[, .(hs6, Mx, hhi_m)], by = "hs6")
links <- ex[hhi_m >= 0.2 & imports / Mx >= 0.1]
out <- rbind(out, data.table(method = "Arriola 2024 (bilateral links)",
  n = uniqueN(links$hs6), pct_products = 100 * uniqueN(links$hs6) / N,
  pct_value = 100 * sum(p[hs6 %in% links$hs6]$Mx) / Mtot,
  chn_count = 100 * mean(links$iso_o == "CHN"),
  chn_value = 100 * sum(links[iso_o == "CHN"]$imports) / sum(links$imports),
  common_geodep = sum(unique(links$hs6) %in% g$hs6)))

cat("EU27 extra-EU imported products in 2024:", N, "\n")
cat("China share of all extra-EU imports: count", round(100 * mean(p$top_o == "CHN"), 1),
    "value", round(100 * sum(p$M_chn) / Mtot, 1), "\n")
print(out[, lapply(.SD, function(v) if (is.numeric(v)) round(v, 1) else v)])
cat("Check GeoDep: public n =", nrow(g), " China first =", sum(g$g_first == "CHN"), "\n")

# China as main supplier, value of the whole product (MERICS-style measure)
chn_main_val <- sapply(c("geodep","geodep_t","ec","ec_pers","bonnet_tr","merics","bn_jm","baur"),
  function(f) { s <- p[get(f) == TRUE]; round(100 * sum(s[top_o == "CHN"]$Mx) / sum(s$Mx), 1) })
print(chn_main_val)
gg <- as.data.table(read_dta(file.path(db, "final/geodep_M_hs22.dta")))[iso_d == "EUN" & year == 2024]
cat("GeoDep EUN rows 2024:", nrow(gg), " with import>0:", sum(gg$import_dpt > 0), "\n")
# MERICS 2-year smoothing variant: criteria on 2023+2024 pooled values
