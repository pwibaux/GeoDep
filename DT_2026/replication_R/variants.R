source("replicate_methods.R", echo = FALSE)
raw <- paste0(Sys.getenv("USERPROFILE"), "/CIREM Dropbox/kévin LEFEBVRE/Dependence/01_GeoDep_database/Data/raw/BACI_HS22")
b <- fread(file.path(raw, "BACI_HS22_Y2024_V202601.csv"), colClasses = list(character = "k"))
cc <- fread(file.path(raw, "country_codes_V202601.csv"))
eu27 <- c("DEU","AUT","BEL","BGR","CYP","HRV","DNK","ESP","EST","FIN","FRA","GRC","HUN","IRL","ITA","LVA","LTU","LUX","MLT","NLD","POL","PRT","ROU","SVK","SVN","SWE","CZE")
b[, k := sprintf("%06s", k)][, k := gsub(" ", "0", k)]
b <- merge(b, cc[, .(i = country_code, io = country_iso3)], by = "i")
b <- merge(b, cc[, .(j = country_code, jo = country_iso3)], by = "j")
b <- b[!(io %in% eu27 & jo %in% eu27)]   # no intra-EU flows, EU members kept separate
w2 <- b[, .(X = sum(v)), by = .(k, io)][, .(hhi_w_sep = sum((X / sum(X))^2)), by = .(hs6 = k)]
p <- merge(p, w2, by = "hs6", all.x = TRUE)
p[, merics_sep := Mx >= 2 * Xx & top_sh >= 0.3 & hhi_w_sep >= 0.25]
cat("\n== MERICS world HHI variants\n")
print(rbind(summ("merics", "EU one exporter"), summ("merics_sep", "EU members separate")))
cat("both:", p[merics & merics_sep, .N], "\n")
cat("China (c) sep:", p[merics_sep == TRUE, round(100*sum(Mx[top_o=="CHN"])/sum(Mx),1)], "\n")

# Arriola: product level, is the bilateral share criterion ever binding?
cat("\n== Arriola\nproducts with import HHI >= 0.2:", p[hhi_m >= 0.2, .N],
    " with at least one link:", uniqueN(links$hs6), "\n")
# 3-year average version (2022-2024)
ex3 <- d[iso_d == "EUN" & iso_o != "EUN", .(v = sum(imports) / 3), by = .(hs6, iso_o)]
ex3[, `:=`(sh = v / sum(v), hhi = sum((v / sum(v))^2)), by = hs6]
l3 <- ex3[hhi >= 0.2 & sh >= 0.1]
cat("3-yr avg: links", nrow(l3), " products", uniqueN(l3$hs6), " China links % count",
    round(100*mean(l3$iso_o=="CHN"),1), " value", round(100*sum(l3[iso_o=="CHN"]$v)/sum(l3$v),1), "\n")
cat("1-yr 2024: links", nrow(links), "\n")
# 'excessive' concentration: import HHI > 2 x world HHI, world HHI >= 0.2, import HHI >= 0.4
p[, arr_exc := hhi_m > 2 * hhi_w & hhi_w >= 0.2 & hhi_m >= 0.4]
print(summ("arr_exc", "Arriola 'excessive concentration'"))
cat("China (c):", p[arr_exc == TRUE, round(100*sum(Mx[top_o=="CHN"])/sum(Mx),1)], "\n")
