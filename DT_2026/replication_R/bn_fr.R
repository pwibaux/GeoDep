suppressMessages(library(data.table))
raw <- paste0(Sys.getenv("USERPROFILE"), "/CIREM Dropbox/kévin LEFEBVRE/Dependence/01_GeoDep_database/Data/raw/BACI_HS22")
b <- fread(file.path(raw, "BACI_HS22_Y2024_V202601.csv"), colClasses = list(character = "k"))
cc <- fread(file.path(raw, "country_codes_V202601.csv"))
b <- merge(b, cc[, .(i = country_code, io = country_iso3)], by = "i")
b <- merge(b, cc[, .(j = country_code, jo = country_iso3)], by = "j")
eu27 <- c("DEU","AUT","BEL","BGR","CYP","HRV","DNK","ESP","EST","FIN","FRA","GRC","HUN","IRL","ITA","LVA","LTU","LUX","MLT","NLD","POL","PRT","ROU","SVK","SVN","SWE","CZE")
f <- b[jo == "FRA"]
fr <- f[, .(Mtot = sum(v), Mx = sum(v[!io %in% eu27]),
            hhi_all = sum((v/sum(v))^2)), by = k]
fh <- f[!io %in% eu27, .(hhi_x = sum((v/sum(v))^2)), by = k]
fr <- merge(fr, fh, by = "k", all.x = TRUE)
# EU-level extra share
e <- b[jo %in% eu27, .(eu_ext = sum(v[!io %in% eu27]) / sum(v)), by = k]
fr <- merge(fr, e, by = "k", all.x = TRUE)
cat("FR extra share > .5:", fr[Mx/Mtot > .5, .N], "\n")
cat("A FR extra>.5 & HHI extra>.5:", fr[Mx/Mtot > .5 & hhi_x > .5, .N], "\n")
cat("B FR extra>.5 & HHI all partners>.5:", fr[Mx/Mtot > .5 & hhi_all > .5, .N], "\n")
cat("C EU extra>.5 & FR HHI extra>.5:", fr[eu_ext > .5 & hhi_x > .5, .N], "\n")
cat("D FR extra>.5 & HHI extra>.5 & Mx > 1M USD:", fr[Mx/Mtot > .5 & hhi_x > .5 & Mx > 1000, .N], "\n")
cat("value share A:", fr[Mx/Mtot > .5 & hhi_x > .5, round(100*sum(Mtot),0)]/fr[,sum(Mtot)], "\n")
