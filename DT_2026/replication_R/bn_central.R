source("replicate_methods.R", echo = FALSE)
raw <- paste0(Sys.getenv("USERPROFILE"), "/CIREM Dropbox/kévin LEFEBVRE/Dependence/01_GeoDep_database/Data/raw/BACI_HS22")
b <- fread(file.path(raw, "BACI_HS22_Y2024_V202601.csv"), colClasses = list(character = "k"))
cc <- fread(file.path(raw, "country_codes_V202601.csv"))
b <- merge(b, cc[, .(i = country_code, io = country_iso3)], by = "i")
b <- merge(b, cc[, .(j = country_code, jo = country_iso3)], by = "j")
b[, hs6 := formatC(as.integer(k), width = 6, flag = "0")]
b <- b[!(hs6 %in% c("271111","271121","271000")) & substr(hs6,1,2) != "93" & io != jo]
# Korniyenko et al. (2017): weighted outdegree centrality, SD across nodes, z-score across products
b[, wbar := sum(v) / .N, by = .(hs6, jo)]           # average import value per supplier of j
ci <- b[, .(c = sum(v / wbar)), by = .(hs6, io)]
nodes <- unique(rbind(b[, .(hs6, n = io)], b[, .(hs6, n = jo)]))
nodes <- merge(nodes, ci, by.x = c("hs6","n"), by.y = c("hs6","io"), all.x = TRUE)
nodes[is.na(c), c := 0]
cent <- nodes[, .(cent = sd(c)), by = hs6]
cent[, z := (cent - mean(cent)) / sd(cent)]
top <- b[, .(X = sum(v)), by = .(hs6, io)][, .(top_w = max(X) / sum(X)), by = hs6]
cent <- merge(cent, top, by = "hs6")
cat("raw centrality quantiles:\n"); print(round(quantile(cent$cent, c(.5,.9,.95,.99)),2))
cat("share of products raw>2.5:", round(100*mean(cent$cent>2.5),1), " z>2.5:", round(100*mean(cent$z>2.5),1), "\n")
cat("median top exporter world share, raw in [2.3,2.7]:", cent[cent>2.3 & cent<2.7, round(median(top_w),2)],
    " z in [2.3,2.7]:", cent[z>2.3 & z<2.7, round(median(top_w),2)], "\n")
p <- merge(p, cent[, .(hs6, cent, z)], by = "hs6", all.x = TRUE)
p[, `:=`(bn_raw = Mx/(Mx+Mi) > 0.5 & hhi_m > 0.5 & cent > 2.5,
         bn_z   = Mx/(Mx+Mi) > 0.5 & hhi_m > 0.5 & z > 2.5)]
cat("\nEU:\n"); print(rbind(summ("bn_jm","B-N without centrality"), summ("bn_raw","B-N, raw > 2.5"), summ("bn_z","B-N, z > 2.5")))
for (f in c("bn_raw","bn_z")) cat(f,"(c):", p[get(f)==TRUE, round(100*sum(Mx[top_o=="CHN"])/sum(Mx),1)], "\n")
# France
eu27 <- c("DEU","AUT","BEL","BGR","CYP","HRV","DNK","ESP","EST","FIN","FRA","GRC","HUN","IRL","ITA","LVA","LTU","LUX","MLT","NLD","POL","PRT","ROU","SVK","SVN","SWE","CZE")
f <- b[jo == "FRA"]
fr <- f[, .(Mtot = sum(v), Mx = sum(v[!io %in% eu27])), by = hs6]
fh <- f[!io %in% eu27, .(hhi = sum((v/sum(v))^2), top_o = io[which.max(v)]), by = hs6]
fr <- merge(fr, fh, by = "hs6"); fr <- merge(fr, cent[, .(hs6, cent, z)], by = "hs6", all.x = TRUE)
fr[, conc := Mx/Mtot > 0.5 & hhi > 0.5]
cat("\nFrance: products", nrow(fr), " concentrated (B-N 121 in 2018):", fr[conc==TRUE,.N],
    " vulnerable raw:", fr[conc & cent>2.5,.N], " vulnerable z (B-N 12 in 2018):", fr[conc & z>2.5,.N],
    " China 1st among z-vulnerable:", fr[conc & z>2.5, sum(top_o=="CHN")], "\n")
fwrite(cent, "centrality_2024.csv")
