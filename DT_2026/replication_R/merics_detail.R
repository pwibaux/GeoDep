source("replicate_methods.R", echo = FALSE)
p[, `:=`(m1 = Mx >= 2 * Xx, m2 = top_sh >= 0.3, m3 = !is.na(hhi_w) & hhi_w >= 0.25)]
cat("\nUniverse:", N, "\nC1 M>=2X:", p[m1==TRUE,.N], " C2 top>=30%:", p[m2==TRUE,.N], " C3 world HHI>=.25:", p[m3==TRUE,.N], "\n")
cat("C1&C2:", p[m1&m2,.N], " C1&C3:", p[m1&m3,.N], " C2&C3:", p[m2&m3,.N], " all:", p[m1&m2&m3,.N], "\n")
s <- p[merics == TRUE]
cat("\nMain suppliers (count):\n"); print(head(s[, .N, by = top_o][order(-N)], 8))
desc <- fread(paste0(Sys.getenv("USERPROFILE"), "/CIREM Dropbox/kévin LEFEBVRE/Dependence/01_GeoDep_database/Data/raw/BACI_HS22/product_codes_HS22_V202601.csv"), colClasses = "character")
setnames(desc, 1:2, c("hs6","label"))
s <- merge(s, desc, by = "hs6", all.x = TRUE)
cat("\nHS chapters (top 10 by count):\n"); print(head(s[, .N, by = .(ch = substr(hs6,1,2))][order(-N)], 10))
cat("\nTop 12 by extra-EU import value (bn USD):\n")
print(s[order(-Mx)][1:12, .(hs6, label = substr(label,1,55), M_bn = round(Mx/1e6,1), top_o, top_sh = round(top_sh,2), hhi_w = round(hhi_w,2), geodep)])
cat("\nMedian extra-EU import value (m USD):", round(median(s$Mx)/1e3,1), "\n")
