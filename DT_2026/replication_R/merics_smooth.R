source("replicate_methods.R", echo = FALSE)
x <- d[year %in% 2023:2024]
ex <- x[iso_d == "EUN" & iso_o != "EUN", .(v = sum(imports)), by = .(hs6, iso_o)]
s <- ex[, .(Mx2 = sum(v), top2 = max(v) / sum(v)), by = hs6]
s <- merge(s, x[iso_o == "EUN" & iso_d != "EUN", .(Xx2 = sum(imports)), by = hs6], all.x = TRUE)
s[is.na(Xx2), Xx2 := 0]
w <- x[iso_o != iso_d, .(X = sum(imports)), by = .(hs6, iso_o)][, .(hw2 = sum((X / sum(X))^2)), by = hs6]
s <- merge(s, w, by = "hs6", all.x = TRUE)
p <- merge(p, s, by = "hs6", all.x = TRUE)
p[, merics_pool := !is.na(Mx2) & Mx2 >= 2 * Xx2 & top2 >= 0.3 & hw2 >= 0.25]
m23 <- yrs[[2]]$p[, .(hs6, m23 = merics)]
p <- merge(p, m23, by = "hs6", all.x = TRUE)
p[, merics_both := merics & !is.na(m23) & m23]
print(rbind(summ("merics", "2024 only"), summ("merics_pool", "2023-24 pooled"), summ("merics_both", "both 2023 and 2024")))
for (f in c("merics","merics_pool","merics_both")) cat(f, "(c):", p[get(f)==TRUE, round(100*sum(Mx[top_o=="CHN"])/sum(Mx),1)], "\n")
cat("2024-only & pooled common:", p[merics & merics_pool, .N], "\n")
