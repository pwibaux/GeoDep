source("replicate_methods.R", echo = FALSE)
p[, bonnet_imp := hhi_m > 0.4 & Mx / (Mx + Xx + Mi) > (0.4 + 0.0186) / 1.287]
print(summ("bonnet_imp", "Bonnet 2026, trade exp. > 0.325"))
cat("(c):", p[bonnet_imp == TRUE, round(100*sum(Mx[top_o=="CHN"])/sum(Mx),1)], "\n")
