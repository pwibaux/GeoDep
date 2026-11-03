source("replicate_methods.R", echo = FALSE)
q <- yrs[[2]]$p   # 2023
q[, tex := Mx / (Mx + Xx + Mi)]
thr <- (0.4 + 0.0186) / 1.287
cat("trade-exposure threshold implied by their imputation:", round(thr, 3), "\n")
f <- function(cond, lab) { s <- q[eval(cond)]
  cat(sprintf("%-55s n=%4d  China 1st=%4d (%.1f%%)\n", lab, nrow(s), sum(s$top_o=="CHN"), 100*mean(s$top_o=="CHN"))) }
cat("\n== 2023 (Bonnet's year). Published, with Prodcom: 835 products, China 394 (47%)\n")
f(quote(hhi_m > 0.4 & tex > 0.4), "Bonnet, trade exposure > 0.4")
f(quote(hhi_m > 0.4 & tex > 0.4 & Mx > 10.8), "  + drop imports < 10,000 EUR")
f(quote(hhi_m > 0.4 & tex > thr & Mx > 10.8), "Bonnet, imputed exposure > 0.4 (trade exp. > 0.325)")
cat("\n== EC 2021 criteria, 2023. Published by Bonnet (fn 33), with production in C3: 339\n")
f(quote(ec == TRUE), "EC 2021, total EU exports in C3 (trade only)")
