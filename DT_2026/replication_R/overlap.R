source("replicate_methods.R", echo = FALSE)
p[, `:=`(ec2 = Mx / (Mx + Mi) > 0.5, ec3 = Mx / Xt > 1, c2 = hhi_w > 0.4, mx2 = Mx >= 2 * Xx)]
cat("\n== GeoDep not in EC:", p[geodep & !ec, .N], "\n")
print(p[geodep & !ec, .N, by = .(fail_extra_share = !ec2, fail_ratio_total_exports = !ec3)])
cat("intra-EU imports / total EU imports, median, for those:", p[geodep & !ec, round(median(Mi/(Mx+Mi)),2)], "\n")
cat("\n== EC not in GeoDep:", p[ec & !geodep, .N], "\n")
print(p[ec & !geodep, .N, by = .(fail_world_hhi = !c2, geodep_2024_only = geodep_t)])
cat("China 1st supplier share: EC&GeoDep", p[ec & geodep, round(100*mean(top_o=="CHN"),1)],
    " EC only", p[ec & !geodep, round(100*mean(top_o=="CHN"),1)],
    " GeoDep only", p[geodep & !ec, round(100*mean(top_o=="CHN"),1)], "\n")
cat("\n== GeoDep not in MERICS:", p[geodep & !merics, .N], " fail M>=2X:", p[geodep & !merics & !mx2, .N], "\n")
cat("== MERICS not in GeoDep:", p[merics & !geodep, .N], " world HHI in [0.25,0.4]:", p[merics & !geodep & hhi_w <= 0.4, .N],
    " China 1st among those:", p[merics & !geodep, round(100*mean(top_o=="CHN"),1)], "\n")
cat("== Bonnet-trade not in GeoDep:", p[bonnet_tr & !geodep, .N], " fail world HHI:", p[bonnet_tr & !geodep & !c2, .N], "\n")
cat("== GeoDep not in Bonnet-trade:", p[geodep & !bonnet_tr, .N], "\n")
