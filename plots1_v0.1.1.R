




m1_col = "#018571"
m2_col = "#80cdc1"
m3_col = "#dfc27d"
m4_col = "#a6611a"

name1 = "A: European insects"
name2 = "B: Riverine fishes"
name3 = "C: Living Planet"
name4 = "D: UK insects"
name5 = "E: UK freshwater fishes"
name6 = "F: North American birds"
name7 = "G: German insects"
name8 = "H: BioTIME"
name9 = "I: Marine fishes"
name10 = "J: Large carnivores"


projections_cmb = NULL
for(a in c(1:10)){
  code = unique(df_cmb4$code)[which(!is.na(unique(df_cmb4$code)))][a]
  message(code)
  df_tmp = subset(df_cmb, code == unique(df_cmb$code)[a])
  df_tmp4 = subset(df_cmb4, code == unique(df_cmb$code)[a])
  
  run = length(min(df_tmp4$year):max(df_tmp4$year))-1
  y1 = c(100)
  y2 = c(100)
  y3 = c(100)
  for(tim in 1:(run)){
    y1 = c(y1,y1[tim]*(1 + df_tmp$coef[1]))
    y2 = c(y2,y2[tim]*(1 + df_tmp$coef[2]))
    y3 = c(y3,y3[tim]*(1 + df_tmp$coef[3]))
  }
  
  
  lc1 = c(100)
  lc2 = c(100)
  lc3 = c(100)
  for(tim in 1:(run)){
    lc1 = c(lc1,lc1[tim]*(1 + df_tmp$coef_lc5[1]))
    lc2 = c(lc2,lc2[tim]*(1 + df_tmp$coef_lc5[2]))
    lc3 = c(lc3,lc3[tim]*(1 + df_tmp$coef_lc5[3]))
  }
  
  
  uc1 = c(100)
  uc2 = c(100)
  uc3 = c(100)
  for(tim in 1:(run)){
    uc1 = c(uc1,uc1[tim]*(1 + df_tmp$coef_uc5[1]))
    uc2 = c(uc2,uc2[tim]*(1 + df_tmp$coef_uc5[2]))
    uc3 = c(uc3,uc3[tim]*(1 + df_tmp$coef_uc5[3]))
  }
  
  projections_tmp = data.frame(
    code = code,
    year = c(min(df_tmp4$year):max(df_tmp4$year)),
    y1 = y1, y2 = y2, y3 = y3,
    lc1 = lc1, lc2 = lc2, lc3 = lc3,
    uc1 = uc1, uc2 = uc2, uc3 = uc3)
  projections_cmb = rbind(projections_cmb, projections_tmp)
}


sum_df_cmb2_rev = data.frame(
  m = (c(1,1,2,2)),
  status = rep(c("a","b"),2),
  prop = c(
    10 - sum(df_cmb2$rev3vs1),
    sum(df_cmb2$rev3vs1),
    10 - sum(df_cmb2$rev3vs2),
    sum(df_cmb2$rev3vs2)
  )
)

rev_plt = ggplot() +
  geom_bar(data = sum_df_cmb2_rev, aes(x = reorder(m, -m), y = prop, fill = status), stat='identity') +
  scale_fill_manual(guide = F, values = c("grey25", "grey65")) +
  scale_y_continuous(breaks = c(0,2,4,6,8,10)) +
  scale_x_discrete(labels=c("1" = "3 vs 1", "2" = "3 vs 2")) +
  labs(x = " ", y = " ") +
  coord_flip() +
  theme_classic()


sum_df_cmb2_stre = data.frame(
  m = (c(1,1,2,2)),
  status = rep(c("a","b"),2),
  prop = c(
    sum(df_cmb2$stre3vs1),
    10 - sum(df_cmb2$stre3vs1),
    sum(df_cmb2$stre3vs2),
    10 - sum(df_cmb2$stre3vs2)
  )
)

stre_plt2 = ggplot() +
  geom_bar(data = sum_df_cmb2_stre, aes(x = reorder(m, -m), y = prop, fill = status), stat='identity', alpha =0.6) +
  scale_fill_manual(guide = F, values = c("steelblue4", "darkcyan")) +
  scale_y_continuous(breaks = c(0,2,4,6,8,10)) +
  scale_x_discrete(labels=c("1" = "3 vs 1", "2" = "3 vs 2")) +
  labs(x = " ", y = " ") +
  coord_flip() +
  theme_classic()


sum_df_cmb2_var = data.frame(
  m = (c(1,1,2,2)),
  status = rep(c("a","b"),2),
  prop = c(
    sum(df_cmb2$var3vs1),
    10 - sum(df_cmb2$var3vs1),
    sum(df_cmb2$var3vs2),
    10 - sum(df_cmb2$var3vs2)
  )
)

var_plt2 = ggplot() +
  geom_bar(data = sum_df_cmb2_var, aes(x = reorder(m, -m), y = prop, fill = status), stat='identity', alpha =0.8) +
  scale_fill_manual(guide = F, values = c("darkgoldenrod4", "orange3")) +
  scale_y_continuous(breaks = c(0,2,4,6,8,10)) +
  scale_x_discrete(labels=c("1" = "3 vs 1", "2" = "3 vs 2")) +
  labs(x = " ", y = "Datasets (N)") +
  coord_flip() +
  theme_classic()



sum_df_cmb2_stre = data.frame(
  m = as.character(c(rep(1,10),rep(2,10))),
  diff = log(abs(c(df_cmb2$mn_change_3vs1, df_cmb2$mn_change_3vs2)))
)

stre_m = lm(diff~m, data = sum_df_cmb2_stre)
summary(stre_m)
library(ggeffects)
sum_df_cmb2_stre2 = ggpredict(stre_m)[[1]]

stre_plt = ggplot() +
  geom_pointrange(data = sum_df_cmb2_stre2, aes(y = rev(x), xmin = exp(conf.low), x = exp(predicted), xmax = exp(conf.high), colour = x), size = 1.1) +
  scale_y_discrete(labels=c("2" = "Correlated effect vs\nRandom intercept",
                            "1" = "Correlated effect vs\nRandom slope")) +
  scale_colour_manual(values = c(m1_col, m2_col, m3_col), guide = "none") +
  labs(x = "Absolute difference in rate of change (%)", y = "") +
  theme_classic() 


sum_df_cmb2_var = data.frame(
  m = as.character(c(rep(1,10),rep(2,10))),
  diff = log((c(df_cmb2$sd_change_3vs1, df_cmb2$sd_change_3vs2)))
)

var_m = lm(diff~m, data = sum_df_cmb2_var)
summary(var_m)
hist(residuals(var_m))
library(ggeffects)
sum_df_cmb2_var2 = ggpredict(var_m)[[1]]

var_plt = ggplot() +
  geom_pointrange(data = sum_df_cmb2_var2, aes(y = rev(x), xmin = exp(conf.low), x = exp(predicted), xmax = exp(conf.high), colour = x), size = 1.1) +
  geom_vline(aes(xintercept = 1), linetype = "dashed") + 
  scale_y_discrete(labels=c("2" = "3 vs 1",
                            "1" = "3 vs 2")) +
  scale_x_log10(breaks = c(0.1,1,10,100), limits = c(0.1,200), labels = c("0.1x", "1x", "10x", "100x")) +
  scale_colour_manual(values = c(m1_col, m2_col, m3_col), guide = "none") +
  labs(x = "Fold change in standard deviation", y = "") +
  theme_classic() +
  theme(axis.text.y = element_blank())


projections_cmb$code = gsub("NA Breeding bird survey (2017)", "BBS (2017)", projections_cmb$code, fixed = T)



plt1 = ggplot(data = projections_cmb[projections_cmb$code  %in% unique(projections_cmb$code)[1],]) +
  geom_line(aes(x = year, y1), colour = m1_col) +
  geom_ribbon(aes(x = year, ymin = lc1, ymax = uc1), alpha = 0.1, fill = m1_col) +
  geom_line(aes(x = year, y2), colour = m2_col) +
  geom_ribbon(aes(x = year, ymin = lc2, ymax = uc2), alpha = 0.1, fill = m2_col) +
  geom_line(aes(x = year, y3), colour = m3_col) +
  geom_ribbon(aes(x = year, ymin = lc3, ymax = uc3), alpha = 0.1, fill = m3_col) +
  geom_hline(aes(yintercept = 100), colour= "grey", linetype = "dotted") +
  scale_x_continuous(expand = c(0,0), breaks = c(2000,2010,2020), limits = c(1994, 2022)) +
  labs(x = " ", y = "Abundance", title = name1) +
  theme_classic() +
  theme(plot.title = element_text(size=8, face = "bold"))


plt2 = ggplot(data = projections_cmb[projections_cmb$code  %in% unique(projections_cmb$code)[2],]) +
  geom_line(aes(x = year, y1), colour = m1_col) +
  geom_ribbon(aes(x = year, ymin = lc1, ymax = uc1), alpha = 0.1, fill = m1_col) +
  geom_line(aes(x = year, y2), colour = m2_col) +
  geom_ribbon(aes(x = year, ymin = lc2, ymax = uc2), alpha = 0.1, fill = m2_col) +
  geom_line(aes(x = year, y3), colour = m3_col) +
  geom_ribbon(aes(x = year, ymin = lc3, ymax = uc3), alpha = 0.1, fill = m3_col) +
  geom_hline(aes(yintercept = 100), colour= "grey", linetype = "dotted") +
  scale_x_continuous(expand = c(0,0), breaks = c(1980,2000,2020), limits = c(1975, 2022)) +
  labs(x = " ", y = " ", title = name2) +
  theme_classic() +
  theme(plot.title = element_text(size=8, face = "bold"))

plt3 = ggplot(data = projections_cmb[projections_cmb$code  %in% unique(projections_cmb$code)[3],]) +
  geom_line(aes(x = year, y1), colour = m1_col) +
  geom_ribbon(aes(x = year, ymin = lc1, ymax = uc1), alpha = 0.1, fill = m1_col) +
  geom_line(aes(x = year, y2), colour = m2_col) +
  geom_ribbon(aes(x = year, ymin = lc2, ymax = uc2), alpha = 0.1, fill = m2_col) +
  geom_line(aes(x = year, y3), colour = m3_col) +
  geom_ribbon(aes(x = year, ymin = lc3, ymax = uc3), alpha = 0.1, fill = m3_col) +
  geom_hline(aes(yintercept = 100), colour= "grey", linetype = "dotted") +
  scale_x_continuous(expand = c(0,0), breaks = c(1980,1995,2010), limits = c(1970, 2015)) +
  labs(x = " ", y = " ", title = name3) +
  theme_classic() +
  theme(plot.title = element_text(size=8, face = "bold"))


plt4 = ggplot(data = projections_cmb[projections_cmb$code  %in% unique(projections_cmb$code)[4],]) +
  geom_line(aes(x = year, y1), colour = m1_col) +
  geom_ribbon(aes(x = year, ymin = lc1, ymax = uc1), alpha = 0.1, fill = m1_col) +
  geom_line(aes(x = year, y2), colour = m2_col) +
  geom_ribbon(aes(x = year, ymin = lc2, ymax = uc2), alpha = 0.1, fill = m2_col) +
  geom_line(aes(x = year, y3), colour = m3_col) +
  geom_ribbon(aes(x = year, ymin = lc3, ymax = uc3), alpha = 0.1, fill = m3_col) +
  geom_hline(aes(yintercept = 100), colour= "grey", linetype = "dotted") +
  scale_x_continuous(expand = c(0,0), breaks = c(1995,2000,2005), limits = c(1992, 2010)) +
  labs(x = " ", y = " ", title = name4) +
  theme_classic() +
  theme(plot.title = element_text(size=8, face = "bold"))


plt5 = ggplot(data = projections_cmb[projections_cmb$code  %in% unique(projections_cmb$code)[5],]) +
  geom_line(aes(x = year, y1), colour = m1_col) +
  geom_ribbon(aes(x = year, ymin = lc1, ymax = uc1), alpha = 0.1, fill = m1_col) +
  geom_line(aes(x = year, y2), colour = m2_col) +
  geom_ribbon(aes(x = year, ymin = lc2, ymax = uc2), alpha = 0.1, fill = m2_col) +
  geom_line(aes(x = year, y3), colour = m3_col) +
  geom_ribbon(aes(x = year, ymin = lc3, ymax = uc3), alpha = 0.1, fill = m3_col) +
  geom_hline(aes(yintercept = 100), colour= "grey", linetype = "dotted") +
  scale_x_continuous(expand = c(0,0), breaks = c(2005,2010,2015), limits = c(2003, 2019)) +
  labs(x = " ", y = " ", title = name5) +
  theme_classic() +
  theme(plot.title = element_text(size=8, face = "bold"))


plt6 = ggplot(data = projections_cmb[projections_cmb$code  %in% unique(projections_cmb$code)[6],]) +
  geom_line(aes(x = year, y1), colour = m1_col) +
  geom_ribbon(aes(x = year, ymin = lc1, ymax = uc1), alpha = 0.1, fill = m1_col) +
  geom_line(aes(x = year, y2), colour = m2_col) +
  geom_ribbon(aes(x = year, ymin = lc2, ymax = uc2), alpha = 0.1, fill = m2_col) +
  geom_line(aes(x = year, y3), colour = m3_col) +
  geom_ribbon(aes(x = year, ymin = lc3, ymax = uc3), alpha = 0.1, fill = m3_col) +
  geom_hline(aes(yintercept = 100), colour= "grey", linetype = "dotted") +
  scale_x_continuous(expand = c(0,0), breaks = c(1970,1995,2020), limits = c(1966, 2023)) +
  coord_cartesian(ylim = c(0,500)) +
  labs(x = " ", y = "Abundance", title = name6) +
  theme_classic() +
  theme(plot.title = element_text(size=8, face = "bold"))


plt7 = ggplot(data = projections_cmb[projections_cmb$code  %in% unique(projections_cmb$code)[7],]) +
  geom_line(aes(x = year, y1), colour = m1_col) +
  geom_ribbon(aes(x = year, ymin = lc1, ymax = uc1), alpha = 0.1, fill = m1_col) +
  geom_line(aes(x = year, y2), colour = m2_col) +
  geom_ribbon(aes(x = year, ymin = lc2, ymax = uc2), alpha = 0.1, fill = m2_col) +
  geom_line(aes(x = year, y3), colour = m3_col) +
  geom_ribbon(aes(x = year, ymin = lc3, ymax = uc3), alpha = 0.1, fill = m3_col) +
  geom_hline(aes(yintercept = 100), colour= "grey", linetype = "dotted") +
  scale_x_continuous(expand = c(0,0), breaks = c(2008,2012,2016), limits = c(2008, 2017)) +
  labs(x = " ", y = " ", title = name7) +
  theme_classic() +
  theme(plot.title = element_text(size=8, face = "bold"))


plt8 = ggplot(data = projections_cmb[projections_cmb$code  %in% unique(projections_cmb$code)[8],]) +
  geom_line(aes(x = year, y1), colour = m1_col) +
  geom_ribbon(aes(x = year, ymin = lc1, ymax = uc1), alpha = 0.1, fill = m1_col) +
  geom_line(aes(x = year, y2), colour = m2_col) +
  geom_ribbon(aes(x = year, ymin = lc2, ymax = uc2), alpha = 0.1, fill = m2_col) +
  geom_line(aes(x = year, y3), colour = m3_col) +
  geom_ribbon(aes(x = year, ymin = lc3, ymax = uc3), alpha = 0.1, fill = m3_col) +
  geom_hline(aes(yintercept = 100), colour= "grey", linetype = "dotted") +
  scale_x_continuous(expand = c(0,0), breaks = c(1970,1990,2010), limits = c(1953, 2015)) +
  coord_cartesian(ylim = c(0,500)) +
  labs(x = " ", y = " ", title = name8) +
  theme_classic() +
  theme(plot.title = element_text(size=8, face = "bold"))


plt9 = ggplot(data = projections_cmb[projections_cmb$code  %in% unique(projections_cmb$code)[9],]) +
  geom_line(aes(x = year, y1), colour = m1_col) +
  geom_ribbon(aes(x = year, ymin = lc1, ymax = uc1), alpha = 0.1, fill = m1_col) +
  geom_line(aes(x = year, y2), colour = m2_col) +
  geom_ribbon(aes(x = year, ymin = lc2, ymax = uc2), alpha = 0.1, fill = m2_col) +
  geom_line(aes(x = year, y3), colour = m3_col) +
  geom_ribbon(aes(x = year, ymin = lc3, ymax = uc3), alpha = 0.1, fill = m3_col) +
  geom_hline(aes(yintercept = 100), colour= "grey", linetype = "dotted") +
  scale_x_continuous(expand = c(0,0)) +
  labs(x = " ", y = " ", title = name9) +
  theme_classic() +
  theme(plot.title = element_text(size=8, face = "bold"))

legend_creator = data.frame(
  id = c("1","2","3"),
  x = 1000,
  y = 100
)

plt10 = ggplot(data = projections_cmb[projections_cmb$code  %in% unique(projections_cmb$code)[10],]) +
  geom_line(aes(x = year, y1), colour = m1_col) +
  geom_ribbon(aes(x = year, ymin = lc1, ymax = uc1), alpha = 0.1, fill = m1_col) +
  geom_line(aes(x = year, y2), colour = m2_col) +
  geom_ribbon(aes(x = year, ymin = lc2, ymax = uc2), alpha = 0.1, fill = m2_col) +
  geom_line(aes(x = year, y3), colour = m3_col) +
  geom_ribbon(aes(x = year, ymin = lc3, ymax = uc3), alpha = 0.1, fill = m3_col) +
  geom_hline(aes(yintercept = 100), colour= "grey", linetype = "dotted") +
  geom_point(data = legend_creator, aes(x = x, y = y, colour = id)) +
  scale_colour_manual(name = "Model", labels = c(
    "1: Random intercept",
    "2: Random slope",
    "3: Correlated effect"
  ), values = c(
    m1_col, 
    m2_col, 
    m3_col
  )) +
  scale_x_continuous(expand = c(0,0), breaks = c(1960,1990,2020)) +
  coord_cartesian(xlim = c(1953, 2023), ylim = c(0,3500)) +
  labs(x = " ", y = " ", title = name10) +
  theme_classic() +
  theme(plot.title = element_text(size=8, face = "bold"),
        legend.position = "bottom")


#library(ggpubr)




ggarrange(plt1, plt2, plt3, plt4, plt5, plt6, plt7, plt8, plt9, plt10, ncol = 5, nrow = 2, common.legend = T, legend = "top")
ggarrange(rev_plt, stre_plt2, var_plt2, ncol = 1, nrow = 3)
ggarrange(stre_plt, var_plt, ncol = 2, nrow = 1, widths = c(1,0.8))


df_cmb2$mean_ac = rowMeans(abs(df_cmb2[,c(19,20)]), na.rm = T)
plt_mnchange_3vs2 = ggplot(df_cmb2) +
  geom_point(aes(x = mean_ac, y = abs(mn_change_3vs2))) +
  geom_smooth(aes(x = mean_ac, y = abs(mn_change_3vs2)), method = "lm", fill = "blue", alpha = 0.1) +
  labs(x = "Mean signal (h)", y = "Absolute difference in rate\nof change (%)") +
  theme_classic()


plt_sdchange_3vs2 = ggplot(df_cmb2) +
  geom_point(aes(x = mean_ac, y = (sd_change_3vs2))) +
  geom_smooth(aes(x = mean_ac, y = (sd_change_3vs2)), method = "lm", fill = "blue", alpha = 0.1) +
  labs(x = "Mean signal (h)", y = "Fold change in standard deviation") +
  theme_classic()


ggarrange(plt_mnchange_3vs2, plt_sdchange_3vs2, ncol = 2)


ggplot(df_cmb) +
  geom_pointrange(aes(x = code, ymin = coef_lc5, y = coef, ymax = coef_uc5, colour = (as.factor(model))), position = position_dodge2(width = 1), size = 0.7) +
  geom_pointrange(aes(x = code, ymin = coef_lc, y = coef, ymax = coef_uc, colour = (as.factor(model))), position = position_dodge2(width = 1), size = 0.3) +
  coord_flip() + 
  scale_colour_manual(guide = F, values = c("#018571", "#80cdc1", "#dfc27d")) +
  labs(x = "", y = "Global trend coefficient") +
  theme_classic()

