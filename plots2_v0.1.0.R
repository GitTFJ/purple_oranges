example_dat = readRDS(paste0("produced_data/run_modelling2_", 10,".rds"))

site_code = 778

example_dat_df = example_dat[[1]]
example_dat_df$m1_pred = example_dat[[4]][[1]]$summary.fitted.values$`0.5quant`
example_dat_df$m1_pred_lc = example_dat[[4]][[1]]$summary.fitted.values$`0.025quant`
example_dat_df$m1_pred_uc = example_dat[[4]][[1]]$summary.fitted.values$`0.975quant`
example_dat_df$m2_pred = example_dat[[4]][[2]]$summary.fitted.values$`0.5quant`
example_dat_df$m2_pred_lc = example_dat[[4]][[2]]$summary.fitted.values$`0.025quant`
example_dat_df$m2_pred_uc = example_dat[[4]][[2]]$summary.fitted.values$`0.975quant`
example_dat_df$m3_pred = example_dat[[4]][[3]]$summary.fitted.values$`0.5quant`
example_dat_df$m3_pred_lc = example_dat[[4]][[3]]$summary.fitted.values$`0.025quant`
example_dat_df$m3_pred_uc = example_dat[[4]][[3]]$summary.fitted.values$`0.975quant`

example_dat_df2 = example_dat[[4]][[1]]$summary.random$site_id[,c(1,5)]
example_dat_df2 = cbind(example_dat_df2, example_dat[[4]][[2]]$summary.random$site_id[,c(5)])
example_dat_df2 = cbind(example_dat_df2, example_dat[[4]][[3]]$summary.random$site_id[,c(5)] + example_dat[[4]][[3]]$summary.random$site_id2[,c(5)][1:806] )
colnames(example_dat_df2) = c("site_id", "m1", "m2", "m3")


extract_example_dat_df = example_dat_df[which(example_dat_df$site_id == site_code & !is.na(example_dat_df$tips_num)),]
extract_example_dat_df[nrow(extract_example_dat_df)+1,] <- NA
extract_example_dat_df[nrow(extract_example_dat_df)+1,] <- NA
extract_example_dat_df[nrow(extract_example_dat_df)+1,] <- NA
extract_example_dat_df[nrow(extract_example_dat_df)+1,] <- NA

extract_example_dat_df$site_spec[49:52] = c("leg1", "leg2", "leg3", "leg4")
extract_example_dat_df$year[49:52] = 2000
extract_example_dat_df$m1_pred[49:52] = -10
extract_example_dat_df$m1_pred_lc[49:52] = -10
extract_example_dat_df$m1_pred_uc[49:52] = -10




plt_m1_a = ggplot(data = extract_example_dat_df) +
  geom_line(aes(x = year, y = (exp(m1_pred)-0.01), colour = site_spec)) +
  geom_ribbon(aes(x = year, ymin =  (exp(m1_pred_lc)-0.01), ymax = (exp(m1_pred_uc)-0.01), fill = site_spec), alpha = 0.1) +
  geom_point(aes(x = year, y = abundance_norm, colour = site_spec)) +
  coord_cartesian(ylim = c(0,1), xlim = c(min(extract_example_dat_df$year), max(extract_example_dat_df$year))) +
  scale_x_continuous(expand = c(0,0), breaks = c(1995, 1998, 2001)) +
  scale_y_continuous(expand = c(0,0)) +
  scale_colour_manual(
    name = "",
    labels = c("A. fimbria", "A. stomias", "H. stenolepis", "S. alutus", "Site: 58.8N, 149.8W", "Projected trends", "Overall trend", "All site level trends"),
    values = c("#9fbdbf", "#1885d9", "#1c50d4", "#7a2da1", "#a60717", "#d1ad4b", "black", "grey")) +
  scale_fill_manual(
    name = "",
    labels = c("A. fimbria", "A. stomias", "H. stenolepis", "S. alutus", "Site: 58.8N, 149.8W", "Projected trends", "Overall trend", "All site level trends"),
    values = c("#9fbdbf", "#1885d9", "#1c50d4", "#7a2da1", "#a60717", "#d1ad4b", "black", "grey")) +
  labs(x = "", y = "Abundance", title = "Random intercept") +
  theme_classic() + 
  theme(plot.title = element_text(size=10, face="italic"))


plt_m1_b = ggplot()  +
  geom_line(data = example_dat_df[which(example_dat_df$site_id == site_code & !is.na(example_dat_df$tips_num)),] ,
            aes(x = year, y = (exp(m1_pred)-0.01), colour = site_spec)) +
  geom_ribbon(data = example_dat_df[which(example_dat_df$site_id == site_code & !is.na(example_dat_df$tips_num)),] ,
              aes(x = year, ymin =  (exp(m1_pred_lc)-0.01), ymax = (exp(m1_pred_uc)-0.01), fill = site_spec), alpha = 0.1) +
  geom_line(data = example_dat_df[which(example_dat_df$site_id == site_code & is.na(example_dat_df$tips_num)),] ,
            aes(x = year_centre+1998, y = (exp(m1_pred)-0.01), colour = "#a60717")) +
  geom_ribbon(data = example_dat_df[which(example_dat_df$site_id == site_code & is.na(example_dat_df$tips_num)),] ,
              aes(x = year_centre+2000, ymin =  (exp(m1_pred_lc)-0.01), ymax = (exp(m1_pred_uc)-0.01), fill = "#a60717"), alpha = 0.1) +
  coord_cartesian(ylim = c(0,1), xlim = c(min(extract_example_dat_df$year), max(extract_example_dat_df$year))) +
  scale_x_continuous(expand = c(0,0), breaks = c(1995, 1998, 2001)) +
  scale_y_continuous(expand = c(0,0)) +
  scale_colour_manual(
    guide = F,
    values = c("#a60717", "#9fbdbf", "#1885d9", "#1c50d4", "#7a2da1")) +
  scale_fill_manual(
    guide = F,
    values = c("#a60717", "#9fbdbf", "#1885d9", "#1c50d4", "#7a2da1")) +
  labs(x = "", y = "") +
  theme_classic() 

plt_m1_d = ggplot() +
  geom_abline(data = example_dat_df2, 
              aes(intercept = exp(m1)-2.67, slope = 0.001), alpha = 0.1, colour = "grey") +
  geom_line(data = example_dat_df[which(is.na(example_dat_df$site_id)),] ,
            aes(x = year_centre+1998, y = (exp(m1_pred)-0.01), colour = "#a60717")) +
  geom_ribbon(data = example_dat_df[which(is.na(example_dat_df$site_id)),] ,
              aes(x = year_centre+1998, ymin =  (exp(m1_pred_lc)-0.01), ymax = (exp(m1_pred_uc)-0.01), fill = "#a60717"), alpha = 0.3) +
  coord_cartesian(ylim = c(0,1), xlim = c(min(extract_example_dat_df$year), max(extract_example_dat_df$year))) +
  scale_x_continuous(expand = c(0,0), breaks = c(1995, 1998, 2001)) +
  scale_y_continuous(expand = c(0,0)) +
  scale_colour_manual(
    guide = F,
    values = c("black")) +
  scale_fill_manual(
    guide = F,
    values = c("black")) +
  labs(x = "", y = "") +
  theme_classic() 


ab_lc = c(100)
ab_mn = c(100)
ab_uc = c(100)
for(a in c(1:11)){
  ab_lc = c(ab_lc,ab_lc[a]*(1 + m3$summary.fixed$`0.025quant`[2]))
  ab_mn = c(ab_mn,ab_mn[a]*(1 + m3$summary.fixed$`0.5quant`[2]))
  ab_uc = c(ab_uc,ab_uc[a]*(1 + m3$summary.fixed$`0.975quant`[2]))
}
example_dat_proj_m1 = data.frame(
  year = c(1993:2004),
  ab_lc = ab_lc,
  ab_mn = ab_mn,
  ab_uc = ab_uc
)

plt_m1_e = ggplot(example_dat_proj_m1) +
  geom_line(aes(x = year, y = ab_mn), colour = "#d1ad4b", size = 2) +
  geom_ribbon(aes(x = year, ymin = ab_lc, ymax = ab_uc), alpha = 0.4, fill = "#d1ad4b") +
  geom_hline(aes(yintercept = 100), linetype = "dashed") +
  scale_y_continuous(limits  = c(0,500), expand = c(0,0)) +
  scale_x_continuous(expand = c(0,0), breaks = c(1995, 1998, 2001)) +
  theme_classic() +
  labs(x = "Year", y = "Projected\nabundance")

plt_m3_a = ggplot(example_dat_df[which(example_dat_df$site_id == site_code & !is.na(example_dat_df$tips_num)),])  +
  geom_line(aes(x = year, y = exp(m2_pred + abundance_norm_log_mean)-0.01, colour = site_spec)) +
  geom_ribbon(aes(x = year, ymin = exp(m2_pred_lc + abundance_norm_log_mean)-0.01, ymax = exp(m2_pred_uc + abundance_norm_log_mean)-0.01, fill = site_spec), alpha = 0.3) +
  geom_point(aes(x = year, y = exp(abundance_norm_centre + abundance_norm_log_mean)-0.01, colour = site_spec))  +
  coord_cartesian(ylim = c(0,1), xlim = c(min(extract_example_dat_df$year), max(extract_example_dat_df$year))) +
  scale_x_continuous(expand = c(0,0), breaks = c(1995, 1998, 2001)) +
  scale_y_continuous(expand = c(0,0)) +
  scale_colour_manual(
    guide = F,
    values = c("#9fbdbf", "#1885d9", "#1c50d4", "#7a2da1", "#a60717", "#d18e08")) +
  scale_fill_manual(
    guide = F,
    values = c("#9fbdbf", "#1885d9", "#1c50d4", "#7a2da1", "#a60717", "#d18e08")) +
  labs(x = "", y = "Abundance", title = "Random slope") +
  theme_classic() + 
  theme(plot.title = element_text(size=10, face="italic"))

plt_m3_b = ggplot()  +
  geom_line(data = example_dat_df[which(example_dat_df$site_id == site_code & !is.na(example_dat_df$tips_num)),], aes(x = year, y = exp(m2_pred + abundance_norm_log_mean)-0.01, colour = site_spec)) +
  geom_ribbon(data = example_dat_df[which(example_dat_df$site_id == site_code & !is.na(example_dat_df$tips_num)),], aes(x = year, ymin = exp(m2_pred_lc + abundance_norm_log_mean)-0.01, ymax = exp(m2_pred_uc + abundance_norm_log_mean)-0.01, fill = site_spec), alpha = 0.3) +
  geom_line(data = example_dat_df[which(example_dat_df$site_id == site_code & is.na(example_dat_df$tips_num)),]  ,
            aes(x = year_centre+1998, y = exp(m2_pred + - 1.318)-0.01, colour = "#a60717")) +
  geom_ribbon(data = example_dat_df[which(example_dat_df$site_id == site_code & is.na(example_dat_df$tips_num)),] ,
              aes(x = year_centre+1998, ymin =  exp(m2_pred_lc  + - 1.318)-0.01, ymax = exp(m2_pred_uc   + - 1.318)-0.01, fill = "#a60717"), alpha = 0.1) +
  coord_cartesian(ylim = c(0,1), xlim = c(min(extract_example_dat_df$year), max(extract_example_dat_df$year))) +
  scale_x_continuous(expand = c(0,0), breaks = c(1995, 1998, 2001)) +
  scale_y_continuous(expand = c(0,0)) +
  scale_colour_manual(
    guide = F,
    values = c("#a60717", "#9fbdbf", "#1885d9", "#1c50d4", "#7a2da1")) +
  scale_fill_manual(
    guide = F,
    values = c("#a60717", "#9fbdbf", "#1885d9", "#1c50d4", "#7a2da1")) +
  labs(x = "", y = "") +
  theme_classic()


example_dat_df2_m3 = NULL
for(a in 1:nrow(example_dat_df2)){
  temp_range = subset(example_dat_df, site_id == a & !is.na(unique_name))
  upper =  max(temp_range$year) - as.integer((max(temp_range$year) + min(temp_range$year))/2)
  #upper = 6
  lower = as.integer((max(temp_range$year) + min(temp_range$year))/2) - min(temp_range$year)
  #lower = 5
  tmp_example_dat_df2_m3 = data.frame(
    site = a,
    year = c(-lower:upper),
    #year = c(min(temp_range$year):max(temp_range$year)),
    abundance = example_dat_df2$m2[a]*-lower:upper + 0.255
  )
  example_dat_df2_m3 = rbind(example_dat_df2_m3, tmp_example_dat_df2_m3)
}


plt_m3_d = ggplot() +
  geom_line(data = example_dat_df2_m3, aes(x = year+1998, y = (abundance), group = site), alpha = 0.1, colour = "grey") +
  geom_line(data = example_dat_df[which(is.na(example_dat_df$site_id)),], 
            aes(x = year_centre + 1998, y = (exp(m2_pred + -1.318)-0.01), colour = "#a60717")) +
  geom_ribbon(data = example_dat_df[which(is.na(example_dat_df$site_id)),] ,
              aes(x = year_centre + 1998, ymin =  (exp(m2_pred_lc + -1.318)-0.01), ymax = (exp(m2_pred_uc + -1.318)-0.01), fill = "#a60717"), alpha = 0.3) +
  coord_cartesian(ylim = c(0,1), xlim = c(min(extract_example_dat_df$year), max(extract_example_dat_df$year))) +
  scale_x_continuous(expand = c(0,0), breaks = c(1995, 1998, 2001)) +
  scale_y_continuous(expand = c(0,0)) +
  scale_colour_manual(
    guide = F,
    values = c("black")) +
  scale_fill_manual(
    guide = F,
    values = c("black")) +
  labs(x = "", y = "") +
  theme_classic()


ab_lc = c(100)
ab_mn = c(100)
ab_uc = c(100)
for(a in c(1:11)){
  ab_lc = c(ab_lc,ab_lc[a]*(1 + m3$summary.fixed$`0.025quant`[2]))
  ab_mn = c(ab_mn,ab_mn[a]*(1 + m3$summary.fixed$`0.5quant`[2]))
  ab_uc = c(ab_uc,ab_uc[a]*(1 + m3$summary.fixed$`0.975quant`[2]))
}
example_dat_proj_m3 = data.frame(
  year = c(min(extract_example_dat_df$year):max(extract_example_dat_df$year)),
  ab_lc = ab_lc,
  ab_mn = ab_mn,
  ab_uc = ab_uc
)

plt_m3_e = ggplot(example_dat_proj_m3) +
  geom_line(aes(x = year, y = ab_mn), colour = "#d1ad4b", size = 2) +
  geom_ribbon(aes(x = year, ymin = ab_lc, ymax = ab_uc), alpha = 0.4, fill = "#d1ad4b") +
  geom_hline(aes(yintercept = 100), linetype = "dashed") +
  scale_y_continuous(limits  = c(0,500), expand = c(0,0)) +
  scale_x_continuous(expand = c(0,0), breaks = c(1995, 1998, 2001)) +
  theme_classic() +
  labs(x = "Year", y = "Projected\nabundance")

plt_m4_a = ggplot(example_dat_df[which(example_dat_df$site_id == site_code & !is.na(example_dat_df$tips_num)),])  +
  geom_line(aes(x = year, y = exp(m3_pred + abundance_norm_log_mean)-0.01, colour = site_spec)) +
  geom_ribbon(aes(x = year, ymin = exp(m3_pred_lc + abundance_norm_log_mean)-0.01, ymax = exp(m3_pred_uc + abundance_norm_log_mean)-0.01, fill = site_spec), alpha = 0.1) +
  geom_point(aes(x = year, y = exp(abundance_norm_centre + abundance_norm_log_mean)-0.01, colour = site_spec))  +
  coord_cartesian(ylim = c(0,1), xlim = c(min(extract_example_dat_df$year), max(extract_example_dat_df$year))) +
  scale_x_continuous(expand = c(0,0), breaks = c(1995, 1998, 2001)) +
  scale_y_continuous(expand = c(0,0)) +
  scale_colour_manual(
    guide = F,
    values = c("#9fbdbf", "#1885d9", "#1c50d4", "#7a2da1")) +
  scale_fill_manual(
    guide = F,
    values = c("#9fbdbf", "#1885d9", "#1c50d4", "#7a2da1")) +
  labs(x = "Year", y = "Abundance", title = "Correlated effect") +
  theme_classic() + 
  theme(plot.title = element_text(size=10, face="italic"))


plt_m4_b = ggplot()  +
  geom_line(data = example_dat_df[which(example_dat_df$site_id == site_code & !is.na(example_dat_df$tips_num)),], aes(x = year, y = exp(m3_pred + abundance_norm_log_mean)-0.01, colour = site_spec)) +
  geom_ribbon(data = example_dat_df[which(example_dat_df$site_id == site_code & !is.na(example_dat_df$tips_num)),], aes(x = year, ymin = exp(m3_pred_lc + abundance_norm_log_mean)-0.01, ymax = exp(m3_pred_uc + abundance_norm_log_mean)-0.01, fill = site_spec), alpha = 0.1) +
  geom_line(data = example_dat_df[which(example_dat_df$site_id == site_code & is.na(example_dat_df$tips_num)),]  ,
            aes(x = year_centre+1998, y = exp(m3_pred + - 1.318)-0.01, colour = "#a60717")) +
  geom_ribbon(data = example_dat_df[which(example_dat_df$site_id == site_code & is.na(example_dat_df$tips_num)),] ,
              aes(x = year_centre+1998, ymin =  exp(m3_pred_lc  + - 1.318)-0.01, ymax = exp(m3_pred_uc   + - 1.318)-0.01, fill = "#a60717"), alpha = 0.1) +
  coord_cartesian(ylim = c(0,1), xlim = c(min(extract_example_dat_df$year), max(extract_example_dat_df$year))) +
  scale_x_continuous(expand = c(0,0), breaks = c(1995, 1998, 2001)) +
  scale_y_continuous(expand = c(0,0)) +
  scale_colour_manual(
    guide = F,
    values = c("#a60717", "#9fbdbf", "#1885d9", "#1c50d4", "#7a2da1")) +
  scale_fill_manual(
    guide = F,
    values = c("#a60717", "#9fbdbf", "#1885d9", "#1c50d4", "#7a2da1")) +
  labs(x = "Year", y = "") +
  theme_classic()



example_dat_df2_m4 = NULL
for(a in 1:nrow(example_dat_df2)){
  temp_range = subset(example_dat_df, site_id == a & !is.na(unique_name))
  upper =  max(temp_range$year) - as.integer((max(temp_range$year) + min(temp_range$year))/2)
  #upper = 6
  lower = as.integer((max(temp_range$year) + min(temp_range$year))/2) - min(temp_range$year)
  #lower = 5
  tmp_example_dat_df2_m4 = data.frame(
    site = a,
    year = c(-lower:upper),
    #year = c(min(temp_range$year):max(temp_range$year)),
    abundance = example_dat_df2$m2[a]*-lower:upper + 0.255
  )
  example_dat_df2_m4 = rbind(example_dat_df2_m4, tmp_example_dat_df2_m4)
}


plt_m4_d = ggplot() +
  geom_line(data = example_dat_df2_m4, aes(x = year+1998, y = abundance, group = site), alpha = 0.1, colour = "grey") +
  geom_line(data = example_dat_df[which(is.na(example_dat_df$site_id)),], 
            aes(x = year_centre + 1998, y = (exp(m3_pred + -1.318)-0.01), colour = "#a60717")) +
  geom_ribbon(data = example_dat_df[which(is.na(example_dat_df$site_id)),] ,
              aes(x = year_centre + 1998, ymin =  (exp(m3_pred_lc + -1.318)-0.01), ymax = (exp(m3_pred_uc + -1.318)-0.01), fill = "#a60717"), alpha = 0.3) +
  coord_cartesian(ylim = c(0,1), xlim = c(min(extract_example_dat_df$year), max(extract_example_dat_df$year))) +
  scale_x_continuous(expand = c(0,0), breaks = c(1995, 1998, 2001)) +
  scale_y_continuous(expand = c(0,0)) +
  scale_colour_manual(
    guide = F,
    values = c("black")) +
  scale_fill_manual(
    guide = F,
    values = c("black")) +
  labs(x = "Year", y = "") +
  theme_classic()

ab_lc = c(100)
ab_mn = c(100)
ab_uc = c(100)
for(a in c(1:11)){
  ab_lc = c(ab_lc,ab_lc[a]*(1 + m3$summary.fixed$`0.025quant`[2]))
  ab_mn = c(ab_mn,ab_mn[a]*(1 + m3$summary.fixed$`0.5quant`[2]))
  ab_uc = c(ab_uc,ab_uc[a]*(1 + m3$summary.fixed$`0.975quant`[2]))
}
example_dat_proj_m4 = data.frame(
  year = c(min(extract_example_dat_df$year):max(extract_example_dat_df$year)),
  ab_lc = ab_lc,
  ab_mn = ab_mn,
  ab_uc = ab_uc
)

plt_m4_e = ggplot(example_dat_proj_m4) +
  geom_line(aes(x = year, y = ab_mn), colour = "#d1ad4b", size = 2) +
  geom_ribbon(aes(x = year, ymin = ab_lc, ymax = ab_uc), alpha = 0.4, fill = "#d1ad4b") +
  geom_hline(aes(yintercept = 100), linetype = "dashed") +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_continuous(expand = c(0,0), breaks = c(1995, 1998, 2001)) +
  coord_cartesian(ylim  = c(0,500)) +
  theme_classic() +
  labs(x = "Year", y = "Projected\nabundance")

ggarrange(
  plt_m1_a,
  plt_m1_b,
  plt_m1_d,
  plt_m1_e,
  plt_m3_a,
  plt_m3_b,
  plt_m3_d,
  plt_m3_e,
  plt_m4_a,
  plt_m4_b,
  plt_m4_d,
  plt_m4_e,
  ncol =4, nrow = 3, common.legend = T)





spa_df = example_dat[[6]]
spa_df3 = data.frame(
  site_id = example_dat[[4]][[3]]$summary.random$site_id2$ID,
  slope =  example_dat[[4]][[3]]$summary.random$site_id2$`0.5quant`)
spa_df2 = left_join(spa_df, spa_df3)
spa_df2 = spa_df2[c(807:941),]

library(akima)
library(ggplot2) 
df_interp <- interp(x = spa_df2$longitude, y = spa_df2$latitude, z = spa_df2$slope, nx = 600, ny = 400, extrap = F, linear = F) 
df_interp <- as.data.frame(interp2xyz(df_interp))
df_interp$z = ifelse(df_interp$z > max(spa_df2$slope, na.rm = T), max(spa_df2$slope, na.rm = T), df_interp$z)
df_interp$z = ifelse(df_interp$z < min(spa_df2$slope, na.rm = T), min(spa_df2$slope, na.rm = T), df_interp$z)
colnames(df_interp) = c("long", "lat", "z")
df_interp$z = 100*(exp(df_interp$z)-1)

tip_density = ggplot() +
  geom_density(data = df_interp, aes(x = z), fill = "grey", alpha = 0.4) +
  coord_cartesian(xlim = c(-15,15)) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  labs(x = "Annual rate\nof change (%)", y = " Density") +
  theme_classic()

world <- map_data("world")

#D55E00

plt_map = ggplot() + 
  geom_tile(data = df_interp, aes(x = long, y = lat, fill = z)) + 
  geom_map(data = world, map = world, aes(long, lat, map_id = region), alpha = 0.4) +
  scale_fill_gradient2(name = "Annual rate of\nchange (%)", low = "#0072B2", mid = "white", high = "#a80000", na.value = NA,  breaks = c(-10,-5, 0, 5,10), labels = c("-5", "-2.5", "0","2.5", "+5"), midpoint = 0, guide = "colorbar", limits = c(-10, 10)) +
  coord_sf(xlim = c(-125,-65), ylim = c(25,55)) +
  labs (x = "Longitude", y = "Latitude") +
  theme_classic()

ts_norm_trim = example_dat[[1]]

gen_link = unique(ts_norm_trim[,c("genus.label","tips_num", "tips_id", "unique_name")])

tip_df = data.frame(
  tips_id = example_dat[[4]][[3]]$summary.random$tips_id$ID,
  val1 = example_dat[[4]][[3]]$summary.random$tips_id$`0.5quant` + example_dat[[4]][[3]]$summary.random$tips_id2$`0.5quant`
)

gen_df = data.frame(
  genus.label = example_dat[[4]][[3]]$summary.random$genus.label$ID,
  val2 = example_dat[[4]][[3]]$summary.random$genus.label$`0.5quant`
)

gen_link = left_join(gen_link, tip_df)
gen_link = left_join(gen_link, gen_df)

gen_link$val3 = gen_link$val1 + gen_link$val2
gen_link = subset(gen_link, !is.na(val1))

gen_link$tips_num = as.character(gen_link$tips_num)
spec_df3 = data.frame(
  tips_num = as.character(example_dat[[5]]$tip.label)
)
spec_df3 = left_join(spec_df3, gen_link)
example_dat[[5]]$tip.label = c(spec_df3$unique_name)

vec = data.frame(val = spec_df3$val3)
rownames(vec) = spec_df3$unique_name

phylosig(example_dat[[5]], vec$val, method = "lambda")

rn = spec_df3$unique_name
gen_link = data.frame(
  slope = 100*(exp(spec_df3$val3)-1)
)

tip_density = ggplot() +
  geom_density(data = gen_link, aes(x = slope), fill = "grey", alpha = 0.4) +
  coord_cartesian(xlim = c(-15,15)) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  labs(x = "Annual rate\nof change (%)", y = " Density") +
  theme_classic()


rownames(gen_link) = rn
library(ggtree)

tmp_tr = example_dat[[5]]
tmp_tr$edge.length = log(tmp_tr$edge.length) - min(log(tmp_tr$edge.length)) + 0.01
tmp_tr = force.ultrametric(tmp_tr)
p = ggtree(tmp_tr, layout = "circular") +
  geom_tippoint() 
#geom_tiplab(size = 1.5) +


plt_clad = gheatmap(p, gen_link, offset=-0.05, width=0.2, colnames = F) + 
  scale_fill_gradient2("Annual rate of\nchange (%)" , midpoint = 0, high = "#a80000", low = "#0072B2", mid = "white") 

ggarrange(plt_clad, plt_map, ncol = 1, labels = c("A", "B"), common.legend = T, heights = c(1.2,0.65), legend = "bottom")


