

forecast_run = readRDS("produced_data/run_modelling4_10.rds")
ts_norm_trim4 = forecast_run[[1]]
run_orig =  readRDS("produced_data/run_modelling1_10.rds")


site_spec_df = data.frame(
  site_spec = forecast_run[[4]][[1]]$summary.random$site_spec$ID,
  m1_site_spec = forecast_run[[4]][[1]]$summary.random$site_spec$`0.5quant`,
  m2_site_spec = forecast_run[[4]][[2]]$summary.random$site_spec$`0.5quant`,
  m3_site_spec = forecast_run[[4]][[3]]$summary.random$site_spec$`0.5quant`,
  om1_site_spec = run_orig[[4]][[1]]$summary.random$site_spec$`0.5quant`,
  om2_site_spec = run_orig[[4]][[2]]$summary.random$site_spec$`0.5quant`,
  om3_site_spec = run_orig[[4]][[3]]$summary.random$site_spec$`0.5quant`
)

tips_df = data.frame(
  tips_id = forecast_run[[4]][[1]]$summary.random$tips_id$ID,
  m1_tips_id = forecast_run[[4]][[1]]$summary.random$tips_id$`0.5quant`,
  m2_tips_id = forecast_run[[4]][[2]]$summary.random$tips_id$`0.5quant`,
  m3_tips_id = forecast_run[[4]][[3]]$summary.random$tips_id$`0.5quant` + forecast_run[[4]][[3]]$summary.random$tips_id2$`0.5quant`,
  om1_tips_id = run_orig[[4]][[1]]$summary.random$tips_id$`0.5quant`,
  om2_tips_id = run_orig[[4]][[2]]$summary.random$tips_id$`0.5quant`,
  om3_tips_id = run_orig[[4]][[3]]$summary.random$tips_id$`0.5quant` + run_orig[[4]][[3]]$summary.random$tips_id2$`0.5quant`
)

genus_df = data.frame(
  genus.label = forecast_run[[4]][[1]]$summary.random$genus.label$ID,
  m1_genus_id = forecast_run[[4]][[1]]$summary.random$genus.label$`0.5quant`,
  m2_genus_id = forecast_run[[4]][[2]]$summary.random$genus.label$`0.5quant`,
  m3_genus_id = forecast_run[[4]][[3]]$summary.random$genus.label$`0.5quant`,
  om1_genus_id = run_orig[[4]][[1]]$summary.random$genus.label$`0.5quant`,
  om2_genus_id = run_orig[[4]][[2]]$summary.random$genus.label$`0.5quant`,
  om3_genus_id = run_orig[[4]][[3]]$summary.random$genus.label$`0.5quant`
)

site_df = data.frame(
  site_id = forecast_run[[4]][[1]]$summary.random$site_id$ID,
  m1_site_id = forecast_run[[4]][[1]]$summary.random$site_id$`0.5quant`,
  m2_site_id = forecast_run[[4]][[2]]$summary.random$site_id$`0.5quant`,
  m3_site_id = forecast_run[[4]][[3]]$summary.random$site_id$`0.5quant` + forecast_run[[4]][[3]]$summary.random$site_id2$`0.5quant`[1:806],
  om1_site_id = run_orig[[4]][[1]]$summary.random$site_id$`0.5quant`,
  om2_site_id = run_orig[[4]][[2]]$summary.random$site_id$`0.5quant`,
  om3_site_id = run_orig[[4]][[3]]$summary.random$site_id$`0.5quant` + run_orig[[4]][[3]]$summary.random$site_id2$`0.5quant`[1:806]
)

square_df = data.frame(
  square = forecast_run[[4]][[1]]$summary.random$square$ID,
  m1_square_id = forecast_run[[4]][[1]]$summary.random$square$`0.5quant`,
  m2_square_id = forecast_run[[4]][[2]]$summary.random$square$`0.5quant`,
  m3_square_id = forecast_run[[4]][[3]]$summary.random$square$`0.5quant`,
  om1_square_id = run_orig[[4]][[1]]$summary.random$square$`0.5quant`,
  om2_square_id = run_orig[[4]][[2]]$summary.random$square$`0.5quant`,
  om3_square_id = run_orig[[4]][[3]]$summary.random$square$`0.5quant`
)




ts_norm_trim5 = unique(ts_norm_trim4[,c("site_spec","tips_id", "genus.label", "site_id", "square")])


ts_norm_trim5 = left_join(ts_norm_trim5, site_spec_df)
ts_norm_trim5 = left_join(ts_norm_trim5, tips_df)
ts_norm_trim5 = left_join(ts_norm_trim5, genus_df)
ts_norm_trim5 = left_join(ts_norm_trim5, site_df)
ts_norm_trim5 = left_join(ts_norm_trim5, square_df)

uni_sites_sample = ts_norm_trim4[,c("site_spec", "abundance_norm_centre2")]
uni_sites_sample = subset(uni_sites_sample, is.na(abundance_norm_centre2))
uni_sites_sample = unique(uni_sites_sample$site_spec)
ts_norm_trim5 = ts_norm_trim5[ts_norm_trim5$site_spec %in% uni_sites_sample,]
ts_norm_trim5$m1_est = ts_norm_trim5$m1_site_spec + ts_norm_trim5$m1_tips_id + ts_norm_trim5$m1_genus_id + ts_norm_trim5$m1_site_id + ts_norm_trim5$m1_square_id
ts_norm_trim5$m2_est = ts_norm_trim5$m2_site_spec + ts_norm_trim5$m2_tips_id + ts_norm_trim5$m2_genus_id + ts_norm_trim5$m2_site_id + ts_norm_trim5$m2_square_id
ts_norm_trim5$m3_est = ts_norm_trim5$m3_site_spec + ts_norm_trim5$m3_tips_id + ts_norm_trim5$m3_genus_id + ts_norm_trim5$m3_site_id + ts_norm_trim5$m3_square_id
ts_norm_trim5$m2_true = ts_norm_trim5$om2_site_spec + ts_norm_trim5$om2_tips_id + ts_norm_trim5$om2_genus_id + ts_norm_trim5$om2_site_id + ts_norm_trim5$om2_square_id
ts_norm_trim5$m3_true = ts_norm_trim5$om3_site_spec + ts_norm_trim5$om3_tips_id + ts_norm_trim5$om3_genus_id + ts_norm_trim5$om3_site_id + ts_norm_trim5$om3_square_id

freq_df = unique(ts_norm_trim4[,c("site_spec","tips_id", "genus.label", "site_id", "square")])

freq_df2 = NULL
for(a in freq_df$site_spec){
  tmp = subset(freq_df, site_spec == a)
  tmp_df = data.frame(
    site_spec = a,
    n_tip = sum(freq_df$tips_id == tmp$tips_id),
    n_gen = sum(freq_df$genus.label == tmp$genus.label),
    n_sit = sum(freq_df$site_id == tmp$site_id),
    n_squ = sum(freq_df$square == tmp$square)
  )
  freq_df2 = rbind(freq_df2, tmp_df)
}

freq_df2$lonely = rowMeans(freq_df2[,c(2:5)])
ts_norm_trim5 = left_join(ts_norm_trim5, freq_df2)

temp_forecast_df_long2 = data.frame(
  model = c(rep("2: Random slope", 1026), rep("3: Correlated effect", 1026)),
  fit = c(ts_norm_trim5$m2_est, ts_norm_trim5$m3_est),
  true = c(ts_norm_trim5$m3_true,ts_norm_trim5$m3_true),
  true2 = c(ts_norm_trim5$m2_true,ts_norm_trim5$m2_true),
  lonely = c(ts_norm_trim5$lonely, ts_norm_trim5$lonely)
)

temp_forecast_df_long2$fit = (exp(temp_forecast_df_long2$fit)-1)*100

temp_forecast_df_long2$true = (exp(temp_forecast_df_long2$true)-1)*100




temp_forecast_df_long2$diff = abs(temp_forecast_df_long2$fit - temp_forecast_df_long2$true)



temp_forecast_df_long2_sum = data.frame(
  model = c("2: Random slope","3: Correlated effect"),
  coef = c(
    coef(lm(true2 ~ fit, data = temp_forecast_df_long2[which(temp_forecast_df_long2$model == "2: Random slope"),]))[2],
    coef(lm(true2 ~ fit, data = temp_forecast_df_long2[which(temp_forecast_df_long2$model == "3: Correlated effect"),]))[2]
  ),
  lc = c(
    confint(lm(true2 ~ fit, data = temp_forecast_df_long2[which(temp_forecast_df_long2$model == "2: Random slope"),]))[2,1],
    confint(lm(true2 ~ fit, data = temp_forecast_df_long2[which(temp_forecast_df_long2$model == "3: Correlated effect"),]))[2,1]
  ),
  uc = c(
    confint(lm(true2 ~ fit, data = temp_forecast_df_long2[which(temp_forecast_df_long2$model == "2: Random slope"),]))[2,2],
    confint(lm(true2 ~ fit, data = temp_forecast_df_long2[which(temp_forecast_df_long2$model == "3: Correlated effect"),]))[2,2]
  )
)


ggplot(temp_forecast_df_long2) +
  geom_point(aes(y = true, x = fit), alpha = 0.05) +
  coord_cartesian(ylim = c(-30,30), xlim = c(-30,30)) +
  geom_abline(slope = 1, linetype = "dashed") +
  facet_wrap(model~., scales = "free") +
  theme_classic() +
  labs(x = "Predicted rate of change (%)", y = "Observed rate of change (%)")




temp_forecast_df_long2 %>%
  group_by(model) %>%
  summarise(mae = median(diff), std = sd(diff))
