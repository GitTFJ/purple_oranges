

forecast_run = readRDS("produced_data/run_modelling3_10.rds")
ts_norm_trim4 = forecast_run[[1]]
ts_norm_trim4$m1_fit = forecast_run[[4]][[1]]$summary.fitted.values$`0.5quant`[1:nrow(ts_norm_trim4)]
ts_norm_trim4$m1_fit = exp(ts_norm_trim4$m1_fit - 0.01)
ts_norm_trim4$m2_fit = forecast_run[[4]][[2]]$summary.fitted.values$`0.5quant`[1:nrow(ts_norm_trim4)]
ts_norm_trim4$m2_fit = exp((ts_norm_trim4$m2_fit + ts_norm_trim4$abundance_norm_log_mean)- 0.01)
ts_norm_trim4$m3_fit = forecast_run[[4]][[3]]$summary.fitted.values$`0.5quant`[1:nrow(ts_norm_trim4)]
ts_norm_trim4$m3_fit = exp((ts_norm_trim4$m3_fit + ts_norm_trim4$abundance_norm_log_mean)- 0.01)
temp_forecast_df = ts_norm_trim4
temp_forecast_df = subset(temp_forecast_df, is.na(temp_forecast_df$abundance_norm_centre2))
temp_forecast_df$m1_dif = abs(temp_forecast_df$m1_fit - temp_forecast_df$abundance_norm)
temp_forecast_df$m2_dif = abs(temp_forecast_df$m2_fit - temp_forecast_df$abundance_norm)
temp_forecast_df$m3_dif = abs(temp_forecast_df$m3_fit - temp_forecast_df$abundance_norm)

temp_forecast_df_long = data.frame(
  model = c(rep("1: Random intercept", 5131), rep("2: Random slope", 5131), rep("3: Correlated effect", 5131)),
  fit = c(temp_forecast_df$m1_fit, temp_forecast_df$m2_fit, temp_forecast_df$m3_fit),
  true = c(temp_forecast_df$abundance_norm,temp_forecast_df$abundance_norm,temp_forecast_df$abundance_norm)
)

summary(lm(true ~ fit, data = temp_forecast_df_long[which(temp_forecast_df_long$model == "1: Random intercept"),]))

summary(lm(true ~ fit, data = temp_forecast_df_long[which(temp_forecast_df_long$model == "2: Random slope"),]))

summary(lm(true ~ fit, data = temp_forecast_df_long[which(temp_forecast_df_long$model == "3: Correlated effect"),]))


temp_forecast_df_long_sum = data.frame(
  model = c("1: Random intercept","2: Random slope","3: Correlated effect"),
  coef = c(
    coef(lm(true ~ fit, data = temp_forecast_df_long[which(temp_forecast_df_long$model == "1: Random intercept"),]))[2],
    coef(lm(true ~ fit, data = temp_forecast_df_long[which(temp_forecast_df_long$model == "2: Random slope"),]))[2],
    coef(lm(true ~ fit, data = temp_forecast_df_long[which(temp_forecast_df_long$model == "3: Correlated effect"),]))[2]
  ),
  lc = c(
    confint(lm(true ~ fit, data = temp_forecast_df_long[which(temp_forecast_df_long$model == "1: Random intercept"),]))[2,1],
    confint(lm(true ~ fit, data = temp_forecast_df_long[which(temp_forecast_df_long$model == "2: Random slope"),]))[2,1],
    confint(lm(true ~ fit, data = temp_forecast_df_long[which(temp_forecast_df_long$model == "3: Correlated effect"),]))[2,1]
  ),
  uc = c(
    confint(lm(true ~ fit, data = temp_forecast_df_long[which(temp_forecast_df_long$model == "1: Random intercept"),]))[2,2],
    confint(lm(true ~ fit, data = temp_forecast_df_long[which(temp_forecast_df_long$model == "2: Random slope"),]))[2,2],
    confint(lm(true ~ fit, data = temp_forecast_df_long[which(temp_forecast_df_long$model == "3: Correlated effect"),]))[2,2]
  )
)

temp_forecast_df_long_sum



ggplot(temp_forecast_df_long) +
  geom_point(aes(x = fit, y = true), alpha = 0.05) +
  geom_abline(slope = 1, linetype = "dashed") +
  coord_cartesian(ylim = c(0,1.02), xlim = c(0,1.1)) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  facet_wrap(model~.) +
  theme_classic() +
  labs(x = "Predicted abundances", y = "Observed abundances")

temp_forecast_df_long$diff = abs(temp_forecast_df_long$fit - temp_forecast_df_long$true)

temp_forecast_df_long %>%
  group_by(model) %>%
  summarise(mae = median(diff), std = sd(diff))

fore_pred = lm(log(diff) ~ model, data = temp_forecast_df_long)
summary(fore_pred)
library(ggeffects)
fore_pred = ggpredict(fore_pred, terms = c("model"))

fore1_mae = ggplot(fore_pred) +
  geom_pointrange(aes(y = rev(x), xmin = conf.low*100, x = predicted*100, xmax = conf.high*100)) +
  scale_x_continuous(limits = c(4,13), expand = c(0,0)) +
  scale_y_discrete(labels = c("Correlated effect", "Random slope", "Random intercept")) +
  labs(x = "Normalised absolute error in abundance prediction (%)", y = "") +
  theme_classic()
