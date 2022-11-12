
df_cmb = NULL
df_cmb2 = NULL
df_cmb3 = NULL
df_cmb4 = NULL
for(a in c(1,3:7,9:11,13)){
  tryCatch({
    tmp2 = readRDS(paste0("produced_data/run_modelling1_", a,".rds"))
    df_tmp = data.frame(
      dataset = a,
      code = tmp2[[1]]$code[1],
      n = tmp2[[2]]$n,
      n_species = tmp2[[2]]$n_species,
      n_site = tmp2[[2]]$n_site,
      n_trends = tmp2[[2]]$n_trends,
      model = c(1:3)
    )
    if(ncol(tmp2[[3]]) == 18){
      tmp_tmp = cbind(tmp2[[3]][,c(1:15)], squ = NA, tmp2[[3]][,c(16:18)])
    } else {
      tmp_tmp = tmp2[[3]]
    }
    df_tmp = left_join(df_tmp, tmp_tmp)
    df_tmp$coef_cv = df_tmp$coef_sd/abs(df_tmp$coef)

    
    
    sumhj = unique(tmp2[[1]][,c("tips_id2", "site_spec")]) %>%
      group_by(tips_id2) %>%
      summarise(N = n())
    
    
    
    df_tmp2 = data.frame(
      code = tmp2[[1]]$code[1],
      mn_change_2vs1 = 100*(exp(df_tmp$coef[2])-1) - 100*(exp(df_tmp$coef[1])-1),
      sd_change_2vs1 = df_tmp$coef_sd[2]/df_tmp$coef_sd[1],
      rev2vs1 = sign(df_tmp$coef[2]) == sign(df_tmp$coef[1]),
      stre2vs1 = abs(df_tmp$coef[2]) > abs(df_tmp$coef[1]),
      var2vs1 = df_tmp$coef_sd[2] > df_tmp$coef_sd[1],
      mn_change_3vs1 = 100*(exp(df_tmp$coef[3])-1) - 100*(exp(df_tmp$coef[1])-1),
      sd_change_3vs1 = df_tmp$coef_sd[3]/df_tmp$coef_sd[1],
      rev3vs1 = sign(df_tmp$coef[3]) == sign(df_tmp$coef[1]),
      stre3vs1 = abs(df_tmp$coef[3]) > abs(df_tmp$coef[1]),
      var3vs1 = df_tmp$coef_sd[3] > df_tmp$coef_sd[1],
      mn_change_3vs2 = 100*(exp(df_tmp$coef[3])-1) - 100*(exp(df_tmp$coef[2])-1),
      sd_change_3vs2 = df_tmp$coef_sd[3]/df_tmp$coef_sd[2],
      rev3vs2 = sign(df_tmp$coef[3]) == sign(df_tmp$coef[2]),
      stre3vs2 = abs(df_tmp$coef[3]) > abs(df_tmp$coef[2]),
      var3vs2 = df_tmp$coef_sd[3] > df_tmp$coef_sd[2],
      phy_coef =     phylosig(tmp2[[5]], tmp2[[4]][[3]]$summary.random$tips_id$mean + tmp2[[4]][[3]]$summary.random$tips_id2$mean + df_tmp$coef[3], method = "lambda")[[1]],
      phy_n = phylosig(tmp2[[5]], sumhj$N[c(-nrow(sumhj))], method = "lambda")[[1]],
      phy_h = df_tmp$tip_h[3]/(df_tmp$tip[3] + df_tmp$tip_h[3] + df_tmp$gen[3]),
      spa_h = df_tmp$sit_h[3]/(df_tmp$sit[3] + df_tmp$sit_h[3] + df_tmp$squ[3]),
      temp = df_tmp$obv_auto[3]/(df_tmp$obv[3] + df_tmp$obv_auto[3]),
      temp_phi = df_tmp$phi[3],
      temp_v = sum(c(df_tmp$obv[3], df_tmp$obv_auto[3]),na.rm = T)/sum(c(
        df_tmp$fix[3],
        df_tmp$obv[3],
        df_tmp$obv_auto[3],
        df_tmp$tip[3],
        df_tmp$gen[3],
        df_tmp$tip_h[3],
        df_tmp$sit[3],
        df_tmp$squ[3],
        df_tmp$sit_h[3],
        df_tmp$sig[3]
      ),na.rm = T),
      sit_v = sum(c(df_tmp$sit[3],df_tmp$sit_h[3], df_tmp$squ[3]),na.rm = T)/sum(c(
        df_tmp$fix[3],
        df_tmp$obv[3],
        df_tmp$obv_auto[3],
        df_tmp$tip[3],
        df_tmp$gen[3],
        df_tmp$tip_h[3],
        df_tmp$sit[3],
        df_tmp$squ[3],
        df_tmp$sit_h[3],
        df_tmp$sig[3]
      ),na.rm = T),
      phy_v = sum(c(df_tmp$tip[3],df_tmp$tip_h[3], df_tmp$gen[3]),na.rm = T)/sum(c(
        df_tmp$fix[3],
        df_tmp$obv[3],
        df_tmp$obv_auto[3],
        df_tmp$tip[3],
        df_tmp$gen[3],
        df_tmp$tip_h[3],
        df_tmp$sit[3],
        df_tmp$squ[3],
        df_tmp$sit_h[3],
        df_tmp$sig[3]
      ),na.rm = T),
      res_v = sum(c(df_tmp$sig[3]),na.rm = T)/sum(c(
        df_tmp$fix[3],
        df_tmp$obv[3],
        df_tmp$obv_auto[3],
        df_tmp$tip[3],
        df_tmp$gen[3],
        df_tmp$tip_h[3],
        df_tmp$sit[3],
        df_tmp$squ[3],
        df_tmp$sit_h[3],
        df_tmp$sig[3]
      ),na.rm = T),
      fix_v = sum(c(df_tmp$fix[3]),na.rm = T)/sum(c(
        df_tmp$fix[3],
        df_tmp$obv[3],
        df_tmp$obv_auto[3],
        df_tmp$tip[3],
        df_tmp$gen[3],
        df_tmp$tip_h[3],
        df_tmp$sit[3],
        df_tmp$squ[3],
        df_tmp$sit_h[3],
        df_tmp$sig[3]
      ),na.rm = T)
      
    )
    df_cmb2 = rbind(df_cmb2, df_tmp2)
    
    gen_link = unique(tmp2[[1]][,c("tips_id", "genus.label")])
    tips_df = data.frame(
      code = tmp2[[1]]$code[1],
      tips_id = tmp2[[4]][[2]]$summary.random$tips_id$ID,
      coef_m1 = df_tmp$coef[1],
      mn_tip_m1 = tmp2[[4]][[1]]$summary.random$tips_id$`0.5quant`,
      lc_tip_m1 = tmp2[[4]][[1]]$summary.random$tips_id$`0.025quant`,
      uc_tip_m1 = tmp2[[4]][[1]]$summary.random$tips_id$`0.975quant`,
      coef_m2 = df_tmp$coef[2],
      mn_tip_m2 = tmp2[[4]][[2]]$summary.random$tips_id$`0.5quant`,
      lc_tip_m2 = tmp2[[4]][[2]]$summary.random$tips_id$`0.025quant`,
      uc_tip_m2 = tmp2[[4]][[2]]$summary.random$tips_id$`0.975quant`,
      coef_m3 = df_tmp$coef[3],
      mn_tip_m3 = tmp2[[4]][[3]]$summary.random$tips_id$`0.5quant` + tmp2[[4]][[3]]$summary.random$tips_id2$`0.5quant`,
      lc_tip_m3 = tmp2[[4]][[3]]$summary.random$tips_id$`0.025quant` + tmp2[[4]][[3]]$summary.random$tips_id2$`0.025quant`,
      uc_tip_m3 = tmp2[[4]][[3]]$summary.random$tips_id$`0.975quant`+ tmp2[[4]][[3]]$summary.random$tips_id2$`0.975quant`)
    
    gen_link = left_join(gen_link, tips_df)
    
    gen_df = data.frame(
      code = tmp2[[1]]$code[1],
      genus.label = tmp2[[4]][[2]]$summary.random$genus.label$ID,
      mn_gen_m1 = tmp2[[4]][[1]]$summary.random$genus.label$`0.5quant`,
      lc_gen_m1 = tmp2[[4]][[1]]$summary.random$genus.label$`0.025quant`,
      uc_gen_m1 = tmp2[[4]][[1]]$summary.random$genus.label$`0.975quant`,
      mn_gen_m2 = tmp2[[4]][[2]]$summary.random$genus.label$`0.5quant`,
      lc_gen_m2 = tmp2[[4]][[2]]$summary.random$genus.label$`0.025quant`,
      uc_gen_m2 = tmp2[[4]][[2]]$summary.random$genus.label$`0.975quant`,
      mn_gen_m3 = tmp2[[4]][[3]]$summary.random$genus.label$`0.5quant`,
      lc_gen_m3 = tmp2[[4]][[3]]$summary.random$genus.label$`0.025quant`,
      uc_gen_m3 = tmp2[[4]][[3]]$summary.random$genus.label$`0.975quant`)
    
    gen_link = left_join(gen_link, gen_df)
    tip_compile = data.frame(
      code = gen_link$code,
      tips_id = gen_link$tips_id,
      m1 = gen_link$coef_m1 + gen_link$mn_tip_m1 + gen_link$mn_gen_m1,
      m2 = gen_link$coef_m2 + gen_link$mn_tip_m2 + gen_link$mn_gen_m2,
      m3 = gen_link$coef_m3 + gen_link$mn_tip_m3 + gen_link$mn_gen_m3
    )
  
    df_tmp$tip_av = c(
      mean(tip_compile$m1, na.rm = T),
      mean(tip_compile$m2, na.rm = T),
      mean(tip_compile$m3, na.rm = T)
    )
    df_cmb = rbind(df_cmb, df_tmp)

    df_cmb3 = rbind(df_cmb3, tip_compile)
    
    
    df_tmp4 = tmp2[[1]]
    print(range(df_tmp4$year, na.rm = T))
    print((range(df_tmp4$latitude, na.rm = T)))
    print(range(df_tmp4$longitude, na.rm = T))
    
    df_cmb4 = rbind(df_cmb4, df_tmp4)
  }, error=function(e){})
}

df_cmb2$sit_v[df_cmb2$sit_v == 0] = NA
round(mean(df_cmb2$temp_v),2)
round(sd(df_cmb2$temp_v),2)

round(mean(df_cmb2$sit_v, na.rm = T),2)
round(sd(df_cmb2$sit_v, na.rm = T),2)

round(mean(df_cmb2$phy_v),2)
round(sd(df_cmb2$phy_v),2)

round(mean(df_cmb2$res_v),2)
round(sd(df_cmb2$res_v),2)

round(mean(df_cmb2$fix_v),2)
round(sd(df_cmb2$fix_v),2)

round(mean(df_cmb2$temp),2)
round(sd(df_cmb2$temp),2)

round(mean(df_cmb2$phy_h),2)
round(sd(df_cmb2$phy_h),2)

round(mean(df_cmb2$spa_h, na.rm = T),2)
round(sd(df_cmb2$spa_h, na.rm = T),2)


length(unique(df_cmb4$unique_site))
length(unique(df_cmb4$unique_name))
length(unique(df_cmb4$site_spec))

