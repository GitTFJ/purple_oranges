
for(a in c(10)){
  
  code = unique(ts_norm_clip2$code)[a]
  message(code)
  ts_norm_trim = subset(ts_norm_clip2, code == unique(ts_norm_clip2$code)[a])
  ts_norm_trim = subset(ts_norm_trim, count >= quantile(ts_norm_trim$count,0.5, na.rm = T))

  output_list = list()
  ts_desc = unique(ts_norm_trim[,c("code", "unique_name", "unique_site", "site_spec")])
  len_phy = length(unique(ts_norm_trim$tips_num))
  print(paste0("phy_len:", len_phy))
  len_spa = nrow(unique(ts_norm_trim[,c("latitude", "longitude")]))
  print(paste0("spa_len:", len_spa))
  ts_norm_trim$year2 = ts_norm_trim$year_centre
  
  ts_desc_phy = ts_desc %>%
    group_by(code, unique_name) %>%
    summarise(phy_n = n())
  
  ts_desc_spa = ts_desc %>%
    group_by(code, unique_site) %>%
    summarise(spa_n = n())
  
  ts_norm_trim = left_join(ts_norm_trim, ts_desc_phy)
  ts_norm_trim = left_join(ts_norm_trim, ts_desc_spa)
  
  
  #Trim to complete cases
  ts_norm_trim = subset(ts_norm_trim, !is.na(tips_num) & !is.na(latitude) & !is.na(longitude))
  
  
  specs = unique(ts_norm_trim$tips_num)
  specs = as.character(specs[which(!is.na(specs))])
  tr_trim = keep.tip(tr, specs)
  phy_mat_trim = vcv.phylo(tr_trim, corr = T)
  phy_mat_trim = phy_mat_trim[, colSums(is.na(phy_mat_trim)) != nrow(phy_mat_trim)]
  phy_mat_trim = phy_mat_trim[rowSums(is.na(phy_mat_trim)) != ncol(phy_mat_trim), ]
  phy_id = rownames(phy_mat_trim)
  phy_mat_trim = chol2inv(chol(phy_mat_trim))
  colnames(phy_mat_trim) = phy_id
  rownames(phy_mat_trim) = phy_id
  
  ts_norm_trim$tips_num = as.character(ts_norm_trim$tips_num)
  ts_norm_trim = ts_norm_trim %>%
    left_join(tibble(tips_num = rownames(phy_mat_trim),
                     tips_id = 1:nrow(phy_mat_trim)))
  colnames(phy_mat_trim) = 1:dim(phy_mat_trim)[1]
  rownames(phy_mat_trim) = 1:dim(phy_mat_trim)[1]
  ts_norm_trim$tips_id2 = ts_norm_trim$tips_id 
  
  taxo = NULL
  for(b in tr_trim$tip.label){
    tmp_taxo = data.frame(
      tips_num = as.character(b),
      node.label = which(tr_trim$tip.label==b),
      genus.label = getParent(tr_trim, which(tr_trim$tip.label==b)))
    taxo = rbind(taxo, tmp_taxo)
  }
  taxo$node.label = as.numeric(as.factor(taxo$node.label))
  taxo$genus.label = as.numeric(as.factor(taxo$genus.label))
  
  ts_norm_trim = left_join(ts_norm_trim, taxo)
  ts_norm_trim$square = as.numeric(as.factor(paste0(round(ts_norm_trim$latitude,0), "_", round(ts_norm_trim$longitude,0))))
  
  
  ts_norm_trim$site2 = paste0(ts_norm_trim$latitude,"_", ts_norm_trim$longitude)
  ts_norm_trim$site_id = as.numeric(as.factor(ts_norm_trim$site2))
  spa_df = unique(ts_norm_trim[,c("site_id","latitude","longitude")])
  spa_df = subset(spa_df, !is.na(latitude) & !is.na(longitude))
  spa_df2 = expand.grid(seq(20,60,by = 5),seq(-130,-60,by = 5))
  spa_df2 = data.frame(
    site_id = c((max(spa_df$site_id)+1):(max(spa_df$site_id)+nrow(spa_df2))),
    latitude = spa_df2$Var1,
    longitude = spa_df2$Var2
  )
  spa_df = rbind(spa_df, spa_df2)
  
  spa_mat = as.matrix(dist(spa_df[,c(2,3)], diag=T, upper=T))
  spa_mat = norm_range(spa_mat)
  spa_mat = abs(spa_mat - 1) 
  spa_id = spa_df$site_id
  colnames(spa_mat) = spa_id
  rownames(spa_mat) = spa_id
  dim_spa_mat = ncol(spa_mat)
  spa_mat = spa_mat[rowSums(is.na(spa_mat)) != dim_spa_mat, ]
  spa_mat = spa_mat[, colSums(is.na(spa_mat)) != dim_spa_mat]
  spa_mat_trim = spa_mat
  spa_id = colnames(spa_mat_trim)
  spa_mat_trim = chol2inv(chol(spa_mat_trim))
  colnames(spa_mat_trim) = spa_id
  rownames(spa_mat_trim) = spa_id
  ts_norm_trim$site_id2 = ts_norm_trim$site_id
  
  phy_mat_trim = as(phy_mat_trim, "sparseMatrix")  
  spa_mat_trim = as(spa_mat_trim, "sparseMatrix")   
  
  
  m2_output = ts_norm_trim %>% group_by(site_id, site_spec) %>%
    do(fit_lm = lm(log(abundance_norm+0.01) ~ year, data = .))
  
  ts_norm_trim2 = NULL
  for(b in 1:nrow(m2_output)){
    ts_norm_trim2_tmp = data.frame(
      site_id = m2_output$site_id[b],
      site_spec = m2_output$site_spec[b],
      coef = coef(m2_output$fit_lm[[b]])[2],
      var = vcov(m2_output$fit_lm[[b]])[2,2]
    )
    ts_norm_trim2 = rbind(ts_norm_trim2, ts_norm_trim2_tmp)
  }
  ts_norm_trim2_tmp2 = ts_norm_trim2 %>%
    group_by(site_id) %>%
    summarise(N = n(), vr = var(coef))
  
  ts_norm_trim2 = left_join(ts_norm_trim2, ts_norm_trim2_tmp2)
  
  newdata <- ts_norm_trim[c(1:100),]
  newdata[] = NA
  newdata$year_centre = seq(min(ts_norm_trim$year_centre), max(ts_norm_trim$year_centre), length.out = 100)
  newdata$site_id = 778
  newdata$site_id2 = 778
  newdata$square = 322
  ts_norm_trim = rbind(ts_norm_trim, newdata)
  
  
  newdata <- ts_norm_trim[c(1:100),]
  newdata[] = NA
  newdata$year_centre = seq(min(ts_norm_trim$year_centre), max(ts_norm_trim$year_centre), length.out = 100)
  ts_norm_trim = rbind(ts_norm_trim, newdata)
  


  
  message("   Model 1 - Intercept model")
  m1 = inla(log(abundance_norm+0.01) ~ 
              year_centre + 
              f(site_spec,  model = "iid", constr = F, hyper = prior_prec) +
              f(tips_id, model = "iid", constr = F, hyper = prior_prec) +
              f(genus.label, model = "iid", constr = F, hyper = prior_prec) +
              f(site_id, model = "iid", constr = F, hyper = prior_prec) +
              f(square, model = "iid", constr = F, hyper = prior_prec),
            data = ts_norm_trim, family = "gaussian", 
            control.predictor=list(compute=TRUE),
            control.inla = list(force.diagonal = T))
  
  m1_coef_mn = m1$summary.fixed[2,4]
  m1_coef_sd = m1$summary.fixed[2,2]
  m1_coef_lc5 = inla.hpdmarginal(0.5, m1$marginals.fixed[[2]])[1]
  m1_coef_uc5 = inla.hpdmarginal(0.5, m1$marginals.fixed[[2]])[2]
  m1_coef_lc = m1$summary.fixed[2,3]
  m1_coef_uc = m1$summary.fixed[2,5]
  m1_fit = m1$summary.fitted.values[,4]
  m1_res = log(ts_norm_trim$abundance_norm+0.01) - m1_fit
  m1_pred = sd(m1_fit)^2
  m1_obv = bri.hyperpar.summary(m1)[2,4]
  m1_tip = bri.hyperpar.summary(m1)[3,4]
  m1_gen = bri.hyperpar.summary(m1)[4,4]
  m1_sit = bri.hyperpar.summary(m1)[5,4]
  m1_squ = bri.hyperpar.summary(m1)[6,4]
  m1_sig = bri.hyperpar.summary(m1)[1,4]
  
  message("   Model 2 - Slope model")
  m2 = inla(abundance_norm_centre ~ 
              year_centre +
              f(site_spec, year_centre, model = "iid", constr = F, hyper = prior_prec) +
              f(tips_id, year_centre, model = "iid", constr = F, hyper = prior_prec) +
              f(genus.label, year_centre, model = "iid", constr = F, hyper = prior_prec) +
              f(site_id, year_centre, model = "iid", constr = F, hyper = prior_prec) +
              f(square, year_centre, model = "iid", constr = F, hyper = prior_prec),
            data = ts_norm_trim, family = "gaussian", 
            control.predictor=list(compute=TRUE),
            control.inla = list(force.diagonal = T))
  m2_coef_mn = m2$summary.fixed[2,4]
  m2_coef_sd = m2$summary.fixed[2,2]
  m2_coef_lc5 = inla.hpdmarginal(0.5, m2$marginals.fixed[[2]])[1]
  m2_coef_uc5 = inla.hpdmarginal(0.5, m2$marginals.fixed[[2]])[2]
  m2_coef_lc = m2$summary.fixed[2,3]
  m2_coef_uc = m2$summary.fixed[2,5]
  m2_fit = m2$summary.fitted.values[,4]
  m2_res = ts_norm_trim$abundance_norm_centre - m2_fit
  m2_pred = sd(m2_fit)^2
  m2_obv = bri.hyperpar.summary(m2)[2,4]
  m2_tip = bri.hyperpar.summary(m2)[3,4]
  m2_gen = bri.hyperpar.summary(m2)[4,4]
  m2_sit = bri.hyperpar.summary(m2)[5,4]
  m2_squ = bri.hyperpar.summary(m2)[6,4]
  m2_sig = bri.hyperpar.summary(m2)[1,4]
  
  
  
  
  ts_norm_trim$site_spec2 = ts_norm_trim$site_spec
  message("   Model 3 - Correlation model")
  m3 = inla(abundance_norm_centre ~ 
              year_centre +
              f(year2, model = "ar1", replicate = as.numeric(as.factor(site_spec2))) +
              f(site_spec, year_centre, model = "iid", 
                constr = F, hyper = prior_prec) +
              f(tips_id2, year_centre, model = "generic0", 
                constr = F, Cmatrix = phy_mat_trim, hyper = prior_prec) +
              f(tips_id, year_centre, model = "iid", 
                constr = F, hyper = prior_prec) +
              f(genus.label, year_centre, model = "iid", 
                constr = F, hyper = prior_prec) +
              f(site_id2, year_centre, model = "generic0", 
                constr = F, Cmatrix = spa_mat_trim, hyper = prior_prec) +
              f(site_id, year_centre, model = "iid", 
                constr = F, hyper = prior_prec) +
              f(square, year_centre, model = "iid", 
                constr = F, hyper = prior_prec),
            data = ts_norm_trim, family = "gaussian", 
            control.predictor=list(compute=TRUE),
            control.inla = list(force.diagonal = T))
  m3_coef_mn = m3$summary.fixed[2,4]
  m3_coef_sd = m3$summary.fixed[2,2]
  m3_coef_lc5 = inla.hpdmarginal(0.5, m3$marginals.fixed[[2]])[1]
  m3_coef_uc5 = inla.hpdmarginal(0.5, m3$marginals.fixed[[2]])[2]
  m3_coef_lc = m3$summary.fixed[2,3]
  m3_coef_uc = m3$summary.fixed[2,5]
  m3_fit = m3$summary.fitted.values[,4]
  m3_res = ts_norm_trim$abundance_norm_centre - m3_fit
  m3_pred = sd(m3_fit)^2
  m3_obv_auto = bri.hyperpar.summary(m3)[2,4]
  m3_obv = bri.hyperpar.summary(m3)[4,4]
  m3_tip_h = bri.hyperpar.summary(m3)[5,4]
  m3_tip = bri.hyperpar.summary(m3)[6,4]
  m3_gen = bri.hyperpar.summary(m3)[7,4]
  m3_sit_h = bri.hyperpar.summary(m3)[8,4]
  m3_sit = bri.hyperpar.summary(m3)[9,4]
  m3_squ = bri.hyperpar.summary(m3)[10,4]
  m3_sig = bri.hyperpar.summary(m3)[1,4]
  m3_phi = bri.hyperpar.summary(m3)[3,4]
  
  cmb_df = data.frame(
    code = code,
    model = c(1:3),
    coef = c(m1_coef_mn,m2_coef_mn,m3_coef_mn),
    coef_sd = c(m1_coef_sd,m2_coef_sd,m3_coef_sd),
    coef_lc5 = c(m1_coef_lc5,m2_coef_lc5,m3_coef_lc5),
    coef_uc5 = c(m1_coef_uc5,m2_coef_uc5,m3_coef_uc5),
    coef_lc = c(m1_coef_lc,m2_coef_lc,m3_coef_lc),
    coef_uc = c(m1_coef_uc,m2_coef_uc,m3_coef_uc),
    fix = c(m1_pred,m2_pred,m3_pred),
    obv = c(m1_obv,m2_obv,m3_obv),
    obv_auto = c(NA,NA,m3_obv_auto),
    tip = c(m1_tip,m2_tip,m3_tip),
    gen = c(m1_gen,m2_gen,m3_gen),
    tip_h = c(NA,NA,m3_tip_h),
    sit = c(m1_sit,m2_sit,m3_sit),
    squ = c(m1_squ,m2_squ,m3_squ),
    sit_h = c(NA,NA,m3_sit_h),
    sig = c(m1_sig,m2_sig,m3_sig),
    phi = c(NA,NA,m3_phi)
  )
  
  descriptives = data.frame(
    code = code,
    n = nrow(ts_norm_trim),
    n_species = length(unique(ts_norm_trim$tips_id)),
    n_site = length(unique(ts_norm_trim$site_id)),
    n_trends = length(unique(ts_norm_trim$site_spec)) 
  )
  
  model_list = list(m1,m2,m3)
  
  output_list[[1]] = ts_norm_trim
  output_list[[2]] = descriptives
  output_list[[3]] = cmb_df
  output_list[[4]] = model_list
  output_list[[5]] = tr_trim
  output_list[[6]] = spa_df
  
  message("SUCCESS")
  
  
  saveRDS(output_list, paste0("produced_data/run_modelling2_", a,".rds"))
  
}



