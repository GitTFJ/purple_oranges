#Load packages
library(dplyr)
library(rotl)
library(data.table)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(RColorBrewer)
library(ggtree)
library(ggtreeExtra)
library(ggnewscale)
library(phytools)
library(lme4)
library(brms)
library(INLA)
library(sp)
library(rworldmap)
library(rlpi)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(brinla)
library(broom)
library(geoR)

norm_range <- function(x){
  (x-min(x, na.rm = T))/(max(x, na.rm = T)-min(x, na.rm = T))
}

#Load abundance time series
dat = read.csv(file.choose())
#Search for standardised species names
tx_search <- tnrs_match_names(names = unique(dat$reported_taxa), context_name = "All life")
#Find species unique ids
ott_in_tree <- ott_id(tx_search)[is_in_tree(ott_id(tx_search))]
#Populate tree
tr <- tol_induced_subtree(ott_ids = ott_in_tree)
#Tidy the tips
tr$tip.label = gsub("\\_ott.*","",tr$tip.label)
tr$tip.label = gsub("_"," ",tr$tip.label)
#Compute branch lengths with the grafen approach
tr = ape::compute.brlen(tr)

#Rename tips
tip_labels = data.frame(
  tips_chr = as.factor(tr$tip.label),
  tips_num = as.numeric(as.factor(tr$tip.label))
)
tr$tip.label = as.numeric(as.factor(tr$tip.label)) 

#Find unique sites - the product of dataset and site
dat$unique_site = paste0(dat$dataset_id, dat$site)

#Add tip labels to dataset
dat = left_join(dat, tip_labels, by = c("reported_taxa" = "tips_chr"))

#Find minimum abundance values
dat2 = dat %>%
  group_by(dataset_id, site, unique_site, reported_taxa, tips_num, country, latitude, longitude,unit) %>%
  mutate(abundance_min = min(abundance))

#Only use time series without zeros
dat2 = subset(dat2, abundance_min > 0)

#Centre timeseries in time, and normalize all abudnances to a maximum of 1
ts_norm = dat2 %>%
  group_by(dataset_id, site, unique_site, reported_taxa, tips_num, country, latitude, longitude,unit) %>%
  mutate(year = year, 
         year_centre = year - mean(year),
         year_mn = mean(year),
         abundance_norm = abundance/max(abundance))



#Make coords numeirc
ts_norm$latitude = as.numeric(ts_norm$latitude)
ts_norm$longitude = as.numeric(ts_norm$longitude)

#Derive descriptive stats about datasets
ts_desc = ts_norm %>%
  group_by(dataset_id, site, unique_site, reported_taxa, tips_num, country, latitude, longitude,unit) %>%
  summarise(year_range = max(year) - min(year), 
            count = n() - 1)
ts_desc$perc = (ts_desc$count/ts_desc$year_range)*100
ts_norm = left_join(ts_norm, ts_desc)

#Only use timeseires with no gaps
ts_norm_clip = subset(ts_norm, perc == 100 & !is.na(abundance_norm))

#Find unique site and species combos
ts_norm_clip$site_spec = paste0(ts_norm_clip$unique_site, ts_norm_clip$reported_taxa)

#Log and centre abundance timeseries
ts_norm_clip2 = ts_norm_clip %>%
  group_by(site_spec) %>%
  mutate(
    abundance_norm_log = log(abundance_norm+0.01),
    abundance_norm_log_mean = mean(log(abundance_norm+0.01)),
    abundance_norm_centre = log(abundance_norm+0.01) - mean(log(abundance_norm+0.01)))
ts_norm_clip2 = subset(ts_norm_clip2, !is.na(abundance_norm))
ts_norm_clip2 = subset(ts_norm_clip2, !is.infinite(abundance_norm))

#Set a generic uniform prior
prior_prec = "expression:
  log_dens = 0 - log(2) - theta / 2;
  return(log_dens);
"
#Only keep the 50% longest timeseries
ts_norm_trim = subset(ts_norm_clip2, count >= quantile(ts_norm_clip2$count,0.5, na.rm = T))

#Create phylo vcv
specs = unique(ts_norm_trim$tips_num)
specs = as.character(specs[which(!is.na(specs))])
tr_trim = keep.tip(tr, specs)
phy_mat_trim = vcv.phylo(tr_trim, corr = T)
phy_mat_trim = phy_mat_trim[, colSums(is.na(phy_mat_trim)) != nrow(phy_mat_trim)]
phy_mat_trim = phy_mat_trim[rowSums(is.na(phy_mat_trim)) != ncol(phy_mat_trim), ]
phy_id = rownames(phy_mat_trim)
#Make a precison matrix
phy_mat_trim = chol2inv(chol(phy_mat_trim))
colnames(phy_mat_trim) = phy_id
rownames(phy_mat_trim) = phy_id

#Link vcv to time series data
ts_norm_trim$tips_num = as.character(ts_norm_trim$tips_num)
ts_norm_trim = ts_norm_trim %>%
  left_join(tibble(tips_num = rownames(phy_mat_trim),
                   tips_id = 1:nrow(phy_mat_trim)))
colnames(phy_mat_trim) = 1:dim(phy_mat_trim)[1]
rownames(phy_mat_trim) = 1:dim(phy_mat_trim)[1]
ts_norm_trim$tips_id2 = ts_norm_trim$tips_id 

#Turn the phylogeny into a quasi taxonomy
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

#Create spatial VCV
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

#Make spatial and phylogentic precision matrices sparse
phy_mat_trim = as(phy_mat_trim, "sparseMatrix")  
spa_mat_trim = as(spa_mat_trim, "sparseMatrix")   



message("   Model 1 - Intercept model")
m1 = inla(log(abundance_norm+0.01) ~ #log abundance is the ressonse
            year_centre + #centred year
            f(site_spec,  model = "iid", constr = F, hyper = prior_prec) + #independent random intercept for each population
            f(tips_id, model = "iid", constr = F, hyper = prior_prec) + #independent random intercept for each species
            f(genus.label, model = "iid", constr = F, hyper = prior_prec) + #independent random intercept for each genera
            f(site_id, model = "iid", constr = F, hyper = prior_prec) +
            #independent random intercept for each site
            f(square, model = "iid", constr = F, hyper = prior_prec), #independent random intercept for each region
          data = ts_norm_trim, family = "gaussian", 
          control.predictor=list(compute=TRUE),
          control.inla = list(force.diagonal = T))

m1_coef_mn = m1$summary.fixed[2,4] #median year coefficient
m1_coef_sd = m1$summary.fixed[2,2] #sd year coefficient
m1_fit = m1$summary.fitted.values[,4] #fitted values
m1_pred = sd(m1_fit)^2 #Variance captured by fixed effects
m1_obv = bri.hyperpar.summary(m1)[2,4] #Variance accounted for by population intercept
m1_tip = bri.hyperpar.summary(m1)[3,4] #Variance accounted for by species intercept
m1_gen = bri.hyperpar.summary(m1)[4,4] #Variance accounted for by genera intercept
m1_sit = bri.hyperpar.summary(m1)[5,4]#Variance accounted for by site intercept
m1_squ = bri.hyperpar.summary(m1)[6,4]#Variance accounted for by region intercept
m1_sig = bri.hyperpar.summary(m1)[1,4]#Residual variance

message("   Model 2 - Slope model")
m2 = inla(abundance_norm_centre ~ #Each abundance time series is centered now too. Every line passes through zero on both axes
            year_centre +
            f(site_spec, year_centre, model = "iid", constr = F, hyper = prior_prec) + #All random intercept are now random slopes e.g.
            f(tips_id, year_centre, model = "iid", constr = F, hyper = prior_prec) +
            f(genus.label, year_centre, model = "iid", constr = F, hyper = prior_prec) +
            f(site_id, year_centre, model = "iid", constr = F, hyper = prior_prec) +
            f(square, year_centre, model = "iid", constr = F, hyper = prior_prec),
          data = ts_norm_trim, family = "gaussian", 
          control.predictor=list(compute=TRUE),
          control.inla = list(force.diagonal = T))
m2_coef_mn = m2$summary.fixed[2,4]
m2_coef_sd = m2$summary.fixed[2,2]
m2_fit = m2$summary.fitted.values[,4] 
m2_pred = sd(m2_fit)^2 
m2_obv = bri.hyperpar.summary(m2)[2,4]
m2_tip = bri.hyperpar.summary(m2)[3,4]
m2_gen = bri.hyperpar.summary(m2)[4,4]
m2_sit = bri.hyperpar.summary(m2)[5,4]
m2_squ = bri.hyperpar.summary(m2)[6,4]
m2_sig = bri.hyperpar.summary(m2)[1,4]



#Need aadditional indicator variables
ts_norm_trim$site_spec2 = ts_norm_trim$site_spec
ts_norm_trim$year2 = ts_norm_trim$year
message("   Model 3 - Correlation model")
m3 = inla(abundance_norm_centre ~ 
            year_centre +
            f(year2, model = "ar1", replicate = as.numeric(as.factor(site_spec2))) + #Ar1 is nested within each population
            f(site_spec, year_centre, model = "iid", 
              constr = F, hyper = prior_prec) +
            f(tips_id2, year_centre, model = "generic0", 
              constr = F, Cmatrix = phy_mat_trim, hyper = prior_prec) + #Species random slope contains the phylgoenetic precision matrix
            f(tips_id, year_centre, model = "iid", 
              constr = F, hyper = prior_prec) +
            f(genus.label, year_centre, model = "iid", 
              constr = F, hyper = prior_prec) +
            f(site_id2, year_centre, model = "generic0", 
              constr = F, Cmatrix = spa_mat_trim, hyper = prior_prec) + #Site random slope contains the spatial precision matrix
            f(site_id, year_centre, model = "iid", 
              constr = F, hyper = prior_prec) +
            f(square, year_centre, model = "iid", 
              constr = F, hyper = prior_prec),
          data = ts_norm_trim, family = "gaussian", 
          control.predictor=list(compute=TRUE),
          control.inla = list(force.diagonal = T))
m3_coef_mn = m3$summary.fixed[2,4]
m3_coef_sd = m3$summary.fixed[2,2]
m3_fit = m3$summary.fitted.values[,4] 
m3_pred = sd(m3_fit)^2 
m3_obv_auto = bri.hyperpar.summary(m3)[2,4] #Estimate of variance captured by ar1 term
m3_obv = bri.hyperpar.summary(m3)[4,4]
m3_tip_h = bri.hyperpar.summary(m3)[5,4] #Estimate of variance captured by phylogeny
m3_tip = bri.hyperpar.summary(m3)[6,4]
m3_gen = bri.hyperpar.summary(m3)[7,4]
m3_sit_h = bri.hyperpar.summary(m3)[8,4] #Estiamte of variance captureed by space
m3_sit = bri.hyperpar.summary(m3)[9,4]
m3_squ = bri.hyperpar.summary(m3)[10,4]
m3_sig = bri.hyperpar.summary(m3)[1,4]
m3_phi = bri.hyperpar.summary(m3)[3,4] #Estimate of ar1 correlation between vars

cmb_df = data.frame(
  model = c(1:3),
  coef = c(m1_coef_mn,m2_coef_mn,m3_coef_mn),
  coef_sd = c(m1_coef_sd,m2_coef_sd,m3_coef_sd),
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
