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

coords2poly = function(lon, lat, poly, att_names)
{  
  points = data.frame(longitude = lon, latitude = lat)
  pointsSP = SpatialPoints(points, proj4string=CRS("+proj=longlat +datum=WGS84 +no_defs"))  
  indices = over(pointsSP, poly)
  indices = indices[,att_names]
  return(indices)
}

rsq = function (x, y) {
  cor(x, y) ^ 2
}

expand.grid.alt = function(seq1,seq2) {
  cbind(rep.int(seq1, length(seq2)),
        c(t(matrix(rep.int(seq2, length(seq1)), nrow=length(seq2)))))
}

norm_range <- function(x){
  (x-min(x, na.rm = T))/(max(x, na.rm = T)-min(x, na.rm = T))
}

is.nan.data.frame <- function(x){
  do.call(cbind, lapply(x, is.nan))
}

expand_fun = function(code){
  trim = ts[unique_site == code]
  trim = unique(trim)
  expand_trim = as.data.table(expand.grid.alt(unique(trim$unique_name), unique(trim$year)))
  expand_trim$dataset_id = trim$dataset_id[1]
  expand_trim$unique_site = trim$unique_site[1]
  return(expand_trim)
}


saveRDS(sessionInfo(),"produced_data/session_info.rds")
