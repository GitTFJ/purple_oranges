ts = read.csv("raw_data/combined_ts_V0.3.csv")
tx = read.csv("raw_data/combined_tx_V0.1.csv")
sp = read.csv("raw_data/combined_sp_V0.2.csv")

ts = left_join(ts, tx)
ts = left_join(ts, tx[,c(2,3)], by = "reported_taxa")
ts$corrected_taxa = ifelse(is.na(ts$corrected_taxa.x), ts$corrected_taxa.y, ts$corrected_taxa.x)
ts$corrected_taxa = ifelse(is.na(ts$corrected_taxa), ts$reported_taxa, ts$corrected_taxa)
ts$corrected_taxa.x = NULL
ts$corrected_taxa.y = NULL

tx_df = data.frame(name = unique(ts$corrected_taxa))
tx_df$name1 = gsub("^\\s+|\\s+$", "", tx_df$name)
tx_df$count = sapply(strsplit(tx_df$name1, " "), length)
tx_df = subset(tx_df, count == 2)
#tx_search <- tnrs_match_names(names = tx_df$name1, context_name = "All life")
#saveRDS(tx_search, "produced_data/rotl_output.rds")
tx_search = readRDS("produced_data/rotl_output.rds")
#ott_in_tree <- ott_id(tx_search)[is_in_tree(ott_id(tx_search))]
#saveRDS(ott_in_tree, "produced_data/ott_output.rds")
ott_in_tree = readRDS("produced_data/ott_output.rds")
#tr <- tol_induced_subtree(ott_ids = ott_in_tree)
#saveRDS(tr, "produced_data/tree_output.rds")
tr = readRDS("produced_data/tree_output.rds")
tr = ape::compute.brlen(tr)
tip_labels = data.frame(
  tips_chr = as.factor(tr$tip.label),
  tips_num = as.numeric(as.factor(tr$tip.label))
)
tr$tip.label = as.numeric(as.factor(tr$tip.label)) 

tx_df$name1 = tolower(tx_df$name1)
tx_df = left_join(tx_df, tx_search, by = c("name1" = "search_string"))

ts = left_join(ts, tx_df[,c(1,4,6)], by = c("corrected_taxa" = "name"))
ts$X = NULL

ts$unique_site = paste0(ts$dataset_id, ts$sub_site, ts$site, ts$region)

ts = as.data.table(ts)
unique_site_list = as.list(unique(ts$unique_site))
ts_expand = lapply(unique_site_list, expand_fun)
ts_expand = rbindlist(ts_expand)
colnames(ts_expand)[1:2] = c("unique_name", "year")
write.csv(ts_expand, "produced_data/ts_expand2.csv")
ts_expand = read.csv("produced_data/ts_expand2.csv")
rm(list=setdiff(ls(), c("ott_in_tree", "sp", "ts", "ts_expand", "tx", "tx_df", "tx_search", "tr", "expand.grid.alt", "is.nan.data.frame", "norm_range", "rsq", "tip_labels", "lat_lon")))
ts_expand$year = as.integer(ts_expand$year)

#Add site back in
tmp_site_df = ts[!kit::fduplicated(ts[,c("site", "unique_site")]),c("site", "unique_site")]
ts_expand = left_join(ts_expand, tmp_site_df)

#Add units back in
tmp_unit_df = ts[!kit::fduplicated(ts[,c("unique_name", "unique_site", "unit")]),c("unique_name", "unique_site", "unit")]
ts_expand = left_join(ts_expand, tmp_unit_df)

ts_expand = left_join(ts_expand, ts[,c(1,3,5,6,7,9,10,12,14)])
ts_expand$abundance = ifelse(is.na(ts_expand$abundance), 0, ts_expand$abundance)
rm(ts, tmp_unit_df, tmp_site_df)
ts_expand$X = NULL
ts_sum = ts_expand %>%
  group_by(dataset_id, site, unique_site, unique_name, year, unit) %>%
  summarise(mean_abundance = mean(abundance, na.rm = T))
rm(ts_expand)
ts_sum = as.data.frame(ts_sum)
ts_sum = left_join(ts_sum, tx_df[!kit::fduplicated(tx_df[,c("ott_id", "unique_name")]),c("ott_id", "unique_name")])
ts_sum = left_join(ts_sum, sp[!kit::fduplicated(sp[,c(1:3)]),])

ts_norm = ts_sum %>%
  group_by(dataset_id, site, unique_site, unique_name, ott_id, country, latitude, longitude,unit) %>%
  mutate(year = year, 
         year_centre = year - mean(year),
         year_mn = mean(year),
         abundance_norm = mean_abundance/max(mean_abundance))

#Remove na species name
ts_norm = subset(ts_norm, !is.na(unique_name))
#Remove spqacies wehere only genus in known
ts_norm = ts_norm[which(sapply(strsplit(ts_norm$unique_name, " "), length) == 2),]

#Remove erronous coordinates
ts_norm$latitude = as.numeric(ts_norm$latitude)
ts_norm$longitude = as.numeric(ts_norm$longitude)
ts_norm = ts_norm[-c(which(ts_norm$latitude > 90 | ts_norm$latitude < -90 | ts_norm$longitude > 180 | ts_norm$longitude < -180)),] #Remove any locations exceeding lat and lon ranges

ts_desc = ts_norm %>%
  group_by(dataset_id, site, unique_site, unique_name, ott_id, country, latitude, longitude, unit) %>%
  summarise(year_range = max(year) - min(year), 
            count = n() - 1)
ts_desc$perc = (ts_desc$count/ts_desc$year_range)*100

ts_norm = left_join(ts_norm, ts_desc)

ts_norm$phy = paste0(gsub(" ", "_", ts_norm$unique_name), "_ott", ts_norm$ott_id)
ts_norm = left_join(ts_norm, tip_labels, by = c("phy" = "tips_chr"))
ts_norm$site_spec = paste0(ts_norm$unique_site, ts_norm$unique_name)

cite = read.csv("raw_data/combined_ct_V0.1.csv")
ts_norm = left_join(ts_norm, cite[,c(1,2)], by = "dataset_id")


tr = readRDS("produced_data/tree_output.rds")
tr = ape::compute.brlen(tr)
tip_labels = data.frame(
  tips_chr = as.factor(tr$tip.label),
  tips_num = as.numeric(as.factor(tr$tip.label))
)
tr$tip.label = as.numeric(as.factor(tr$tip.label)) 

ts_norm$abundance_norm[is.nan(ts_norm$abundance_norm)]<-NA
ts_norm_clip = subset(ts_norm, perc == 100 & !is.na(abundance_norm))

ts_zero = ts_norm_clip %>%
  group_by(site_spec) %>%
  summarise(zero = sum(mean_abundance == 0))

ts_norm_clip = left_join(ts_norm_clip, ts_zero)
ts_norm_clip$perc_zero = (((ts_norm_clip$count+1) - ts_norm_clip$zero)/(ts_norm_clip$count+1))*100
ts_norm_clip = subset(ts_norm_clip, perc_zero == 100)

ts_norm_clip2 = ts_norm_clip %>%
  group_by(site_spec) %>%
  mutate(
    abundance_norm_log = log(abundance_norm+0.01),
    abundance_norm_log_mean = mean(log(abundance_norm+0.01)),
    abundance_norm_centre = log(abundance_norm+0.01) - mean(log(abundance_norm+0.01)))
ts_norm_clip2 = subset(ts_norm_clip2, !is.na(abundance_norm))
ts_norm_clip2 = subset(ts_norm_clip2, !is.infinite(abundance_norm))

saveRDS(ts_norm_clip2, "produced_data/ts_norm7.rds")
