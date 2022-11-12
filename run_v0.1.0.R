# Step 0 - Preperation ----

##Set working directory ----
setwd("...")

##Create directory structure ----
dir.create("raw_data")
dir.create("produced_data")
dir.create("outputs")

##Load all packages and functions ----
source("code/load_packages_v0.1.0.R") 

##Manually add curated datasets to the 'raw_data' directory

#Step 1 - Manipulation ----
source("code/manipulate_data_v0.1.1.R") 

##Load tree ----
tr = readRDS("produced_data/tree_output.rds")

##Load abundances ----
ts_norm_clip2 = readRDS("produced_data/ts_norm7.rds")

#Step 2 - Run models for each dataset
source("code/modelling1_v0.1.1.R") 

#Step 3 - Run biotime saving all outputs to plot simultations
source("code/modelling2_v0.1.0.R") 

#Step 4 - Run biotime removing some abundance values
source("code/modelling3_v0.1.0.R") 

#Step 5 - Run biotime removing some population trends
source("code/modelling4_v0.1.0.R") 

#Step 6 - Extract outputs from step 2
source("code/output1_v0.1.0.R") 

#Step 7 - Plots from step 2 (Figure 2)
source("code/output1_v0.1.0.R") 

#Step 8 - Plots from step 3 (Figure 3)
source("code/output1_v0.1.0.R") 

#Step 9 - Plots from step 4 (Figure 4)
source("code/output1_v0.1.0.R") 

#Step 10 - Plots from step 4 (Figure 4)
source("code/output1_v0.1.0.R") 







