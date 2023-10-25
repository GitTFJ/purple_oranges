# Biodiversity abundance trend analysis

This repository contains all information needed to reproduce the analyses of [CENSORED - AWAITING PUBLICATION]. 

The repository contains four key Rmarkdown documents, which should be run in the following order:

**data_compile.Rmd** - This script compiles trend datasets into one common trend database. All of the data are openly available and the script either automatically downloads any datasets, or when data license agreements need to be signed (as in the Living Planet), offers clear instructions on how the data should be downloaded and stored. Once downloaded the datasets are cleaned and stored in a coherent fashion.

**manipulate.Rmd** - This script manipluates each of the compiled datasets into standard formats, appends spatial and phylogenetic structures, conducts data transformations, and prepares the data for analysis

**model.Rmd** - This script draws on additional scripts to analyse and store the manipulated data

**visualise.Rmd** - This script presents the model outputs, summaries and figures needed to reproduce the analyses presented in [CENSORED - AWAITING PUBLICATION]

Each of these scripts contains comprehensive annotation. However, if any questions related to the analyses, remaine, please contact me T.F.JOHNSON(AT)SHEFFIELD.AC.UK

This is part of a larger project aimed to improve macro-scale bidoiversity models/statistics. If you are interested in working together, please get in touch!
