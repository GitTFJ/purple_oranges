Readme
================
Johnson T.F.

# Biodiversity trend analysis

This repository contains all information needed to reproduce the
analyses of \[CENSORED - AWAITING PUBLICATION\].

The repository contains four key Rmarkdown documents, which should be
run in the following order:

data_compile.Rmd - This script compiles trend datasets into one common
trend database. All of the data are openly available and the script
either automatically downloads the datasets, or when data license
agreements need to be signed (as in the Living Planet), offers clear
instructions on how the data should be downloaded and stored. Once
downloaded the datasets are cleaned and compiled in a coherent fashion.

manipulate.Rmd - This script manipulates each of the compiled datasets
into standard formats, appends spatial and phylogenetic structures,
conducts data transformations, and prepares the data for analysis

model.Rmd - This script draws on additional scripts to analyse and store
the manipulated data

visualise.Rmd - This script presents the model outputs, summaries and
figures needed to reproduce the analyses presented in \[CENSORED -
AWAITING PUBLICATION\]

Each of these scripts contains comprehensive annotation. However, if any
questions related to the analyses remain, please contact me
T.F.JOHNSON(AT)SHEFFIELD.AC.UK

This is part of a larger project aimed to improve macro-scale
bidoiversity models/statistics. If you are interested in working
together, please get in touch!

### Load packages

``` r
library(INLA)
library(brinla)
```

### Load data

``` r
df = readRDS("../data/derived_data/analysis_list.rds")[[2]] #This is the Pilotto data
```

### Set prior

``` r
prior_prec = "expression:
  log_dens = 0 - log(2) - theta / 2;
  return(log_dens);
" #This is a uniform prior set on the hyperparameters of the random effects (e.g. random effect precision)
```

### Run random intercept model

``` r
m1 = inla(log_abundance ~ #log abundance is the ressonse
            year_centre + #centred year
            f(site_spec_code,  model = "iid", constr = F, hyper = prior_prec) + #independent random intercept for each population
            f(tips_code, model = "iid", constr = F, hyper = prior_prec) + #independent random intercept for each species
            f(genus_code, model = "iid", constr = F, hyper = prior_prec) + #independent random intercept for each genera
            # Note: INLA does not require explicitly defining a nested structure in the model formula. Instead, its important to ensure the coding used for the random effects are unique and represent this nested structure. See here for guidance: https://groups.google.com/g/r-inla-discussion-group/c/vhf_qY4tvX0/m/cVXvIxbrBwAJ
            f(site_code, model = "iid", constr = F, hyper = prior_prec) +
            #independent random intercept for each site
            f(region_code, model = "iid", constr = F, hyper = prior_prec), #independent random intercept for each region
          data = df[[1]],  #Using the data from 'manipulate.Rmd'
          family = "gaussian", #A gaussian error distribution
          control.predictor=list(compute=TRUE), #Save the predicted values
          control.fixed = list(mean.intercept = 0, prec.intercept = 0.001, #Set a prior on the intercept of N(0,1000)
                               mean = 0, prec = 1), #Set a prior on the fixed effect coefficient of N(0,1)
          num.threads = 4) #Number of cores to run the model on. Lots of cores can actually slow things down, so you are better going for fewer cores IMO.
summary(m1)
```

    ## 
    ## Call:
    ##    c("inla.core(formula = formula, family = family, contrasts = contrasts, 
    ##    ", " data = data, quantiles = quantiles, E = E, offset = offset, ", " 
    ##    scale = scale, weights = weights, Ntrials = Ntrials, strata = strata, 
    ##    ", " lp.scale = lp.scale, link.covariates = link.covariates, verbose = 
    ##    verbose, ", " lincomb = lincomb, selection = selection, control.compute 
    ##    = control.compute, ", " control.predictor = control.predictor, 
    ##    control.family = control.family, ", " control.inla = control.inla, 
    ##    control.fixed = control.fixed, ", " control.mode = control.mode, 
    ##    control.expert = control.expert, ", " control.hazard = control.hazard, 
    ##    control.lincomb = control.lincomb, ", " control.update = 
    ##    control.update, control.lp.scale = control.lp.scale, ", " 
    ##    control.pardiso = control.pardiso, only.hyperparam = only.hyperparam, 
    ##    ", " inla.call = inla.call, inla.arg = inla.arg, num.threads = 
    ##    num.threads, ", " blas.num.threads = blas.num.threads, keep = keep, 
    ##    working.directory = working.directory, ", " silent = silent, inla.mode 
    ##    = inla.mode, safe = FALSE, debug = debug, ", " .parent.frame = 
    ##    .parent.frame)") 
    ## Time used:
    ##     Pre = 1.54, Running = 0.311, Post = 0.0231, Total = 1.87 
    ## Fixed effects:
    ##              mean    sd 0.025quant 0.5quant 0.975quant  mode kld
    ## (Intercept) 2.857 0.260      2.319    2.865      3.346 2.882   0
    ## year_centre 0.040 0.002      0.036    0.040      0.043 0.040   0
    ## 
    ## Random effects:
    ##   Name     Model
    ##     site_spec_code IID model
    ##    tips_code IID model
    ##    genus_code IID model
    ##    site_code IID model
    ##    region_code IID model
    ## 
    ## Model hyperparameters:
    ##                                             mean       sd 0.025quant 0.5quant
    ## Precision for the Gaussian observations 4.03e+00 1.70e-01      3.772 4.01e+00
    ## Precision for site_spec_code            1.54e+00 3.14e-01      1.049 1.50e+00
    ## Precision for tips_code                 2.21e+04 2.65e+04   2482.733 1.42e+04
    ## Precision for genus_code                1.32e+01 1.48e+01      1.183 8.78e+00
    ## Precision for site_code                 2.36e-01 3.80e-02      0.173 2.32e-01
    ## Precision for region_code               4.46e+00 6.02e+00      0.468 2.68e+00
    ##                                         0.975quant     mode
    ## Precision for the Gaussian observations   4.43e+00    3.920
    ## Precision for site_spec_code              2.29e+00    1.400
    ## Precision for tips_code                   9.11e+04 6356.429
    ## Precision for genus_code                  5.23e+01    3.261
    ## Precision for site_code                   3.23e-01    0.222
    ## Precision for region_code                 1.97e+01    1.159
    ## 
    ## Marginal log-Likelihood:  -2625.25 
    ##  is computed 
    ## Posterior summaries for the linear predictor and the fitted values are computed
    ## (Posterior marginals needs also 'control.compute=list(return.marginals.predictor=TRUE)')

### Run random slope model

``` r
message("   Model 2 - Slope model")
m2 = inla(cent_abundance ~ #Each abundance time series is centered now too. Every line passes through zero on both axes
            year_centre +
            f(site_spec_code, year_centre, model = "iid", constr = F, hyper = prior_prec) + #All random intercepts described above are now random slopes
            f(tips_code, year_centre, model = "iid", constr = F, hyper = prior_prec) +
            f(genus_code, year_centre, model = "iid", constr = F, hyper = prior_prec) +
            f(site_code, year_centre, model = "iid", constr = F, hyper = prior_prec) +
            f(region_code, year_centre, model = "iid", constr = F, hyper = prior_prec),
          data = df[[1]],  family = "gaussian", 
          control.predictor=list(compute=TRUE),
          control.fixed = list(mean.intercept = 0, prec.intercept = 0.001,
                               mean = 0, prec = 1),
          num.threads = 4)
summary(m2)
```

    ## 
    ## Call:
    ##    c("inla.core(formula = formula, family = family, contrasts = contrasts, 
    ##    ", " data = data, quantiles = quantiles, E = E, offset = offset, ", " 
    ##    scale = scale, weights = weights, Ntrials = Ntrials, strata = strata, 
    ##    ", " lp.scale = lp.scale, link.covariates = link.covariates, verbose = 
    ##    verbose, ", " lincomb = lincomb, selection = selection, control.compute 
    ##    = control.compute, ", " control.predictor = control.predictor, 
    ##    control.family = control.family, ", " control.inla = control.inla, 
    ##    control.fixed = control.fixed, ", " control.mode = control.mode, 
    ##    control.expert = control.expert, ", " control.hazard = control.hazard, 
    ##    control.lincomb = control.lincomb, ", " control.update = 
    ##    control.update, control.lp.scale = control.lp.scale, ", " 
    ##    control.pardiso = control.pardiso, only.hyperparam = only.hyperparam, 
    ##    ", " inla.call = inla.call, inla.arg = inla.arg, num.threads = 
    ##    num.threads, ", " blas.num.threads = blas.num.threads, keep = keep, 
    ##    working.directory = working.directory, ", " silent = silent, inla.mode 
    ##    = inla.mode, safe = FALSE, debug = debug, ", " .parent.frame = 
    ##    .parent.frame)") 
    ## Time used:
    ##     Pre = 1.48, Running = 0.332, Post = 0.0178, Total = 1.83 
    ## Fixed effects:
    ##              mean    sd 0.025quant 0.5quant 0.975quant  mode kld
    ## (Intercept) 0.000 0.007     -0.013    0.000      0.013 0.000   0
    ## year_centre 0.022 0.017     -0.014    0.023      0.053 0.025   0
    ## 
    ## Random effects:
    ##   Name     Model
    ##     site_spec_code IID model
    ##    tips_code IID model
    ##    genus_code IID model
    ##    site_code IID model
    ##    region_code IID model
    ## 
    ## Model hyperparameters:
    ##                                             mean       sd 0.025quant 0.5quant
    ## Precision for the Gaussian observations     8.58     0.25       8.09     8.58
    ## Precision for site_spec_code              300.93    97.75     142.71   290.20
    ## Precision for tips_code                 15993.37 17104.48    1181.22 10763.74
    ## Precision for genus_code                 3591.64 10812.91     187.44  1257.36
    ## Precision for site_code                   100.19    26.57      60.10    96.09
    ## Precision for region_code               19430.06 26722.49    1716.13 11492.28
    ##                                         0.975quant    mode
    ## Precision for the Gaussian observations       9.08    8.59
    ## Precision for site_spec_code                522.51  269.15
    ## Precision for tips_code                   61905.64 3287.80
    ## Precision for genus_code                  21502.26  403.67
    ## Precision for site_code                     163.80   87.86
    ## Precision for region_code                 87074.33 4433.09
    ## 
    ## Marginal log-Likelihood:  -1205.26 
    ##  is computed 
    ## Posterior summaries for the linear predictor and the fitted values are computed
    ## (Posterior marginals needs also 'control.compute=list(return.marginals.predictor=TRUE)')

### Run correlated effect model

``` r
m2_sig = bri.hyperpar.summary(m2)[1,4] #Residual standard deviation
ar_prior = list(theta1 = list(prior="pc.prec", param=c(m2_sig*3, 0.01)), #penalised complexity prior statement, where we state the odds of the standard deviation captured by the ar1 term exceeding 3*residual standard deviation are very small (probabaility = 0.01)
                theta2 = list(prior="pc.cor1", param=c(0, 0.9), initial = 0)) #Here we state the odds of detecting some autocorrelation (rho greater than 0) are very high (probability of 0.9). We set the inital rho at 0.

#Need aadditional indicator variables
message("   Model 3 - Correlation model")
m3 = inla(cent_abundance ~ 
            year_centre +
            f(year3, model = "ar1", replicate = site_spec_code2, hyper = ar_prior) + #We specify that abundance observations within population time-series should be ar-1 temporally autocorrelated
            f(site_spec_code, year_centre, model = "iid", 
              constr = F, hyper = prior_prec) +
            f(tips_code2, model = "generic0", 
              constr = F, Cmatrix = df[[2]], hyper = prior_prec) + #We specify that species should covary accoriding to a phylogenetic covariance matrix (Cmatrix argument). 
            f(tips_code, year_centre, model = "iid", 
              constr = F, hyper = prior_prec) +
            f(genus_code, year_centre, model = "iid", 
              constr = F, hyper = prior_prec) +
            f(site_code2, year_centre, model = "generic0", 
              constr = F, Cmatrix = df[[3]], hyper = prior_prec) + #We specify that sites should covary accoriding to a spatial covariance matrix (Cmatrix argument). 
            f(site_code, year_centre, model = "iid", 
              constr = F, hyper = prior_prec) +
            f(region_code, year_centre, model = "iid", 
              constr = F, hyper = prior_prec),
          data = df[[1]], family = "gaussian", 
          control.predictor=list(compute=TRUE),
          control.fixed = list(mean.intercept = 0, prec.intercept = 0.001,
                               mean = 0, prec = 1),
          num.threads = 4)
summary(m3)
```

    ## 
    ## Call:
    ##    c("inla.core(formula = formula, family = family, contrasts = contrasts, 
    ##    ", " data = data, quantiles = quantiles, E = E, offset = offset, ", " 
    ##    scale = scale, weights = weights, Ntrials = Ntrials, strata = strata, 
    ##    ", " lp.scale = lp.scale, link.covariates = link.covariates, verbose = 
    ##    verbose, ", " lincomb = lincomb, selection = selection, control.compute 
    ##    = control.compute, ", " control.predictor = control.predictor, 
    ##    control.family = control.family, ", " control.inla = control.inla, 
    ##    control.fixed = control.fixed, ", " control.mode = control.mode, 
    ##    control.expert = control.expert, ", " control.hazard = control.hazard, 
    ##    control.lincomb = control.lincomb, ", " control.update = 
    ##    control.update, control.lp.scale = control.lp.scale, ", " 
    ##    control.pardiso = control.pardiso, only.hyperparam = only.hyperparam, 
    ##    ", " inla.call = inla.call, inla.arg = inla.arg, num.threads = 
    ##    num.threads, ", " blas.num.threads = blas.num.threads, keep = keep, 
    ##    working.directory = working.directory, ", " silent = silent, inla.mode 
    ##    = inla.mode, safe = FALSE, debug = debug, ", " .parent.frame = 
    ##    .parent.frame)") 
    ## Time used:
    ##     Pre = 1.62, Running = 5.87, Post = 0.0964, Total = 7.59 
    ## Fixed effects:
    ##               mean    sd 0.025quant 0.5quant 0.975quant   mode kld
    ## (Intercept) -0.002 0.011     -0.025   -0.002      0.020 -0.002   0
    ## year_centre  0.022 0.020     -0.020    0.023      0.058  0.026   0
    ## 
    ## Random effects:
    ##   Name     Model
    ##     year3 AR1 model
    ##    site_spec_code IID model
    ##    tips_code2 Generic0 model
    ##    tips_code IID model
    ##    genus_code IID model
    ##    site_code2 Generic0 model
    ##    site_code IID model
    ##    region_code IID model
    ## 
    ## Model hyperparameters:
    ##                                             mean       sd 0.025quant 0.5quant
    ## Precision for the Gaussian observations 1.84e+04 3.86e+04    858.218 8.18e+03
    ## Precision for year3                     7.97e+00 2.90e-01      7.406 7.97e+00
    ## Rho for year3                           4.05e-01 2.10e-02      0.365 4.05e-01
    ## Precision for site_spec_code            5.22e+02 2.56e+02    198.437 4.66e+02
    ## Precision for tips_code2                2.46e+04 2.11e+04   2662.692 1.88e+04
    ## Precision for tips_code                 2.70e+04 2.60e+04   2949.952 1.94e+04
    ## Precision for genus_code                4.19e+04 5.80e+05    232.066 2.97e+03
    ## Precision for site_code2                2.82e+04 2.67e+04   3143.928 2.04e+04
    ## Precision for site_code                 1.07e+02 2.92e+01     60.694 1.03e+02
    ## Precision for region_code               1.42e+04 1.70e+04   1139.430 9.05e+03
    ##                                         0.975quant     mode
    ## Precision for the Gaussian observations   1.02e+05 2148.978
    ## Precision for year3                       8.55e+00    7.967
    ## Rho for year3                             4.46e-01    0.405
    ## Precision for site_spec_code              1.18e+03  374.023
    ## Precision for tips_code2                  8.01e+04 7760.167
    ## Precision for tips_code                   9.61e+04 8263.421
    ## Precision for genus_code                  2.47e+05  455.488
    ## Precision for site_code2                  9.92e+04 8832.030
    ## Precision for site_code                   1.75e+02   96.374
    ## Precision for region_code                 5.95e+04 3132.759
    ## 
    ## Marginal log-Likelihood:  -1386.45 
    ##  is computed 
    ## Posterior summaries for the linear predictor and the fitted values are computed
    ## (Posterior marginals needs also 'control.compute=list(return.marginals.predictor=TRUE)')

    ## [1] "2023-11-02 10:22:36 GMT"

Session info

    ## R version 4.2.3 (2023-03-15)
    ## Platform: aarch64-apple-darwin20 (64-bit)
    ## Running under: macOS Monterey 12.1
    ## 
    ## Matrix products: default
    ## BLAS:   /Library/Frameworks/R.framework/Versions/4.2-arm64/Resources/lib/libRblas.0.dylib
    ## LAPACK: /Library/Frameworks/R.framework/Versions/4.2-arm64/Resources/lib/libRlapack.dylib
    ## 
    ## locale:
    ## [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
    ## 
    ## attached base packages:
    ## [1] parallel  stats     graphics  grDevices utils     datasets  methods  
    ## [8] base     
    ## 
    ## other attached packages:
    ## [1] brinla_0.1.0  INLA_23.04.24 sp_2.0-0      foreach_1.5.2 Matrix_1.5-3 
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] lubridate_1.9.2    codetools_0.2-19   lattice_0.20-45    digest_0.6.33     
    ##  [5] grid_4.2.3         MatrixModels_0.5-1 evaluate_0.21      rlang_1.1.1       
    ##  [9] cli_3.6.1          rstudioapi_0.15.0  generics_0.1.3     rmarkdown_2.23    
    ## [13] splines_4.2.3      iterators_1.0.14   tools_4.2.3        Deriv_4.1.3       
    ## [17] xfun_0.39          yaml_2.3.7         fastmap_1.1.1      compiler_4.2.3    
    ## [21] timechange_0.2.0   htmltools_0.5.5    knitr_1.43
