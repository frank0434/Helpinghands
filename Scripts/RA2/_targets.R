# _targets.R file
library(targets)
# library(future)
# library(future.batchtools)
# library(slurmR)

source(here::here("R/functions.R"))
options(tidyverse.quiet = TRUE)

tar_option_set(packages = c("tidyverse", "sensobol","data.table"))

list(
# input files  ------------------------------------------------------------
  tar_target(fig1.coefs_path,
             "Data/Jochum_2017/Jochum_JAE2017_Fig1_richness_coefs_confint.csv",
             format = "file"
             ),
  tar_target(CDI_path, 
             "Data/LIDET-CDI.csv", 
             format = "file"
             ),
  tar_target(weather_path, 
             "Data/Weather.csv", 
             format = "file"
             ),
  tar_target(fig1.coefs,
             read.csv(fig1.coefs_path)
             ),
  tar_target(data_CDI, 
             read.csv(CDI_path)
             ),
  tar_target(data_weather, 
             read.csv(weather_path)
             ),

# simulated data ----------------------------------------------------------
  tar_target(x, nrow(data_CDI)),
  tar_target(site_name, data_CDI$Site_Code),
  tar_target(coefs, subset(fig1.coefs %>% 
                             mutate(mean = confint_upper_boundary,
                                    sd = confint_lower_boundary), 
                           extrap_richness_response_group == "all")),
  tar_target(data, 
             tibble::tibble(
              LM = rnorm(x, mean = 10, sd = 4), # Adjusted to make the output more ecologically meaningful
              prich = rpois(x,5),
              pH = rpois(x,5), # coefficient = 0
              'C:N'  = rnorm(x),
              'C:P'  = rnorm(x, sd = 5), # Adjusted to make the output more ecologically meaningful
              'C:K'  = rnorm(x),
              'C:Ca' = rnorm(x),
              'C:Mg' = rnorm(x),
              'C:Na' = rnorm(x),
              'C:S'  = rnorm(x))
  ),
# Sensitivities in one site 
  tar_target(dat, data.table(data)[1] %>% 
               melt.data.table(variable.name = "params")),
  # Config the sampling matrics
  tar_target(config, list(N = 2 ^ 13, # sample size
                          params = coefs$predictor, # parameter names 
                          matrices = c("A", "B", "AB", "BA"), # sampling design
                          first = "azzini", # first order effect estimator
                          total = "azzini", # total order effect estimator
                          order = "second", # also want second order effect
                          R = 10 ^ 3, # bootsrap the indeces 10^3 times
                          type = "percent",
                          conf = 0.95)), # confidence intervals
  # Construct the sampling matrics 
  tar_target(matrix, sobol_matrices(matrices = config$matrices, 
                                    N = config$N, params = config$params,
                                    order = config$order, type = "LHS")), 
  tar_target(mat, fill_mat(matrix, distribution = coefs)),
  tar_target(y, model_richness_fun(mat, dat)),

# model 1
  tar_target(richness,
             model_richness(data, coefs, site_name = site_name)),
# model 2
  tar_target(litter_decomposition_rate, 
             decomposition(taxonomic.richness = richness,
                           AllCDIs = data_CDI)),
# model 3
  tar_target(litterMass, 
             surfaceOM(Weather = data_weather,
                       decomposition = litter_decomposition_rate)),
# output some plots
  tar_target(outputplots,
             output_plots(litterMass, site_name)),

# render reports 
  tarchetypes::tar_render(SA_report, "Reports/report_template.Rmd")


)



