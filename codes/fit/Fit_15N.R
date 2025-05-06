# Empty the environment
rm(list = ls())

# Load libraries
library(tidyverse)
library(cmdstanr)
library(posterior)
library(bayesplot)

# Source functions
source("Codes/DepthProfile_DataManipulation.R")

# Load and format data ----------------------------------------------------
D <- import.depth_profiles() %>% 
  filter(complete.cases(d15N), complete.cases(cn)) %>% 
  mutate(unit_id = paste(study, site, core, unit_nb, sep = "_")) %>% 
  group_by(unit_id) %>% 
  mutate(n = n()) %>% 
  filter(n>4) %>% 
  ungroup() %>% 
  mutate(unit_id_nb = as.numeric(as.factor(unit_id))) %>% 
  filter(fraction == "Bulk") %>% 
  ## Fill NA in SOC
  group_by(unit_id) %>% 
  mutate(toc_mean = mean(toc, na.rm = T)) %>% 
  mutate(toc = ifelse(is.na(toc), toc_mean, toc),
         toc = ifelse(toc <= 0, 0.01, toc))

# Compile the model -------------------------------------------------------
mod_15N <- cmdstan_model("Codes/Fit/HierarchicalModel.stan")

# Fit -------------------------------------------------
## Prepare data for stan
StanDat_fit_15N <- list(prior_only = 0,
                        N = nrow(D), U = length(unique(D$unit_id_nb)),
                        y = D$d15N, SOC = D$cn, CN = log(D$cn), unit = D$unit_id_nb,
                        lam_sigma_beta_1 = 2,
                        lam_sigma_beta_2 = 2,
                        lam_sigma = 1,
                        sd_gamma_0 = 3,
                        sd_gamma = 2,
                        mean_beta_0 = 0, sd_beta_0 = 3)

## Sample from the posterior distribution
fit_15N <- mod_15N$sample(data = StanDat_fit_15N,
                          chains = 3, parallel_chains = 3,
                          iter_warmup = 2000, iter_sampling = 2000,
                          adapt_delta = 0.9)
## Explore diagnostics
fit_15N$cmdstan_diagnose()

fit_15N$draws("rho") %>% summarise_draws()

mu_15N_draws <- as_draws_matrix(fit_15N$draws("mu"))
yrep_15N_draws <- as_draws_matrix(fit_15N$draws("y_rep"))
samples <- sample(1:nrow(yrep_15N_draws), 200)
## Overlay of density functions
ppc_dens_overlay(y = D$d15N, mu_15N_draws[samples,])
ppc_dens_overlay(y = D$d15N, yrep_15N_draws[samples,])
## Observed vs. predicted
ppc_scatter_avg(y = D$d15N, mu_15N_draws[samples,])
## Distribution of the errors
ppc_error_scatter_avg(y = D$d15N, mu_15N_draws[samples,])

## Standard deviations
mcmc_areas(fit_15N$draws(c("sigma", "sigma_beta")))
## Intercepts
mcmc_areas(fit_15N$draws(c("beta_0")))
mcmc_areas(fit_15N$draws(c("gamma_0")))
beta_1 <- paste("beta[", 1:max(D$unit_id_nb),",1]", sep = "" )
mcmc_intervals(fit_15N$draws(beta_1))
## Slopes
beta_2 <- paste("beta[", 1:max(D$unit_id_nb),",2]", sep = "" )
mcmc_intervals(fit_15N$draws(beta_2))
mcmc_areas(fit_15N$draws(c("gamma")))

## Save model results
fit_15N$save_object("Results/Fit/fit_15N.RDS")

