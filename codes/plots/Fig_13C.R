# Empty the environment
rm(list = ls())

# Load libraries
library(tidyverse)
library(ggpubr)
library(ggtext)
library(cmdstanr)
library(posterior)
library(bayesplot)
library(tidybayes)
library(modelr)

# Source functions
source("Codes/DepthProfile_DataManipulation.R")

# Load and format data ----------------------------------------------------
## Data used to fit the model
D <- import.depth_profiles() %>% 
  filter(complete.cases(d13C), complete.cases(cn)) %>% 
  mutate(unit_id = paste(study, site, core, unit_nb, sep = "_")) %>% 
  group_by(unit_id) %>% 
  mutate(n = n()) %>% 
  filter(n>3) %>% 
  filter(fraction == "Bulk") %>% 
  ungroup() %>% 
  mutate(unit_id_nb = as.numeric(as.factor(unit_id)))

SOC <- D %>% group_by(unit_id, unit_id_nb) %>% 
  summarise(SOC_mean = mean(toc, na.rm = T),
            SOC_se = sd(toc, na.rm = T)/sqrt(n())) %>% 
  mutate(SOC_lower = SOC_mean - SOC_se,
         SOC_upper = SOC_mean + SOC_se) %>% 
  ungroup()

## Data for predictions
D_pred <- D %>% select(unit_id, unit_id_nb, cn) %>% 
  group_by(unit_id, unit_id_nb) %>% 
  data_grid(cn = seq_range(log(cn), 100))

SOC_pred <- seq_range(SOC$SOC_mean, 100)

# Compile the model -------------------------------------------------------
gq_13C <- cmdstan_model("Codes/Fit/GQ.stan")

## Get model results
fit_13C <- readRDS("Results/Fit/fit_13C.RDS")

fit_13C$summary() %>% filter(variable %in% c("gamma_0","gamma"))

# Compute generated quantities -------------------------------------------------
## Prepare data for stan
StanDat_gq_13C <- list(prior_only = 1,
  N = nrow(D), N_pred = nrow(D_pred), K = 1,
  U = length(unique(D$unit_id_nb)), U_pred = length(SOC_pred),
  y = D$d13C, SOC = select(SOC, SOC_mean), SOC_pred = SOC_pred, CN = log(D$cn), CN_pred = D_pred$cn,
  unit = D$unit_id_nb, unit_pred = D_pred$unit_id_nb,
  lam_sigma_beta_1 = 2,
  lam_sigma_beta_2 = 2,
  lam_sigma = 1,
  sd_gamma_0 = 2,
  sd_gamma = 2,
  mean_beta_0 = -27, sd_beta_0 = 2)

## Compute generated quantities
pred_13C <- gq_13C$generate_quantities(fit_13C, 
                                       data = StanDat_gq_13C,
                                       parallel_chains = 3)

# Plot the d13C ~ CN relationships ----------------------------------------
## Extract draws
mu_pred <- pred_13C$draws("mu_pred") %>% as_draws_matrix()
## Average predictions
mu_pred_med <- apply(mu_pred, 2, median)
## Bind them to the original data frame
D_pred$d13C <- mu_pred_med
## Join SOC values
D_pred <- left_join(D_pred, SOC)

## Plot everything
(p_cn <- D %>% ggplot(aes(x = cn, y = d13C)) + 
  geom_point(alpha = 0.3) + 
  geom_line(data = D_pred, aes(x = exp(cn), group = unit_id, color = SOC_mean), lwd = 1, alpha = 0.8) +
  theme_minimal() +
  scale_colour_viridis_c() +
  labs(x = "C/N",
     y = "*&delta;*<sup>13</sup>C (&permil;)",
     color = "SOC (%)") +
  # ggtitle("BLINDED DATA : \nNon-interpretable slopes") +
  scale_x_log10() +
  theme(axis.title.y = element_markdown())
)

# Plot the slope ~ SOC relationship ---------------------------------------
## Extract mean C content
SOC_chr <- paste("SOC_mean[", 1:max(SOC$unit_id_nb),"]", sep = "" )
SOC_sum <- fit_13C$draws(SOC_chr) %>% summarise_draws(median, mad) %>% 
  rename(SOC_mean = median, SOC_mad = mad) %>% 
  mutate(SOC_lower = SOC_mean - SOC_mad,
         SOC_upper = SOC_mean + SOC_mad)
## Extract slope draws
beta_2 <- paste("beta[", 1:max(SOC$unit_id_nb),",2]", sep = "" )
beta_sum <- fit_13C$draws(beta_2) %>% summarise_draws(median, mad) %>% 
rename(slope_med = median, slope_mad = mad) %>% 
  mutate(slope_lower = slope_med - slope_mad,
         slope_upper = slope_med + slope_mad)
## Add it to the original data.frame
SOC_slope <- bind_cols(SOC_sum, beta_sum)

## Extract prediction draws
mu_beta_pred_sum <- pred_13C$draws("mu_beta_pred") %>% 
  summarise_draws(., ~quantile2(., probs = c(0.025, 0.5, 0.975)))
## Join SOC values
SOC_pred <- bind_cols(data.frame(SOC_mean = SOC_pred), mu_beta_pred_sum)

## Plot everything
(p_slope <- SOC_slope %>% ggplot(aes(x = SOC_mean, y = slope_med)) + 
  geom_line(data = SOC_pred, aes(x = SOC_mean, y = q50), lwd = 1) +
  geom_ribbon(data = SOC_pred, aes(y = q50, ymin = q2.5, ymax = q97.5), alpha = 0.2) +
  geom_point() + 
  geom_linerange(aes(ymin = slope_lower, ymax = slope_upper), alpha = 0.5) +
  geom_linerange(aes(xmin = SOC_lower, xmax = SOC_upper), alpha = 0.5) +
  geom_hline(yintercept = 0, linetype =  2) +  
  labs(x = "Mean unit SOC (%)",
       y = "Slope between *&delta;*<sup>13</sup>C and C/N") +
  # ggtitle("BLINDED DATA : \nNon-interpretable slopes") +
  theme_minimal() +
  theme(axis.title.y = element_markdown())
)

# Get the two plots together and save ----------------------------------------------
(p_13C <- ggarrange(p_cn, p_slope, labels = c("a", "b")))

ggsave("Plots/Fig_13C.png", p_13C, height = 4, width = 7.5, bg = "white")


