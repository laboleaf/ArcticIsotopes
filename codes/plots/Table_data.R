# Empty the environment
rm(list = ls())

# Load libraries
library(tidyverse)
# library(kableExtra)

# Source functions
source("Codes/DepthProfile_DataManipulation.R")

# https://haozhu233.github.io/kableExtra/awesome_table_in_pdf.pdf

# Load and format data ----------------------------------------------------
D <- import.depth_profiles() %>% 
  filter(complete.cases(cn)) %>% 
  mutate(unit_id = paste(study, site, core, unit_nb, sep = "_")) %>% 
  group_by(unit_id) %>% 
  mutate(n = n()) %>% 
  filter(n>4) %>% 
  ungroup() %>% 
  mutate(unit_id_nb = as.numeric(as.factor(unit_id)))

OM_list <- split(D$toc, D$study)

Table <- D %>% 
  group_by(unit_id) %>% 
  mutate(toc_mean = mean(toc, na.rm = T)) %>% 
  mutate(core_id = paste(site, core), 
         unit_id_study = paste(site, core, unit_nb)) %>% 
  group_by(study) %>% 
  summarise(n_site = length(unique(site)),
            n_core = length(unique(core_id)),
            n_unit = length(unique(unit_id_study)),
            OM = paste0(round(mean(toc_mean, na.rm = T), 2), " (", 
                        round(min(toc_mean, na.rm = T), 2), "-", 
                        round(max(toc_mean, na.rm = T), 2), ")")
            )

write_csv2(Table, "Plots/Table_data.csv")
  # kbl(booktabs = TRUE) %>%
  # kable_paper(full_width = FALSE) %>%
  # column_spec(5, image = spec_hist(OM_list, 300, 500, xaxt = "s"))
  
  

