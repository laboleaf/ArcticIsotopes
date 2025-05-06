# Function importing and joining the different depth profile datasets -----------------
## THe data sets will have the following standard columns
## study; site; core; depth_up; depth_low; depth; fraction; cn; d13c; d15N; toc
import.depth_profiles <- function(){
  ## Load the needed libraries
  require(tidyverse)
  require(readxl)
  
# Import data from Andersson et al. 2012 Jour. Quat Science ---------------
  Andersson_raw <- read_xlsx("Data/CSV_Litterature/Andersson_2012_JQS.xlsx", 
                             sheet = "Data")
  
  # pairs(Andersson_raw %>% select(depth_1, depth_2, depth_3, depth_4))
  
  ## Put data in a standard format
  Andersson <- Andersson_raw %>% 
    mutate(depth = rowMeans(data.frame(depth_1, depth_2, depth_3, depth_4)),
           fraction = "Bulk", depth_up = NA, depth_low = NA,
           core = as.character(core)) %>% 
    select(study, site, core, unit, unit_code, unit_nb, depth_up, depth_low, depth, fraction, 
           cn, d13C, d15N, toc)
  summary(Andersson)
  
# Import data from HicksPries et al. 2012 Ecosystems ---------------
  HicksPries_raw <- read_xlsx("Data/CSV_Litterature/HicksPries_Crummer_2012_Ecosystems_DATA.xlsx", 
                             sheet = "Data")
  
  # pairs(HicksPries_raw %>% select(depth_1, depth_2, depth_3))
  
  # Put data in a standard format
  HicksPries <- HicksPries_raw %>% 
    mutate(depth = rowMeans(data.frame(depth_1, depth_2, depth_3, na.rm = T)),
           fraction = "Bulk", depth_up = NA, depth_low = NA,
           core = as.character(core)) %>% 
    select(study, site, core, unit, unit_code, unit_nb, depth_up, depth_low, depth, fraction, 
           cn, d13C, d15N, toc)
  summary(HicksPries)
  
# Import data from Hugelius et al. 2012 Jour Geophysical Sci : BGS ---------------
  Hugelius_raw <- read_xlsx("Data/CSV_Litterature/Hugelius_Crill_2012_JGSBGS_DATA.xlsx", 
                              sheet = "Data")
  
  # pairs(Hugelius_raw %>% select(depth_1, depth_2, depth_3))
  
  # Put data in a standard format
  Hugelius <- Hugelius_raw %>% 
    mutate(depth = rowMeans(data.frame(depth_1, depth_2, depth_3, depth_4, na.rm = T)),
           fraction = "Bulk", depth_up = NA, depth_low = NA,
           core = as.character(core)) %>% 
    select(study, site, core, unit, unit_code, unit_nb, depth_up, depth_low, depth, fraction, 
           cn, d13C, d15N, toc)
    summary(Hugelius)
    
# Import data from Hutchings et al. 2019 Geochemica Cosmochimica Acta --------
  Hutchings_raw <- read_xlsx("Data/CSV_Litterature/Hutchings_Schuur_2019_GeoCosmoActa_DATA.xlsx", sheet = "bulk")
  
  ## Put data in a standard format
  Hutchings <- Hutchings_raw %>% 
    select(core, d.mid, unit_nb, cn, d13C, toc) %>%  ## Select needed columns
    filter(complete.cases(cn), complete.cases(d13C)) %>% ## Keep values with d13C and C/N
    mutate(study = "Hutchings2019231", site = "EML", unit = NA, unit_code = NA, fraction = "Bulk", depth_up = NA, depth_low = NA, d15N = NA,
           core = as.character(core)) %>% ## Create needed columns and ID
    rename(depth = d.mid) %>% ## Rename columns
    select(study, site, core, unit, unit_code, unit_nb, depth_up, depth_low, depth, fraction, 
           cn, d13C, d15N, toc)
    summary(Hutchings)
    
# Import data from Kruger et al. 2014 Biogesciences --------
  Kruger_raw <- read_xlsx("Data/CSV_Litterature/Kruger_Alewell_2014_BGS_DATA.xlsx")
  
  ## Put data in a standard format
  Kruger <- Kruger_raw %>% 
    mutate(d13C = -d13C) %>% 
    mutate(study = "Krüger20143369", unit = NA, unit_code = NA, unit_nb = 1, fraction = "Bulk", depth_up = NA, depth_low = NA,
           core = as.character(core)) %>% 
    select(study, site, core, unit, unit_code, unit_nb, depth_up, depth_low, depth, fraction,cn, d13C, d15N, toc)
    summary(Kruger)
    
# Import data from Li et al. 2014 STOTEN --------
  Li_raw <- read_xlsx("Data/CSV_Litterature/Li_Chen_2020_STOTEN_DATA.xlsx")
  
  ## Put data in a standard format
  Li <- Li_raw %>% 
    select(site, core, `13C`, `C/N`, Deep, SOC_g_kg) %>% 
    mutate(study = "Li2020", unit = NA, unit_code = NA, unit_nb = 1, fraction = "Bulk", depth_up = NA, depth_low = NA, d15N = NA,
           core = as.character(core),
           toc = SOC_g_kg/1000*100) %>% 
    rename(depth = Deep, cn = `C/N`, d13C = `13C`) %>% 
    select(study, site, core, unit, unit_code, unit_nb, depth_up, depth_low, depth, fraction,cn, d13C, d15N, toc)
    summary(Li)

# Import data from Oelbermann et al. 2008 Can Jour Soil Science ---------------
  Oelbermann_raw <- read_xlsx("Data/CSV_Litterature/Oelbermann_Schiff_2008_CJSS_DATA.xlsx", 
                            sheet = "Data")
  
  # pairs(Oelbermann_raw %>% select(depth_1, depth_2))
  
  # Put data in a standard format
  Oelbermann <- Oelbermann_raw %>% 
    mutate(depth = rowMeans(data.frame(depth_1, depth_2, depth_3, depth_4, na.rm = T)),
           fraction = "Bulk", depth_up = NA, depth_low = NA,
           core = as.character(core)) %>% 
    select(study, site, core, unit, unit_code, unit_nb, depth_up, depth_low, depth, fraction, 
           cn, d13C, d15N, toc)
  summary(Oelbermann)
  
# Import data from Pashtukhov et al. 2018 EurasianSoilScience --------
  Pasthukov_raw <- read_xlsx("Data/CSV_Litterature/Pasthukov_Kaverin_2018_EurasianSoilScience_DATA.xlsx")
  
  ## Put data in a standard format
  Pasthukov <- Pasthukov_raw %>% 
    mutate(study = "Pastukhov2020", unit = NA, unit_code = NA, unit_nb = 1, fraction = "Bulk", depth = rowMeans(data.frame(depth_up, depth_low)),
           core = as.character(core)) %>%
    select(study, site, core, unit, unit_code, unit_nb, depth_up, depth_low, depth, fraction,cn, d13C, d15N, toc)
    summary(Pasthukov)
    
# Import data from Prater et al. 2020 Biogeosciences --------
  Prater_raw <- read_xlsx("Data/CSV_Litterature/Prater_Mueller_2020_BGS_DATA.xlsx")
  
  ## Put data in a standard format
  Prater <- Prater_raw %>% 
    select(core:d15N, C_mg_g, Mass_mg_g, cn_core) %>% 
    mutate(study = "Prater2020", site = "Samoylov", unit = NA, unit_code = NA, unit_nb = 1,
           depth = rowMeans(data.frame(depth_up, depth_low)),
           core = as.character(core),
           toc = C_mg_g/1000*100) %>%
    group_by(study, site, core, unit, unit_code, unit_nb, depth_up, depth_low, depth) %>% 
    summarise(cn = weighted.mean(cn, Mass_mg_g, na.rm = T),
              cn_core = unique(cn_core),
              d13C = weighted.mean(d13C, Mass_mg_g, na.rm = T),
              d15N = weighted.mean(d15N, Mass_mg_g, na.rm = T),
              toc = weighted.mean(toc, Mass_mg_g, na.rm = T)) %>% 
    ungroup() %>% 
    mutate(fraction = "Bulk") %>% 
    select(study, site, core, unit, unit_code, unit_nb, depth_up, depth_low, depth, fraction,cn, d13C, d15N, toc)
    summary(Prater)
    
# Import data from Ravn et al. 2020 Soil Biol Biochem---------------
  Ravn_raw <- read_xlsx("Data/CSV_Litterature/Ravn_Michelsen_2020_SBB_DATA.xlsx", 
                              sheet = "Data")
  
  # pairs(Ravn_raw %>% select(depth_1, depth_2))
  
  # Put data in a standard format
  Ravn <- Ravn_raw %>% 
    mutate(depth = rowMeans(data.frame(depth_1, depth_2, depth_3, depth_4, na.rm = T)),
           fraction = "Bulk", depth_up = NA, depth_low = NA,
           core = as.character(core)) %>% 
    select(study, site, core, unit, unit_code, unit_nb, depth_up, depth_low, depth, fraction, 
           cn, d13C, d15N, toc)
  summary(Ravn)
  
# Import data from Schirrmeister et al. 2011 Journal Gerophy Research ---------------
  Schirrmeister_raw <- read_xlsx("Data/CSV_Litterature/Schirrmeister_2011_JGR_DATA.xlsx", 
                        sheet = "Data", na= c("", "NA"))
  
  # pairs(Schirrmeister_raw %>% select(depth_1, depth_2, depth_4) %>% filter(complete.cases(.)))
  
  # Put data in a standard format
  Schirrmeister <- Schirrmeister_raw %>% 
    mutate(depth = rowMeans(data.frame(depth_1, depth_2, depth_4, na.rm = T)),
           fraction = "Bulk", depth_up = NA, depth_low = NA,
           core = as.character(core)) %>% 
    select(study, site, core, unit, unit_code, unit_nb, depth_up, depth_low, depth, fraction, 
           cn, d13C, d15N, toc)
  summary(Schirrmeister)
  
# Import data from Schirrmeister et al. 2016 Arct. Alp. Ant. Res ---------------
  Schirrmeisterb_raw <- read_xlsx("Data/CSV_Litterature/Schirrmeister_Wetterich_2018_AAAR_DATA.xlsx", 
                                 sheet = "Data", na= c("", "NA"))
  
  # pairs(Schirrmeisterb_raw %>% select(depth_1, depth_2, depth_4) %>% filter(complete.cases(.)))
  
  # Put data in a standard format
  Schirrmeisterb <- Schirrmeisterb_raw %>% 
    mutate(fraction = "Bulk", depth_up = NA, depth_low = NA,
           core = as.character(core), d15N = NA) %>% 
    mutate(across(c(depth, cn, d13C, toc), ~as.numeric(.x))) %>% 
    select(study, site, core, unit, unit_code, unit_nb, depth_up, depth_low, depth, fraction, 
           cn, d13C, d15N, toc)
  summary(Schirrmeisterb)
  
# Import data from Skrzypek et al. 2011 Polish Polar Research ---------------
  Skrzypek_raw <- read_xlsx("Data/CSV_Litterature/Skrzypek_Wotjun_2008_PPR_DATA.xlsx", 
                                 sheet = "Data", na= c("", "NA"))
  
  # pairs(Skrzypek_raw %>% select(depth_1, depth_2, depth_4) %>% filter(complete.cases(.)))
  
  # Put data in a standard format
  Skrzypek <- Skrzypek_raw %>% 
    mutate(depth = rowMeans(data.frame(depth_1, depth_2, depth_3, depth_4, na.rm = T)),
           fraction = "Bulk", depth_up = NA, depth_low = NA,
           core = as.character(core)) %>% 
    select(study, site, core, unit, unit_code, unit_nb, depth_up, depth_low, depth, fraction, 
           cn, d13C, d15N, toc)
  summary(Skrzypek)
  
# Import data from Stapel et al. 2016 JGR Biogeosciences ---------------
  Stapel_raw <- read_xlsx("Data/CSV_Litterature/Stapel_Mangelsdorf_2016_JGRBGS_DATA.xlsx", 
                                  sheet = "Data", na= c("", "NA"))
  
  # pairs(Stapel_raw %>% select(depth_1, depth_2, depth_4) %>% filter(complete.cases(.)))
  
  # Put data in a standard format
  Stapel <- Stapel_raw %>% 
    mutate(fraction = "Bulk", depth_up = NA, depth_low = NA,
           core = as.character(core), d15N = NA) %>% 
    mutate(across(c(depth, cn, d13C, toc), ~as.numeric(.x))) %>% 
    select(study, site, core, unit, unit_code, unit_nb, depth_up, depth_low, depth, fraction, 
           cn, d13C, d15N, toc)
  summary(Stapel)

# Import data from Strauss et al. 2011 Biogeosciences ---------------
  Strauss_raw <- read_xlsx("Data/CSV_Litterature/Strauss_Herzschuh_2015_BGS.xlsx", 
                            sheet = "Data", na= c("", "NA"))
  
  # pairs(Strauss_raw %>% select(depth_1, depth_2, depth_4) %>% filter(complete.cases(.)))
  
  # Put data in a standard format
  Strauss <- Strauss_raw %>% 
    mutate(depth = rowMeans(data.frame(depth_1, depth_2, depth_4, na.rm = T)),
           fraction = "Bulk", depth_up = NA, depth_low = NA,
           core = as.character(core)) %>% 
    select(study, site, core, unit, unit_code, unit_nb, depth_up, depth_low, depth, fraction, 
           cn, d13C, d15N, toc)
  summary(Strauss)
  
# Import data from Wetterich et al. 2017 Perm. and Peri. Proc. ---------------
  Wetterich_raw <- read_xlsx("Data/CSV_Litterature/Wetterich_Meyer_2017_PPP_DATA.xlsx", 
                          sheet = "Data", na= c("", "NA"))
  
  # pairs(Wetterich_raw %>% select(depth_1, depth_2, depth_4) %>% filter(complete.cases(.)))
  
  # Put data in a standard format
  Wetterich <- Wetterich_raw %>% 
    mutate(fraction = "Bulk", depth_up = NA, depth_low = NA,
           core = as.character(core), d15N = NA) %>% 
    mutate(across(c(depth, cn, d13C, toc), ~as.numeric(.x))) %>% 
    select(study, site, core, unit, unit_code, unit_nb, depth_up, depth_low, depth, fraction, 
           cn, d13C, d15N, toc)
  summary(Wetterich)
  
# Import data from Wild et al. 2018 Env. Res Letter ---------------
  Wild_raw <- read_xlsx("Data/CSV_Litterature/Wild_Richter_2018_ERL_DATA.xlsx", 
                           sheet = "Data", na= c("", "NA"))
  
  # pairs(Wild_raw %>% select(depth_1, depth_2, depth_3, depth_4) %>% filter(complete.cases(.)))
  
  # Put data in a standard format
  Wild <- Wild_raw %>% 
    mutate(depth = rowMeans(data.frame(depth_1, depth_2, depth_4, na.rm = T)),
           fraction = "Bulk", depth_up = NA, depth_low = NA,
           core = as.character(core)) %>% 
    select(study, site, core, unit, unit_code, unit_nb, depth_up, depth_low, depth, fraction, 
           cn, d13C, d15N, toc)
  summary(Wild)
  
# Import data from Windirsch et al. 2018 M. Sc Thesis ---------------
  Windirsch_raw <- read_xlsx("Data/CSV_Litterature/Windirsch_2018_Thesis_DATA.xlsx", 
                             sheet = "Data", na= c("", "NA"))
  
  # pairs(Windirsch_raw %>% select(depth_1, depth_2, depth_4) %>% filter(complete.cases(.)))
  
  # Put data in a standard format
  Windirsch <- Windirsch_raw %>% 
    mutate(fraction = "Bulk", depth_up = NA, depth_low = NA,
           core = as.character(core), d15N = NA) %>% 
    mutate(across(c(toc, `TN [%]`), ~ifelse(.x == "<0.100", 0.100, .x))) %>% 
    mutate(across(c(depth, cn, d13C, toc, `TN [%]`), ~as.numeric(.x)),
           cn = toc/`TN [%]`) %>% 
    select(study, site, core, unit, unit_code, unit_nb, depth_up, depth_low, depth, fraction, 
           cn, d13C, d15N, toc)
  summary(Windirsch)
  
# Import data from Xu et al. 2009 Journal of Geophysical Research --------
  Xu_raw <- read_xlsx("Data/CSV_Litterature/Xu_White_2009_JGR_DATA.xlsx")
  
  ## Put data in a standard format
  Xu <- Xu_raw %>% 
    select(-horizon) %>% 
    mutate(study = "Xu2009", site = "MAT", unit = NA, unit_code = NA, unit_nb = 1,
           depth = rowMeans(data.frame(depth_up, depth_low)),
           core = as.character(core),
           toc = soc_g_kg/1000*100) %>%
    select(study, site, core, unit, unit_code, unit_nb, depth_up, depth_low, depth, fraction,cn, d13C, d15N, toc)
    
  ## Bind data frames together
  D <- bind_rows(Andersson, HicksPries, Hugelius, Hutchings, Kruger, Li, Oelbermann,
                 Pasthukov, Prater, Ravn, Schirrmeister, Schirrmeisterb, Stapel,
                 Skrzypek, Strauss, Wild, Wetterich, Windirsch, Xu) %>% 
    mutate(study_num = as.numeric(factor(study)))
  
  return(D)
}

# Function to blind data --------------------------------------------------
import.depth_profiles.BLIND <- function(D){
  require(tidyverse)
  
  ## Randomize variables of interests
  D_Blind <- D %>% mutate(
    cn = D %>% mutate(unit_id = paste(study, site, core, unit_nb, sep = "_")) %>% 
      group_by(unit_id) %>% sample_n(n()) %>% pull(cn),
    d13C = D %>% mutate(unit_id = paste(study, site, core, unit_nb, sep = "_")) %>% 
      group_by(unit_id) %>% sample_n(n()) %>% pull(d13C),
    d15N = D %>% mutate(unit_id = paste(study, site, core, unit_nb, sep = "_")) %>% 
      group_by(unit_id) %>% sample_n(n()) %>% pull(d15N),
  )
}

