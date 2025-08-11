
# DATE LAST UPDATED: 07/18/2025
# PROJECT: AI Employment Impacts
# AUTHOR: Sarah Eckhardt
# DESCRIPTION: construct all crosswalks required for analysis:
# (a) O*NET based crosswalk for SOC 2010 -> SOC 2018
# using O*NET job information distribution similarity to construct crosswalk weights
# SOURCES:
# O*NET https://www.onetcenter.org/database.html
# XWALKS  https://www.bls.gov/soc/2018/soc_2010_to_2018_crosswalk.xlsx
# (b) Census 2010 -> Census 2018 codes 
# (c) industry based crosswalks

# a 5th uses David Dorn 1990; which is set; just needs the 1990 based occupation


rm(list = ls())

# packages
library(tidyr)
library(dplyr)
library(readxl)
library(stringr)

# set project paths
user_path = "ENTER-USER-PATH"
project_path = file.path(user_path, "AI-Unemployment")
data_path = file.path(project_path, "data/1raw")
wrangled_path = file.path(project_path, "data/2wrangled")


################################################################################
################################################################################
                  # read in admin crosswalk and clean up #
################################################################################
################################################################################

# read in administrative soc 2010 to soc 2018
xwalk_soc2010_soc2018 <- read_excel(file.path(data_path, "soc_2010_to_2018_crosswalk.xlsx"),
                                    skip=8) %>%
  rename(soc_2010 = `2010 SOC Code`, soc_title_2010 = `2010 SOC Title`, soc_2018 = `2018 SOC Code`, soc_title_2018 = `2018 SOC Title`)


# clean up 2018 SOC -> Census 2018 crosswalk from Census
xwalk_soc2018_cen2018 = read_excel(file.path(data_path, "2018-occupation-code-list-and-crosswalk.xlsx"),
                                   sheet = "2018 Census Occ Code List", skip = 4) %>% 
  select(-c(...1)) %>% na.omit() %>% filter(!grepl(":", `2018 Census Title`),`2018 SOC Code` != "none")

# the X,XX, and end in 0 are placeholders
soc2018_list = xwalk_soc2010_soc2018 %>%
  select(contains("2018")) %>% distinct() %>%
  mutate(trunc_3dig = paste0(substr(soc_2018, 1, 4),"XXX"),
         trunc_2dig = paste0(substr(soc_2018, 1, 5), "XX"),
         trunc_1dig = paste0(substr(soc_2018, 1, 6),"X"))

soc2018_list_trunc3 = soc2018_list %>% select(full_2018_3dig =soc_2018, trunc_3dig) %>% distinct()
soc2018_list_trunc2 = soc2018_list %>% select(full_2018_2dig =soc_2018, trunc_2dig) %>% distinct()
soc2018_list_trunc1 = soc2018_list %>% select(full_2018_1dig=soc_2018, trunc_1dig) %>% distinct()

xwalk_soc2018_cen2018 = xwalk_soc2018_cen2018 %>%
  mutate(`2018 SOC Code 2` = gsub("0$", "X", `2018 SOC Code`)) %>%
  mutate(`2018 SOC Code 2` = ifelse(`2018 SOC Code 2` == "25-100X", "25-1XXX", `2018 SOC Code 2`),
         `2018 SOC Code 2` = ifelse(`2018 SOC Code 2` == "29-900X", "29-90XX", `2018 SOC Code 2`),
         `2018 SOC Code 2` = ifelse(`2018 SOC Code 2` == "39-100X", "39-10XX", `2018 SOC Code 2`),
         `2018 SOC Code 2` = ifelse(`2018 SOC Code 2` == "53-100X", "53-1XXX", `2018 SOC Code 2`)) %>%
  
  left_join(soc2018_list_trunc1, by = c("2018 SOC Code 2" = "trunc_1dig")) %>%
  left_join(soc2018_list_trunc2, by = c("2018 SOC Code 2" = "trunc_2dig")) %>% 
  left_join(soc2018_list_trunc3, by = c("2018 SOC Code 2" = "trunc_3dig")) %>%
  
  mutate(soc_2018 = case_when(
    !is.na(full_2018_1dig) ~ full_2018_1dig,
    !is.na(full_2018_2dig) ~ full_2018_2dig,
    !is.na(full_2018_3dig) ~ full_2018_3dig,
    TRUE ~ `2018 SOC Code`
  )) %>% select(census_2018 = `2018 Census Code`, title_census_2018 = `2018 Census Title`, soc_2018)


# generate starting soc2010 - cen2018 crosswalk;  built from administrative files
xwalk_soc2010_cen2018 = xwalk_soc2010_soc2018 %>%
  full_join(xwalk_soc2018_cen2018, by = "soc_2018")

length(unique(xwalk_soc2018_cen2018$soc_2018))
length(unique(xwalk_soc2010_soc2018$soc_2018))



#################################################################################
# read in the O*NET actity data
# convert values to importance weights
# aggregate at 6 - digit


onet_soc2010_ability <- read_excel(file.path(data_path, "db_25_0_excel", "Abilities.xlsx")) %>%
  filter(`Scale Name` == "Importance") %>%
  select(
    soc8dig_2010 = `O*NET-SOC Code`,
    element_id = `Element Name`,
    importance = `Data Value`
  ) %>%
  # Step 1: Create 6-digit SOC code
  mutate(soc_2010 = substr(soc8dig_2010, 1, 7)) %>%
  # Step 2: Average importance across 8-digit SOCs within 6-digit SOC + ability
  group_by(soc_2010, element_id) %>%
  summarise(mean_importance = mean(importance, na.rm = TRUE), .groups = "drop") %>%
  
  # Step 3: Normalize importance *across all occupations* within each ability (to reflect relative importance)
  group_by(element_id) %>%
  mutate(
    norm_importance = (mean_importance - min(mean_importance, na.rm = TRUE) + 1e-6) /
      (max(mean_importance, na.rm = TRUE) - min(mean_importance, na.rm = TRUE) + 1e-6)
  ) %>%
  ungroup() %>%
  
  # Step 4: Within each SOC, rescale to sum to 1 across abilities
  group_by(soc_2010) %>%
  mutate(
    weight = norm_importance / sum(norm_importance, na.rm = TRUE)
  ) %>%
  ungroup() %>% select(-norm_importance, -mean_importance)


onet_soc2018_ability <- read_excel(file.path(data_path, "db_25_1_excel", "Abilities.xlsx")) %>%
  
  filter(`Scale Name` == "Importance") %>%
  select(
    soc8dig_2018 = `O*NET-SOC Code`,
    element_id = `Element Name`,
    importance = `Data Value`
  ) %>%
  # Step 1: Create 6-digit SOC code
  mutate(soc_2018 = substr(soc8dig_2018, 1, 7)) %>%
  left_join(xwalk_soc2018_cen2018, by = "soc_2018", relationship = "many-to-many") %>%
  
  # Step 2: Average importance across 8-digit SOCs within 6-digit SOC + ability
  group_by(census_2018, element_id) %>%
  summarise(mean_importance = mean(importance, na.rm = TRUE), .groups = "drop") %>%
  
  # Step 3: Normalize importance *across all occupations* within each ability (to reflect relative importance)
  group_by(element_id) %>%
  mutate(
    norm_importance = (mean_importance - min(mean_importance, na.rm = TRUE) + 1e-6) /
      (max(mean_importance, na.rm = TRUE) - min(mean_importance, na.rm = TRUE) + 1e-6)
  ) %>%
  ungroup() %>%
  
  # Step 4: Within each SOC, rescale to sum to 1 across abilities
  group_by(census_2018) %>%
  mutate(
    weight = norm_importance / sum(norm_importance, na.rm = TRUE)
  ) %>%
  ungroup() %>% select(-norm_importance, -mean_importance)


#################################################################################
# read in exposure measures for crosswalking

setwd(data_path)

# Felton
xposure_felten = read_excel("AIOE_DataAppendix.xlsx", sheet = "Appendix A") %>%
  rename(soc_2010=`SOC Code`) %>% select(-`Occupation Title`)

xposure_felton_activity = read_excel("AIOE_DataAppendix.xlsx", sheet = "Appendix E") %>%
  rename(element_id = `O*NET Abilities`, aioe_ability = `Ability-Level AI Exposure`)


# Eloundou, T., Manning, S., Mishkin,P., & Rock, D. (2024)   
xposure_gpts_r_gpts = read.csv("gptsRgpts_occ_lvl.csv") %>%
  mutate(soc_2018 = substr(O.NET.SOC.Code, 1, 7)) %>%
  select(soc_2018, gpt4_beta, human_beta)

# Eisfeldt, Schubert, Taska, Zhang, (2024)
xposure_eisfeldt = read.csv("genaiexp_estz_occscores.csv") %>%
  rename(soc_2010 = soc2010)


################################################################################
# crosswalk based on weights ----
  # crosswalk to soc 2010 codes
  # crosswalk to census 2018 codes
  # compute weights twice

xposure_felten_adj = xposure_felten %>%
  left_join(onet_soc2010_ability, by = "soc_2010") %>%
  rename(weight_2010 = weight) %>%
  # get step1 weigh -- ai task weighted by importance value
  mutate(AIOE_sim_1 = weight_2010*AIOE) %>%
  left_join(xwalk_soc2010_cen2018, by = "soc_2010") %>%
  left_join(onet_soc2018_ability, by = c("census_2018", "element_id")) %>%
  rename(weight_2018 = weight) %>%
  mutate(AIOE_sim_2 = AIOE_sim_1*weight_2018) %>%
  ungroup() %>% group_by(census_2018) %>% summarise(AIOE_sim = sum(AIOE_sim_2)) %>%
  ungroup() %>%
  mutate(AIOE_quint_sim = ntile(AIOE_sim, 5))

xposure_gpts_r_gpts_adj = xposure_gpts_r_gpts %>%
  left_join(xwalk_soc2018_cen2018, by = "soc_2018") %>%
  left_join(onet_soc2018_ability, by = "census_2018") %>%
  rename(weight_2018 = weight) %>%
  mutate(gpt4_beta_sim_2 = gpt4_beta*weight_2018,
         human_beta_sim_2 = human_beta*weight_2018) %>%
  ungroup() %>% group_by(census_2018) %>% 
  summarise(gpt4_beta_sim = sum(gpt4_beta_sim_2),
            human_beta_sim = sum(human_beta_sim_2)) %>%
  ungroup() %>%
  mutate(gpt4_beta_quint_sim = ntile(gpt4_beta_sim, 5),
         human_beta_quint_sim = ntile(human_beta_sim, 5))


xposure_eisfeldt_adj = xposure_eisfeldt %>%
  left_join(onet_soc2010_ability, by = "soc_2010") %>%
  rename(weight_2010 = weight) %>%
  # get step1 weigh -- ai task weighted by importance value
  mutate(estz_total_sim_1 = weight_2010*genaiexp_estz_total,
           estz_core_sim_1 = weight_2010*genaiexp_estz_core,
           estz_supplemental_sim_1 = weight_2010*genaiexp_estz_supplemental) %>%
  left_join(xwalk_soc2010_cen2018, by = "soc_2010") %>%
  left_join(onet_soc2018_ability, by = c("census_2018", "element_id")) %>%
  rename(weight_2018 = weight) %>%
  mutate(estz_total_sim_2 = weight_2010*estz_total_sim_1,
         estz_core_sim_2 = weight_2010*estz_core_sim_1,
         estz_supplemental_sim_2 = weight_2010*estz_supplemental_sim_1) %>%
  ungroup() %>% group_by(census_2018) %>% 
  summarise(estz_total_sim = sum(estz_total_sim_2),
            estz_core_sim = sum(estz_core_sim_2),
            estz_supplemental_sim = sum(estz_supplemental_sim_1)) %>%
  ungroup() %>%
  mutate(estz_total_quint_sim = ntile(estz_total_sim, 5),
         estz_core_quint_sim = ntile(estz_core_sim, 5),
         estz_supplemental_quint_sim = ntile(estz_supplemental_sim, 5))



################################################################################
# administrative crosswalks

xposure_felten_admin = xposure_felten %>%
  left_join(xwalk_soc2010_cen2018, by = "soc_2010") %>%
  group_by(census_2018) %>% summarise(AIOE_admin = mean(AIOE)) %>%
  ungroup() %>%
  mutate(AIOE_quint_admin = ntile(AIOE_admin, 5))
  
xposure_gpts_r_gpts_admin = xposure_gpts_r_gpts %>%
  left_join(xwalk_soc2018_cen2018, by = "soc_2018") %>%
  group_by(census_2018) %>%
  summarise(gpt4_beta_admin = mean(gpt4_beta),
            human_beta_admin = mean(human_beta)) %>%
  ungroup() %>%
  mutate(gpt4_beta_quint_admin = ntile(gpt4_beta_admin, 5),
         human_beta_quint_admin = ntile(human_beta_admin, 5))

xposure_eisfeldt_admin = xposure_eisfeldt %>%
  left_join(xwalk_soc2010_cen2018, by = "soc_2010") %>%
  group_by(census_2018) %>%
  summarise(estz_total_admin = mean(genaiexp_estz_total),
            estz_core_admin = mean(genaiexp_estz_core),
            estz_supplemental_admin = mean(genaiexp_estz_supplemental)) %>%
  ungroup() %>%
  mutate(estz_total_quint_admin = ntile(estz_total_admin, 5),
         estz_core_quint_admin = ntile(estz_core_admin, 5),
         estz_supplemental_quint_admin = ntile(estz_supplemental_admin, 5))


################################################################################
#  Felton

onet_soc2018_ability_f = onet_soc2018_ability %>%
mutate(element_id = ifelse(element_id == "Visual Color Discrimination", "Visual Color Determination", element_id)) # difference in naming

names(onet_soc2018_ability)

xposure_felten_activity_wgt = 
  onet_soc2018_ability_f %>% 
  full_join(xposure_felton_activity, by = "element_id") %>%
  group_by(census_2018) %>% summarise(AIOE_wgt = sum(aioe_ability*weight)) %>%
  ungroup() %>%  mutate(AIOE_quint_wgt = ntile(AIOE_wgt, 5))


xposure_felten_final =
  full_join(xposure_felten_adj, xposure_felten_admin, by = "census_2018") %>%
  full_join(xposure_felten_activity_wgt, by = "census_2018")

xposure_eisfeldt_final = 
  full_join(xposure_eisfeldt_adj, xposure_eisfeldt_admin, by = "census_2018")


xposure_gpts_r_gpts_final = 
  full_join(xposure_gpts_r_gpts_adj, xposure_gpts_r_gpts_admin, by = "census_2018")

# export
setwd(wrangled_path)
write.csv(xposure_felten_final, "xposure_felten_measures_xwalk.csv")
write.csv(xposure_gpts_r_gpts_final, "xposure_gpts_r_gpts_measures_xwalk.csv")
write.csv(xposure_eisfeldt_final, "xposure_eisfeldt_measures_xwalk.csv")

#################################################################################
#################################################################################

# OCC 2010 TO 2018 POPULATION WEIGHTED CROSSWALK  #

#################################################################################
#################################################################################

# since IPUMS uses occ 2010 for year < 2020
setwd(data_path)
# read in admin crosswalk
census_occ_2010_occ_2018_xwalk <- read_excel("2018-occupation-code-list-and-crosswalk.xlsx",
                                             sheet = "2010 to 2018 Crosswalk ", skip = 3) %>%
  select(occ2010 = `2010 Census Code`, occ2018 = `2018 Census Code`) %>%
  filter(!(is.na(occ2010) & is.na(occ2018))) %>%
  fill(occ2010, occ2018, .direction = "down") %>%
  distinct() %>%
  mutate(occ2010 = as.numeric(occ2010), occ2018 = as.numeric(occ2018)) %>% na.omit() %>%
  distinct()

# create a weighted crosswalk using weights from microdata, and harmonized 2010 occupations
weights = read_dta("occ_2010_2018_cps_crosswalk.dta") %>%
  filter(occ != 0) %>%
  ungroup() %>% group_by(occ, occ2010) %>% summarise(wt = sum(wtfinl)) %>%
  ungroup() %>% group_by(occ2010) %>% mutate(totwt = sum(wt)) %>%
  mutate(xwalk_wt = wt/totwt) %>% select(-c(wt, totwt)) %>% rename(occ2018 = occ)

census_occ_2010_occ_2018_xwalk = census_occ_2010_occ_2018_xwalk %>%
  left_join(weights, by = c("occ2010", "occ2018")) %>%
  group_by(occ2010) %>%
  mutate(xwalk_wt_2 = 1 / n()) %>%
  ungroup() %>%
  mutate(xwalk_wt_fnl = case_when(
    is.na(xwalk_wt) ~ xwalk_wt_2,
    TRUE ~  xwalk_wt
  )) %>% select(occ2010, occ2018, xwalk_wt_fnl)

census_occ_2010_occ_2018_xwalk <- bind_rows(
  census_occ_2010_occ_2018_xwalk,
  tibble(occ2010 = 0, occ2018 = 0, xwalk_wt_fnl = 1), # IPUMS missing
  tibble(occ2010 = -1, occ2018 = -1, xwalk_wt_fnl = 1) # EPI missing
)

setwd(wrangled_path)
write.csv(census_occ_2010_occ_2018_xwalk, "census_occ_2010_occ_2018_xwalk_cleaned_population_weighted.csv")

#################################################################################
#################################################################################

# NAICS INDUSTRY TO CENSUS INDUSTRY CROSSWALK  #

#################################################################################
#################################################################################


#########################
# the 2-digit crosswalk #
#########################

# combine both the 2017 based crosswalk (for 2020+ dates)
# and the 2012 based crosswalk (for <2020 dates)


# crosswalk in a census to naics industry crosswalk
# note that this is the equivalent file from Census's official sources.

# https://usa.ipums.org/usa/volii/ind2017.shtml

xwalk_census_naics = read.csv(file.path(
  data_path, "2017-industry-code-list.csv"),
  skip = 2) %>%
  select(ind = X2017.Census.Code, naics = X2017.NAICS.Code) %>%
  na.omit() %>% filter(ind!="", naics !="") %>%
  filter(!grepl("-", ind)) %>%
  
  mutate(naics_clean = gsub("Part of ", "", naics)) %>%
  mutate(naics_clean = gsub("exc. ", ",", naics_clean)) %>% # the excl. do not matter as I am only aggregating to 2 digit
  mutate(naics_clean = gsub("pt. ", "", naics_clean)) %>%
  mutate(naics_clean = gsub("Pts.", "", naics_clean)) %>%
  
  # Split by comma
  separate_rows(naics_clean, sep = ",") %>%
  # Trim whitespace
  mutate(naics_clean = str_trim(naics_clean)) %>%
  # Remove empty values (in case of trailing commas)
  filter(naics_clean != "") %>%
  
  mutate(naics_2digit = substr(naics_clean, 1, 2)) %>%
  select(ind, naics_2digit) %>% distinct() %>%
  
  # add a year variable
  mutate(year = "2020, 2021, 2022, 2023, 2024, 2025") %>%
  separate_rows(year, sep = ",") %>%
  mutate(year = str_trim(year)) %>%
  filter(year != "")

table(xwalk_census_naics$naics_2digit)

# crosswalk for 2015-2020 -- 2012 based classification
xwalk_census_naics_2012_basis = read_excel(
  file.path(data_path,
            "census-2012-final-code-list.xls"),
  skip = 4) %>%
  select(ind = `2012 Census Code`, naics = `2012 NAICS Code`) %>%
  na.omit() %>% filter(ind!="", naics !="") %>%
  filter(!grepl("-", ind)) %>%
  
  mutate(naics_clean = gsub("Part of ", "", naics)) %>%
  mutate(naics_clean = gsub("exc. ", ",", naics_clean)) %>% # the excl. do not matter as I am only aggregating to 2 digit
  mutate(naics_clean = gsub("pt. ", "", naics_clean)) %>%
  mutate(naics_clean = gsub("Pts.", "", naics_clean)) %>%
  
  # Split by comma
  separate_rows(naics_clean, sep = ",") %>%
  # Trim whitespace
  mutate(naics_clean = str_trim(naics_clean)) %>%
  # Remove empty values (in case of trailing commas)
  filter(naics_clean != "") %>%
  
  mutate(naics_2digit = substr(naics_clean, 1, 2)) %>%
  select(ind, naics_2digit) %>% distinct() %>%
  
  # add a year variable
  mutate(year = "2015, 2016, 2017, 2018, 2019") %>%
  separate_rows(year, sep = ",") %>%
  mutate(year = str_trim(year)) %>%
  filter(year != "")

xwalk_census_naics = bind_rows(xwalk_census_naics, xwalk_census_naics_2012_basis)
xwalk_census_naics = xwalk_census_naics %>%
  mutate(ind = as.numeric(ind),
         year = as.numeric(year))

setwd(wrangled_path)
write.csv(xwalk_census_naics, "xwalk_naics_ind_to_census_ind_combined.csv")

################################################################################
################################################################################
                        # Occ 1990 -> occ 2018 codes #

setwd(data_path)
sample = read_dta("cps_monthly_occ2018_occ1990_xwalk.dta")

sample_xwalk = sample %>%
  ungroup() %>%
  group_by(occ, occ1990) %>%
  summarise(wgt = sum(wtfinl, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(occ) %>%
  mutate(wgt_xwalk = wgt/sum(wgt)) %>%
  filter(wgt_xwalk !=0) %>% select(-wgt) %>% rename(occ2018 = occ)

setwd(wrangled_path)
write.csv(sample_xwalk, "weighted_occ1990_occ2018_xwalk.csv")

