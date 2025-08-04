

# LAST UPDATED: 07/18/2025
# AUTHOR: Sarah Eckhardt
# DESCRIPTION: This file configures the crosswalks for Felton, GPTs are GPTs,
# Eisfeldt, and Webb AI exposure methods using a variety of methods:
    # a cleaned administrative version, assigning the mean exposure for n>1 occ matches
    # an O*NET similarity version, assigning the most similar occupation's exposure
    # and for Felton specifically, an exposure built from activity task similarity.
    # (for the later two, see ai crosswalks.R)
# it then combines these with the CPS basic monthly dataset for analysis,
# addressing the fact that pre-2020 data is in occ 2010 codes, and
# handling the many-to-many occ 2010 to occ 2018 crosswalk with a probabilistic
# population weighted assignment approach

rm(list = ls())

# packages
library(tidyr)
library(dplyr)
library(readxl)
library(stringr)
library(data.table)
library(haven)
library(purrr)

# set project paths
user_path = "/Users/sarah/Library/CloudStorage/GoogleDrive-sarah@eig.org/.shortcut-targets-by-id/0B8XLtQT1s8ceT2tKZWZmTkJmM1k"
project_path = file.path(user_path, "EIG/RESEARCH/1 ACTIVE/AI Unemployment")
data_path = file.path(project_path, "data/1raw")
wrangled_path = file.path(project_path, "data/2wrangled")

######################
# read in crosswalks #
######################
setwd(wrangled_path)

xposure_felten_measures = read.csv("xposure_felten_measures_xwalk.csv")
xposure_gpts_r_gpts_measures = read.csv("xposure_gpts_r_gpts_measures_xwalk.csv")
xposure_eisfeldt_measures = read.csv("xposure_eisfeldt_measures_xwalk.csv")

xposure_felten_measures = xposure_felten_measures %>%
  filter(!is.na("census_2018"))

xposure_gpts_r_gpts_measures = xposure_gpts_r_gpts_measures %>%
  filter(!is.na("census_2018"))

xposure_eisfeldt_measures = xposure_eisfeldt_measures %>%
  filter(!is.na("census_2018"))



# Webb (2022)
setwd(data_path)
xposure_webb = read.csv("exposure_by_occ1990dd_lswt2010.csv") %>%
  select(occ1990dd, pct_ai) %>% mutate(pct_ai_quint = ntile(pct_ai, 5))

# construct webb crosswalk --- based on occ1990 codes.    
    xwalk_dd_oocc1990 = read_dta(file.path(data_path, "occ1990_occ1990dd", "occ1990_occ1990dd.dta"))
    
    xposure_webb = xposure_webb %>% left_join(xwalk_dd_oocc1990, by = "occ1990dd") %>% rename(occ1990 = occ)
    
    xposure_webb = xposure_webb %>% filter(!is.na(occ1990)) %>% distinct()
    
    count(xposure_webb)
    length(unique(xposure_webb$occ1990))
    
setwd(wrangled_path)
write.csv(xposure_webb, "xposure_webb_xwalk.csv")

################################################################################
# 3. ADD MEASURES TO EACH MICRODATASET

# read in nested Occ 2010 -> Occ 2018 crosswalk
setwd(wrangled_path)
xwalk_occ2010_occ2018 <- 
  read.csv("census_occ_2010_occ_2018_xwalk_cleaned_population_weighted.csv")


xwalk_nested <- xwalk_occ2010_occ2018 %>%
  group_by(occ2010) %>%
  summarise(
    occ2018_options = list(occ2018),
    weights = list(xwalk_wt_fnl),
    .groups = "drop"
  )

rm(xwalk_occ2010_occ2018)

# do the same for 1990s crossswalks
xwalk_occ1990_occ2018 <-  read.csv("weighted_occ1990_occ2018_xwalk.csv") %>%
  select(-X, -wgt_xwalk)


#######
# CPS #
#######
setwd(data_path)
cps_monthly = read_dta("cps_basic_monthly.dta")

# merge proportionally - handle the m:m crosswalk
# *_join repeats observations for each crosswalked options

# Join nested options to CPS
cps_with_xwalk_options <- cps_monthly %>%
  rename(occ_raw = occ) %>%
  left_join(xwalk_nested, by = c("occ_raw" = "occ2010")) # then NULL crosswalks are the not in universe (0)


# Randomly assign one occ2018 per person using xwalk weights
set.seed(123)  # for reproducibility
cps_assigned <- cps_with_xwalk_options %>%
  mutate(
    occ2018 = map2_chr(occ2018_options, weights, ~ {
      # Check for valid, matching length and positive weights
      if (is.null(.x) || length(.x) == 0 || is.null(.y) || length(.y) != length(.x) || sum(.y) == 0) {
        return("0")  # fallback
      }
      
      # Only one option with positive weight — return it
      if (length(.x) == 1) {
        return(as.character(.x))
      }
      
      # Otherwise, sample normally
      sample(.x, size = 1, prob = .y)
    })
  ) %>%
  mutate(occ2018 = as.integer(occ2018))

# final check for duplicates
count(cps_assigned) == count(cps_monthly)

# assign occ based on year and probabilistic crosswalk
cps_monthly_w_xposure = cps_assigned %>%
  mutate(occ = case_when(
    year > 2019 ~ occ_raw,
    year <= 2019 ~ occ2018
  ))  %>% 
  select(-c(occ2018, occ2018_options, weights)) %>%
  filter(occ != 0) # missing occ codes, niu


# add in occupation measures
cps_monthly_w_xposure = cps_monthly_w_xposure %>%
  # Felton
  left_join(xposure_felten_measures, by = c("occ" = "census_2018")) %>%

  # GPTs are GPTs
  left_join(xposure_gpts_r_gpts_measures, by = c("occ" = "census_2018")) %>%
  
  # Eisfeldt
  left_join(xposure_eisfeldt_measures, by = c("occ" = "census_2018")) %>%
  
  # Webb
  left_join(xposure_webb, by = "occ1990")


# add the missings back in
cps_monthly_missing = cps_monthly %>%  filter(occ == 0)
rm(cps_assigned, cps_with_xwalk_options, cps_monthly)
cps_monthly_w_xposure = bind_rows(cps_monthly_w_xposure, cps_monthly_missing)
cps_monthly_w_xposure = cps_monthly_w_xposure %>% select(-X.y, -X, -X.x)

# save dataset for analysis
setwd(wrangled_path)
write_dta(cps_monthly_w_xposure, "cps_monthly_w_xposure_xwalked.dta")

# clean up
rm(cps_monthly_w_xposure, cps_monthly_missing)


########
# ASEC #
########

setwd(data_path)
cps_asec = read_dta("cps_asec.dta")


# Join nested options to CPS
cps_with_xwalk_options <- cps_asec %>%
  rename(occ_raw = occ) %>%
  left_join(xwalk_nested, by = c("occ_raw" = "occ2010")) # then NULL crosswalks are the not in universe (0)


# Randomly assign one occ2018 per person using xwalk weights
set.seed(123)  # for reproducibility
cps_assigned <- cps_with_xwalk_options %>%
  mutate(
    occ2018 = map2_chr(occ2018_options, weights, ~ {
      # Check for valid, matching length and positive weights
      if (is.null(.x) || length(.x) == 0 || is.null(.y) || length(.y) != length(.x) || sum(.y) == 0) {
        return("0")  # fallback
      }
      
      # Only one option with positive weight — return it
      if (length(.x) == 1) {
        return(as.character(.x))
      }
      
      # Otherwise, sample normally
      sample(.x, size = 1, prob = .y)
    })
  ) %>%
  mutate(occ2018 = as.integer(occ2018))

# final check for duplicates
count(cps_assigned) == count(cps_asec)

# assign occ based on year and probabilistic crosswalk
cps_asec_w_xposure = cps_assigned %>%
  mutate(occ = case_when(
    year > 2019 ~ occ_raw,
    year <= 2019 ~ occ2018
  ))  %>% 
  select(-c(occ2018, occ2018_options, weights)) %>%
  filter(occ != 0) # missing occ codes, niu


# add in occupation measures
cps_asec_w_xposure = cps_asec_w_xposure %>%
  # Felton
  left_join(xposure_felten_measures, by = c("occ" = "census_2018")) %>%
  
  # GPTs are GPTs
  left_join(xposure_gpts_r_gpts_measures, by = c("occ" = "census_2018")) %>%
  
  # Eisfeldt
  left_join(xposure_eisfeldt_measures, by = c("occ" = "census_2018")) %>%
  
  # Webb
  left_join(xposure_webb, by = "occ1990")



cps_asec_missing = cps_asec %>%  filter(occ == 0)
rm(cps_assigned, cps_with_xwalk_options, cps_asec)
cps_asec_w_xposure = bind_rows(cps_asec_w_xposure, cps_asec_missing)
cps_asec_w_xposure = cps_asec_w_xposure %>% select(-X.y, -X, -X.x)

# save dataset for analysis
setwd(wrangled_path)
write_dta(cps_asec_w_xposure, "cps_asec_w_xposure_xwalked.dta")


# clean up
rm(cps_asec_w_xposure, cps_assigned, cps_with_xwalk_options, cps_asec, cps_asec_missing)


#############################
# OUTGOING ROTATIONAL GROUP #
#############################

# read in 2015 - 2024 org panels and combine
setwd(file.path(data_path, "epi_cpsorg_1979_2025"))

yrs = c(2015:2024)

cps_org_list = list()
for (yr in yrs) {
  
  org_file_name = paste0("epi_cpsorg_",yr,".dta")
  print(paste("loading",org_file_name))
  org_file = read_dta(org_file_name)
  
  cps_org_list[[yr]] = org_file
  
}

cps_org = bind_rows(cps_org_list)

rm(cps_org_list, org_file)

cps_org = cps_org %>% mutate(id = group_indices(., hrhhid, hrhhid2, pulineno)) %>%
  select(hrhhid, hrhhid2, pulineno, id, everything())

# EPI does NOT prperly crosswalk occs here.
# uses -1 as missing rather than IPUMS 0
# Join nested options to CPS
cps_with_xwalk_options <- cps_org %>%
  left_join(xwalk_nested, by = c("occ10" = "occ2010")) # then NULL crosswalks are the not in universe (0)

rm(cps_org)

# Randomly assign one occ2018 per person using xwalk weights
set.seed(123)  # for reproducibility
cps_assigned <- cps_with_xwalk_options %>%
  mutate(
    occ2018 = map2_chr(occ2018_options, weights, ~ {
      # Check for valid, matching length and positive weights
      if (is.null(.x) || length(.x) == 0 || is.null(.y) || length(.y) != length(.x) || sum(.y) == 0) {
        return("0")  # fallback
      }
      
      # Only one option with positive weight — return it
      if (length(.x) == 1) {
        return(as.character(.x))
      }
      
      # Otherwise, sample normally
      sample(.x, size = 1, prob = .y)
    })
  ) %>%
  mutate(occ2018 = as.integer(occ2018))

# final check for duplicates
count(cps_assigned) == count(cps_org)


# assign occ based on year and probabilistic crosswalk
cps_org_w_xposure = cps_assigned %>%
  mutate(occ18 = case_when(
    year > 2019 ~ occ18,
    year <= 2019 ~ occ2018
  ))  %>% 
  select(-c(occ2018, occ2018_options, weights)) # %>%
#  filter(occ18 != -1) # missing occ codes, niu


cps_org_w_xposure = cps_org_w_xposure %>%
  left_join(xwalk_occ1990_occ2018, by = c("occ18" = "occ2018"))

# I need a 1990s occ crosswalk to get the Webb indicator to work. save off of that for now, unless Nathan requests it
cps_org_w_xposure = cps_org_w_xposure %>%
  # Felton
  left_join(xposure_felten_measures, by = c("occ18" = "census_2018")) %>%
  
  # GPTs are GPTs
  left_join(xposure_gpts_r_gpts_measures, by = c("occ18" = "census_2018")) %>%
  
  # Eisfeldt
  left_join(xposure_eisfeldt_measures, by = c("occ18" = "census_2018")) %>%
  
  left_join(xposure_webb, by ="occ1990")

# save dataset for analysis
setwd(wrangled_path)

cps_org_w_xposure = cps_org_w_xposure %>%
  select(-X, -X.x, -X.y)

write_dta(cps_org_w_xposure, "cps_org_w_xposure_xwalked.dta")
