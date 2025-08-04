

# DATE LAST UPDATED: 07/28/2025
# PROJECT: AI Employment Impacts
# AUTHOR: Sarah Eckhardt
# DESCRIPTION: Create charts and cited stats for main analysis

rm(list = ls())

# packages
library(tidyr)
library(dplyr)
library(readxl)
library(stringr)
library(haven)
library(ggplot2)
library(blscrapeR)
library(data.table)
library(zoo)


# set project paths
user_path = "/Users/sarah/Library/CloudStorage/GoogleDrive-sarah@eig.org/.shortcut-targets-by-id/0B8XLtQT1s8ceT2tKZWZmTkJmM1k"
project_path = file.path(user_path, "EIG/RESEARCH/1 ACTIVE/AI Unemployment")
data_path = file.path(project_path, "data/1raw")
wrangled_path = file.path(project_path, "data/2wrangled")
output_path = file.path(project_path, "data/3final/mainline_text")

setwd(wrangled_path)

################################################################################
################################################################################

# Read in and wrangle data for analysis
  # CPS - basic monthly
  # CPS - asec
  # CPS - outgoing rotational group
  # QCEW
  # CES

################################################################################
################################################################################

# CPS - basic monthly
cps_monthly_xposure = read_dta("cps_monthly_w_xposure_xwalked.dta")

cps_monthly_xposure = cps_monthly_xposure %>%
  mutate(
    date = make_date(year, month = month),
    qdate = as.yearqtr(paste(year, month), format = "%Y %m"),
    qdate_date = as.Date(qdate),
    
    unemp = case_when(
      empstat == 10 | empstat == 12 ~ 0, # employed
      empstat == 20 | empstat == 21 | empstat == 22 ~ 1, # unemployed
      empstat  >= 30 ~ 99),  # niu / nilf
    
    lfp = case_when(
      labforce == 1 ~ 0, # not in labor force
      labforce == 2 ~ 1, # in labor force
      TRUE ~ NA
    ),
    
    educ_cat = case_when(
      educ < 73 ~ "<HS",
      educ == 73 ~ "HS",
      educ > 73 & educ < 111 ~ "someCol",
      educ == 111 ~ "BA",
      educ >= 120 ~ "BA+",
      TRUE ~ NA_character_))


# CPS - asec
cps_asec = read_dta("cps_asec_w_xposure_xwalked.dta")


# CPS - outgoing rotational group
cps_org = read_dta(file.path(wrangled_path, "cps_org_w_xposure_xwalked.dta"))

# identify change in unemployment, change in occupation, change in laborforce status.
# use datatable to optimize speed.
setDT(cps_org)
setorder(cps_org, id, minsamp, year)  # Make sure it's sorted properly

cps_org[, `:=`(
  unemp_lead = shift(unemp, type = "lead"),
  lfstat_lead = shift(lfstat, type = "lead"),
  occ18_lead = shift(occ18, type = "lead")
), by = id]

# toue: transition to unemployment
cps_org[, toue := fifelse(
  lfstat == 1 &
    lfstat_lead != 3 &
    unemp_lead == 1 &
    !is.na(unemp_lead),
  TRUE, FALSE
)]

# chgocc: occupation change
cps_org[, chgocc := fifelse(
  lfstat == 1 &
    lfstat_lead == 1 &
    !is.na(occ18) & !is.na(occ18_lead) &
    occ18 != occ18_lead,
  TRUE, FALSE
)]

# chglf: change in LF status
cps_org[, chglf := fifelse(
  !is.na(lfstat) & !is.na(lfstat_lead) &
    lfstat != lfstat_lead,
  TRUE, FALSE
)]

# changing to higher/lower exposed quintile
for (var in quint_xposure_vars) {
  lead_var <- paste0(var, "_lead")
  cps_org[, (lead_var) := shift(get(var), type = "lead"), by = id]
}

for (var in quint_xposure_vars) {
  new_var <- paste0("chgocc_lowerexp_", var)
  lead_var <- paste0(var, "_lead")
  
  cps_org[, (new_var) := fifelse(
    chgocc == TRUE &
      !is.na(get(var)) & !is.na(get(lead_var)) &
      get(lead_var) != get(var), # identify ANY switch
    TRUE, FALSE
  )]
}

# add year-month and year-quarter dates
cps_org <- cps_org %>%
  mutate(ym = as.yearmon(paste(year, month), "%Y %m")) %>%
  mutate(yq = as.yearqtr(ym)) # year quarter

cps_org = cps_org %>%
  filter(year < 2024)

org_charts = cps_org %>%
  filter(!is.na(AIOE_quint_wgt)) %>%
  group_by(yq, AIOE_quint_wgt) %>%
  summarise(
    toue = weighted.mean(toue, finalwgt, na.rm = TRUE),
    chgocc = weighted.mean(chgocc, finalwgt, na.rm = TRUE),
    chglf = weighted.mean(chglf, finalwgt, na.rm = TRUE),
    .groups = "drop"
  )



# QCEW

qcewGetAreaData <- function(year, qtr, area) {
  url <- "http://data.bls.gov/cew/data/api/YEAR/QTR/area/AREA.csv"
  url <- sub("YEAR", year, url, ignore.case=FALSE)
  url <- sub("QTR", tolower(qtr), url, ignore.case=FALSE)
  url <- sub("AREA", toupper(area), url, ignore.case=FALSE)
  read.csv(url, header = TRUE, sep = ",", quote="\"", dec=".", na.strings=" ", skip=0)
}


years = c(2014:2024)
qcew_list = list()

for(year in years) {
  us_year = list()
  
  for(quarter in c(1,2,3,4)) {
    us_qtr <- qcewGetAreaData(paste(year), paste(quarter), "US000")
    us_qtr = us_qtr %>% mutate(year = year) %>%
      filter(own_code == 5)
    
    us_year[[quarter]] = us_qtr
  }
  
  year_data = bind_rows(us_year)
  qcew_list[[year]] = year_data
}

qcew_df = bind_rows(qcew_list)
qcew_df = qcew_df %>% rename(naics = industry_code)
qcew_df = qcew_df %>% mutate(across(c(year, qtr, qtrly_estabs, month1_emplvl, month2_emplvl,
                                      month3_emplvl, total_qtrly_wages, taxable_qtrly_wages,
                                      qtrly_contributions, avg_wkly_wage, lq_disclosure_code,
                                      lq_qtrly_estabs, lq_month1_emplvl, lq_month2_emplvl, 
                                      lq_month3_emplvl, lq_total_qtrly_wages, lq_taxable_qtrly_wages,
                                      lq_qtrly_contributions, lq_avg_wkly_wage, oty_disclosure_code,
                                      oty_qtrly_estabs_chg, oty_qtrly_estabs_pct_chg, oty_month1_emplvl_chg,
                                      oty_month1_emplvl_pct_chg, oty_month2_emplvl_chg, oty_month2_emplvl_pct_chg,
                                      oty_month3_emplvl_chg, oty_month3_emplvl_pct_chg, oty_total_qtrly_wages_chg,
                                      oty_total_qtrly_wages_pct_chg, oty_taxable_qtrly_wages_chg, oty_taxable_qtrly_wages_pct_chg,
                                      oty_qtrly_contributions_chg, oty_qtrly_contributions_pct_chg, oty_avg_wkly_wage_chg,
                                      oty_avg_wkly_wage_pct_chg),
                                    ~as.numeric(.)))


# merge with AIOE exposure measure at industry level
aioe_industry = read_excel(
  file.path(data_path,"AIOE_DataAppendix.xlsx"), sheet = "Appendix B"
) %>% select(naics = NAICS, aiie = AIIE) %>% mutate(naics = as.character(naics)) %>%
  mutate(aioe_q5 = ntile(aiie, 5))

qcew_df = qcew_df %>% left_join(aioe_industry, by = "naics") %>% filter(!is.na(aioe_q5))



# read in CES
ces_codes = c("CES5000000001", "CES5051200001", "CES5051211001", "CES5051213001",
              "CES5051300001", "CES5051310001", "CES5051311001", "CES5051312001",
              "CES5051313001", "CES5051319001", "CES5051319001", "CES5051320001",
              "CES5051600001", "CES5051610001", "CES5051611001", "CES5051612001",
              "CES5051620001", "CES5051700001", "CES5051710001", "CES5051711001",
              "CES5051711101", "CES5051711201", "CES5051712001", "CES5051780001",
              "CES5051800001", "CES5051900001", "CES5051921001", "CES5051929001")

# pulls in 9-year increments.
year_ranges <- list(
  c(1990, 1998),
  c(1999, 2007),
  c(2008, 2016),
  c(2017, 2025)
)

ces_df <- map_dfr(year_ranges, function(yr) {
  bls_api(
    seriesid = ces_codes,
    startyear = yr[1],
    endyear = yr[2],
    registrationKey = Sys.getenv("83be88f5e04b48fd9e9e4956214a094f")
  )
})


# assign industry codes
ces_df = ces_df %>%
  mutate(industry_name = case_when(
    seriesID == "CES5000000001" ~ "Information", 
    seriesID == "CES5051200001" ~ "Motion picture and sound recording industries", 
    seriesID == "CES5051211001" ~ "Motion picture and video production", 
    seriesID == "CES5051213001" ~ "Motion picture and video exhibition", 
    seriesID == "CES5051300001" ~ "Publishing industries", 
    seriesID == "CES5051310001" ~ "Newspaper, periodical, book, and directory publishers", 
    seriesID == "CES5051311001" ~ "Newspaper publishers", 
    seriesID == "CES5051312001" ~ "Periodical publishers", 
    seriesID == "CES5051313001" ~ "Book publishers", 
    seriesID == "CES5051319001" ~ "Directory, mailing list, and other publishers",
    seriesID == "CES5051319001" ~ "Directory, mailing list, and other publishers", 
    seriesID == "CES5051320001" ~ "Software publishers", 
    seriesID == "CES5051600001" ~ "Broadcasting and content providers", 
    seriesID == "CES5051610001" ~ "Radio and television broadcasting stations", 
    seriesID == "CES5051611001" ~ "Radio broadcasting stations", 
    seriesID == "CES5051612001" ~ "Television broadcasting stations", 
    seriesID == "CES5051620001" ~ "Media streaming distribution services, social networks, and other media networks and content providers", 
    seriesID == "CES5051700001" ~ "Telecommunications", 
    seriesID == "CES5051710001" ~ "Wired and wireless telecommunications (except satellite)", 
    seriesID == "CES5051711001" ~ "Wired and wireless telecommunications carriers (except satellite)", 
    seriesID == "CES5051711101" ~ "Wired telecommunications carriers", 
    seriesID == "CES5051711201" ~ "Wireless telecommunications carriers (except satellite)", 
    seriesID == "CES5051712001" ~ "Telecommunications resellers", 
    seriesID == "CES5051780001" ~ "All other telecommunications	", 
    seriesID == "CES5051800001" ~ "Computing infrastructure providers, data processing, web hosting, and related services", 
    seriesID == "CES5051900001" ~ "Web search portals, libraries, archives, and other information services", 
    seriesID == "CES5051921001" ~ "Libraries and archives	", 
    seriesID == "CES5051929001" ~ "Web search portals and all other information services"),
    
    industry_code = 
      substr(seriesID, 6, 11),
    industry_code = case_when(
      seriesID == "CES5000000001" ~ "51",
      seriesID == "CES5051319001" ~ "51314",
      TRUE ~ industry_code),
    industry_code = gsub("0","", industry_code),
    month_num = match(periodName, month.name),
    date = as.Date(paste(year, month_num, 1, sep = "-")))

# do quarterly
ces_qtr_df = ces_df %>%
  mutate(
    quarter = quarter(date),
    year_quarter = as.Date(paste0(year, "-", (quarter * 3), "-01"))
  ) %>%
  group_by(year_quarter, industry_name, industry_code, seriesID) %>%
  summarise(value = mean(value, na.rm = TRUE)) %>%
  arrange(industry_name, year_quarter) %>% group_by(industry_name) %>%
  mutate(job_chng = value - lag(value))


# set color scheme for charts. grey = national
graph_color_scheme_1 <-  c("1" = "#f0b799", "2" = "#234f8b", "3" = "#5e9c86", "4" = "#b3d6dd", "5" = "#e1ad28", "99" = "grey")

graph_color_scheme_2 <- c("1" = "#f0b799", "2" = "#234f8b", "3" = "#5e9c86", "4" = "#b3d6dd", "5" = "#e1ad28")


################################################################################
################################################################################
# ANALYSIS - FIGURES
################################################################################
################################################################################



################################################################################
# Summary Statistics by AI Exposure Quintile -- Felten

cps_monthly_2025 = cps_monthly_xposure %>% filter(year == 2025)

# extract 2024 sample
cps_asec_2024 = cps_asec %>% filter(year == 2024) %>%
  filter(incwage != 99999999) # niu

unemp_share <- cps_monthly_2025 %>% group_by(AIOE_quint_wgt, unemp) %>% 
  filter(unemp != 99, !is.na(AIOE_quint_wgt)) %>% # missing
  group_by(AIOE_quint_wgt) %>%
  summarise(
    share_unemp = 100*sum(wtfinl * (unemp == 1)) / sum(wtfinl))

educ_share <- cps_monthly_2025 %>% group_by(AIOE_quint_wgt, educ_cat) %>% 
  filter(!is.na(educ_cat), AIOE_quint_wgt) %>% # missing
  summarise(wt_total = sum(wtfinl), .groups = "drop") %>%
  group_by(AIOE_quint_wgt) %>%
  mutate(
    share_educ = 100*wt_total / sum(wt_total)
  ) %>% select(-wt_total) %>%
  pivot_wider(names_from = educ_cat, values_from = share_educ)

sex_share <- cps_monthly_2025 %>% group_by(AIOE_quint_wgt, sex) %>% 
  filter(sex != 9, !is.na(AIOE_quint_wgt)) %>% # missing
  summarise(wt_total = sum(wtfinl), .groups = "drop") %>%
  group_by(AIOE_quint_wgt) %>%
  mutate(
    share_sex = 100*wt_total / sum(wt_total)
  ) %>% select(-wt_total) %>%
  pivot_wider(names_from = sex, values_from = share_sex) %>%
  rename(men = `1`, women =`2`)

# asec chart
mean_wages = cps_asec %>% group_by(AIOE_quint_wgt) %>%
  filter(!is.na(AIOE_quint_wgt)) %>% # missing
  summarise(mean_incwage = weighted.mean(incwage, wt = wtfinl, na.rm = TRUE))

# combine
table = unemp_share %>% full_join(educ_share, by = var) %>%
  full_join(sex_share, by = var) %>% full_join(mean_wages, by = var)

# Save csv
setwd(output_path)
write.csv(table, "3_summary_statistics.csv", row.names = FALSE)


################################################################################
# Unemployment rate by AI exposure quintile

var_source = "Felten et al. (2021)"
crosswalk_version =  "using weighted abilities-based exposure (Felten et al. (2021)."

p_unemp_df = cps_monthly_xposure %>%
  filter(unemp !=99, !is.na(AIOE_quint_wgt)) %>%
  ungroup() %>%  group_by(qdate_date, AIOE_quint_wgt) %>%
  summarise(
    unemp = weighted.mean(unemp, wtfinl, na.rm = TRUE),
    .groups = "drop"
  )

p_unemp_df_overall = cps_monthly_xposure %>%
  filter(unemp !=99) %>%
  ungroup() %>%  group_by(qdate_date) %>%
  summarise(
    unemp = weighted.mean(unemp, wtfinl, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(AIOE_quint_wgt := 99)

p_unemp_df = bind_rows(p_unemp_df, p_unemp_df_overall) %>%
  mutate(AIOE_quint_wgt := factor(AIOE_quint_wgt))

# create bins
n_bins <- 30 

unemp_binned_df <- p_unemp_df %>%
  group_by(AIOE_quint_wgt) %>%
  mutate(bin = ntile(qdate_date, n_bins)) %>%
  group_by(bin, AIOE_quint_wgt) %>%
  summarise(
    qdate_center = as.Date(median(qdate_date, na.rm = TRUE)),
    unemp_bin = mean(unemp, na.rm = TRUE),
    .groups = "drop"
  )

p_unemp <- ggplot(p1_df, aes(x = qdate_date, y = unemp, color = AIOE_quint_wgt, linetype = AIOE_quint_wgt)) +
  geom_smooth(
    aes(linetype = AIOE_quint_wgt), method = "loess", span = 0.5, se = FALSE, linewidth = 0.5, show.legend = TRUE
  )  +
  geom_point(data = unemp_binned_df, aes(x = qdate_center, y = unemp_bin, color =AIOE_quint_wgt),
             size = 2, alpha = 0.3) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_color_manual(
    values = graph_color_scheme_1,
    labels = c("1", "2", "3", "4", "5", "US total"),
    name = "Exposure"
  ) +
  scale_linetype_manual(
    values = c("1" = "solid", "2" = "solid", "3" = "solid", "4" = "solid", "5" = "solid", "99" = "dashed"),
    guide = "none"  # suppress separate linetype legend
  ) +
  labs(
    y = "unemployment (%)",
    x = "",
    title = "Unemployment rate by AI exposure quintile",
    caption = paste("Note: AI exposure estimate crosswalked",crosswalk_version,"\nSource: AI exposure from", var_source, "Quarterly unemployment from CPS.")
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.position = "bottom",
    legend.title = element_text(size = 10),
    legend.key = element_blank(),
    plot.caption.position = "plot",
    plot.caption = element_text(hjust = 0)
  ) +
  guides(
    color = guide_legend(nrow = 1, byrow = TRUE),  # Force legend to 2 rows
    #    linetype = "none",
    #    color = guide_legend(nrow = 1, byrow = TRUE)
  )

ggsave("4_unemployment_by_quintile.png", plot = p_unemp, width = 8, height = 5, dpi = 300)


################################################################################
# Share of workers moving out of the labor force by quintile

lfpr_moving_plot <- ggplot(org_charts, aes(x = as.Date(yq), y = chglf, color = factor(AIOE_quint_wgt))) +
  geom_smooth(method = "loess", se = FALSE, span = 0.5, linewidth = 0.5) +  # LOESS lines
  geom_point(size = 1.8, alpha = 0.35) +  # Raw points
  scale_color_manual(
    values = graph_color_scheme_2,
    name = "Exposure",
    labels = c("1", "2", "3", "4", "5")
  ) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    y = "% moving out of labor force",
    x = NULL,
    title = "Share of workers moving out of the labor force by quintile",
    caption = paste("Note: Graphed are 1-year changes, where the most recent data represents change from Q4 2023 to Q4 2024.\nAI exposure estimate crosswalked",crosswalk_version,"\nSource: AI exposure from", var_source, "Quarterly labor force participation data from CPS Outgoing Rotational Group sample.")
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.position = "bottom",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9),
    plot.caption.position = "plot",
    plot.caption = element_text(hjust = 0)
  ) +
  guides(
    color = guide_legend(nrow = 1, byrow = TRUE)  # Force legend to 2 rows
  )

ggsave("5_out_of_lf_by_quintile.png", plot = lfpr_moving_plot, width = 8, height = 5, dpi = 300, bg = "white")


################################################################################
# Share of workers changing occupation by quintile

occ_switching_plot <- ggplot(org_charts, aes(x = as.Date(yq), y = chgocc, color = factor(AIOE_quint_wgt)) +
   geom_smooth(method = "loess", se = FALSE, span = 0.5, linewidth = 0.5) +  # LOESS lines
   geom_point(size = 1.8, alpha = 0.35) +  # Raw points
   scale_color_manual(
     values = graph_color_scheme_2,
     name = "Exposure",
     labels = c("1", "2", "3", "4", "5")
   ) +
   scale_y_continuous(labels = scales::percent_format()) +
   labs(
     y = "% changing occupation",
     x = NULL,
     title = "Share of workers changing occupation by quintile",
     caption = paste("Note: Graphed are 1-year changes, where the most recent data represents change from Q4 2023 to Q4 2024.\nAI exposure estimate crosswalked",crosswalk_version,"\nSource: AI exposure from", var_source, "Quarterly occupation data from CPS Outgoing Rotational Group sample.")
   ) +
   theme_minimal(base_size = 12) +
   theme(
     plot.background = element_rect(fill = "white", color = NA),
     panel.background = element_rect(fill = "white", color = NA),
     legend.position = "bottom",
     legend.title = element_text(size = 10),
     legend.text = element_text(size = 9),
     plot.caption.position = "plot",
     plot.caption = element_text(hjust = 0)
   ) +
   guides(
     color = guide_legend(nrow = 1, byrow = TRUE)  # Force legend to 2 rows
   )
                             
ggsave("6_changing_occupation_by_quintile.png", plot = occ_switching_plot, width = 8, height = 5, dpi = 300, bg = "white")
                             


################################################################################
# Probability of switching to a lower-exposed occupation

plot_switching_lower_exposure <- org_charts_long %>%
  filter(AIOE_quint_wgt==5) %>%
  ggplot(aes(x = as.Date(yq), y = chgocc_lowerexp_AIOE_quint_wgt)) +
  geom_smooth(method = "loess", se = FALSE, span = 0.5, linewidth = 0.5, color = "#234f8b") +  # LOESS lines
  geom_point(size = 1.8, alpha = 0.35, color = "#234f8b") +
  theme_minimal(base_size = 12) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(x = "",
       y = "pr(switching to lower exposed occupation)",
       title = "Probability of switching to a lower-exposed occupation",
       subtitle = "For the most exposed workers only",
       caption = "Note: AI exposure estimate crosswalked using weighted abilities-based exposure (Felten et al. 2021)\nSource: Felten (2021) for AI exposure, and CPS ORG from the Economic Policy Institute \nfor occupation switching"
  ) +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.position = "bottom",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9),
    plot.caption.position = "plot",
    plot.caption = element_text(hjust = 0)
  )

setwd(output_path)
ggsave("7_changing_lower_exposure.png",  width = 8, height = 5, dpi = 300)


################################################################################
# Employment by exposure quintile


qcew_df_cumulative <- qcew_df %>%
  filter(!is.na(aioe_q5)) %>%
  mutate(yq = as.yearqtr(paste(year, qtr), format = "%Y %q")) %>%
  group_by(aioe_q5, yq) %>%
  summarise(
    qtr_emplvl = sum(month1_emplvl + month2_emplvl + month3_emplvl, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(aioe_q5) %>%
  arrange(aioe_q5, yq) %>%  # ← key line!
  mutate(cumulative_emplvl = cumsum(qtr_emplvl)) %>%
  ungroup() %>%
  mutate(qtr_emplvl = qtr_emplvl/1000000)


plot_employment <- ggplot(
  qcew_df_cumulative %>% arrange(aioe_q5, yq),
  aes(x = yq, y = qtr_emplvl, color = factor(aioe_q5))
) +
  geom_point(size = 1.5, alpha = 0.5) +           # add points
  geom_smooth(se = FALSE, size = 0.5, method = "loess", span = 0.2) +  # add smooth lines without confidence interval
  labs(
    y = "employment (millions)",
    x = "",
    title = "Employment by exposure quintile",
    caption = "Source: industry-level AI exposure from Felten et al. (2021). Industry-level employment data from QCEW"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.caption.position = "plot",
    plot.caption = element_text(hjust = 0)
  ) +
  scale_color_manual(
    values = graph_color_scheme_2,
    name = "Exposure",
    labels = c("1", "2", "3", "4", "5")
  )

setwd(output_path)
ggsave("8_emloyment_by_quintile.png",plot = plot_employment, width = 8, height = 5, dpi = 300, bg = "white")


################################################################################
# Unemployment rate for recent graduates vs all other young workers

quint_5_young_df = cps_monthly_xposure %>%
  filter(unemp !=99, educ !=999) %>% # missing education
  mutate(exposure = ifelse(
    AIOE_quint_wgt == 5,
    "highly exposed (quintile 5)",
    "quintiles 1-4"
  )) %>% 
  filter(age >=22 & age<=27) %>%
  mutate(recent_grad = ifelse(
    educ >=111, # college and working grad students
    "recent graduate", "other young workers"
  )) %>%
  group_by(recent_grad, qdate_date, exposure) %>%
  summarise(unemp_rte = weighted.mean(unemp, weights = wtfinl, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(recent_grad = factor(recent_grad),
         exposure = factor(exposure)) %>%
  filter(!is.na(exposure)) %>%
  mutate(group_label = paste(recent_grad, exposure, sep = " - "))
  

plot_recent_grads <-
  ggplot(quint_5_young_df, aes(x = qdate_date, y = unemp_rte, group = group_label, color = group_label, linetype = exposure)) +
  
  geom_smooth(method = "loess", span = 0.5, se = FALSE, linewidth = 0.7) +
  
  geom_point(
    data = function(d) d %>% filter(!(format(qdate_date, "%Y-%m") %in% c("2020-04", "2020-05", "2020-06"))),
    alpha = 0.4,
    size = 2
  ) +
  
  scale_color_manual(
    values = c(
      "recent graduate - highly exposed (quintile 5)" = "#e1ad28",   # golden yellow
      "recent graduate - quintiles 1-4" = "#b35c00",                # darker orange
      "other young workers - highly exposed (quintile 5)" = "#234f8b", # dark blue
      "other young workers - quintiles 1-4" = "#b3d6dd"               # light blue
    )
  ) +
  
  labs(
    title = "Unemployment rate for recent graduates vs all other young workers",
    subtitle = "By exposure category",
    x = NULL,
    y = "unemployment (%)",
    color = "Exposure and \nDegree Group",
    linetype = "AI exposure",
    caption = "Note: Sample covers 22–27 year olds. AI exposure estimate crosswalked using weighted abilities-based exposure (Felten et al., 2021).\nSource: AI exposure from Felten et al. (2021), unemployment rates from CPS, author's calculations"
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    legend.box = "vertical",
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    plot.caption.position = "plot",
    plot.caption = element_text(hjust = 0)
  ) +
  guides(linetype = "none",
         color = guide_legend(nrow = 2, byrow = TRUE))

setwd(output_path)
ggsave("9_unemployment_recent_grads.png", plot = plot_recent_grads, width = 8, height = 5, dpi = 300)


################################################################################
# Quarterly 3-digit information sector jobs 1990-2025

info_sector_jobs <- ces_qtr_df %>% filter(nchar(industry_code) == 3) %>%
  filter(year_quarter>="2015-03-01") %>%
  mutate(`3-digit NAICS` = case_when(
    industry_name == "Broadcasting and content providers" ~ "Broadcasting (21.9%)",
    industry_name == "Computing infrastructure providers, data processing, web hosting, and related services" ~ "Data processing (34.7%)",
    industry_name == "Motion picture and sound recording industries" ~ "Motion picture (22.6%)",
    industry_name =="Telecommunications" ~ "Telecommunications (NA)",
    industry_name =="Publishing industries" ~ "Publishing (36.4%)",
    industry_name =="Web search portals, libraries, archives, and other information services" ~ "Other information services (33.4%)"
  ))


plot_info_sector_jobs <-
  ggplot(info_sector_jobs, aes(x = year_quarter, y = value, group = `3-digit NAICS`,
             color = `3-digit NAICS`)) +
 theme_minimal() +
  geom_point(size = 1.5, alpha = 0.5) +           # add points
  geom_smooth(se = FALSE, size = 0.5, method = "loess", span = 0.2) +
  labs(title = "Quarterly 3-digit information sector jobs 2015-2025",
       y = "jobs (thousands)",
       caption = "Note: AI uptake as of June 2025 in the legend\nSource: BLS Current Employment Statistics, BTOS for AI uptake",
       x = "") +
  scale_color_manual(
    values = c(
      "Broadcasting (21.9%)" = "#234f8b",
      "Data processing (34.7%)" = "#da9969",
      "Motion picture (22.6%)" = "#b3d6dd",
      "Telecommunications (NA)" = "#5e9c86",
      "Publishing (36.4%)" = "#e1ad28",
      "Other information services (33.4%)" = "#1a654d"
    )
  ) +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 8),
    plot.caption.position = "plot",
    plot.caption = element_text(hjust = 0)
  ) +
  guides(
    color = guide_legend(nrow = 2, byrow = TRUE)  # wrap to 2 rows
  )

setwd(output_path)
ggsave("10_employment_3digit_nacis.png", plot = plot_info_sector_jobs, width = 8, height = 5, dpi = 300)


################################################################################
# Change in Unemployment 2022/2023 to 2024/2025


quint_vars <- c(
  "AIOE_quint_wgt", "gpt4_beta_quint_admin", "human_beta_quint_admin",
  "estz_core_quint_admin", "pct_ai_quint" 
)

results <- data.frame(
  v = quint_vars,
  e = NA_real_,
  se = NA_real_
)


for (i in seq_along(quint_vars)) {
  vname <- quint_vars[i]
  
  df2 <- read_dta("cps_monthly_w_xposure_xwalked.dta") %>%
    filter(year >= 2022, !is.na(.data[[vname]])) %>%
    mutate(
      unemp = ifelse(empstat <= 29, as.integer(empstat >= 20 & empstat <= 29) * 100, NA),
      post = year >= 2024,
      tr = .data[[vname]] == 5,
      tr_post = tr * post
    )
  
  model <- lm(unemp ~ tr + post + tr_post, data = df2, weights = wtfinl)
  coefs <- summary(model)$coefficients
  
  results$e[i] <- coefs["tr_post", "Estimate"]
  results$se[i] <- coefs["tr_post", "Std. Error"]
}

results <- results %>%
  mutate(
    m = row_number(),
    up = e + 1.96 * se,
    dwn = e - 1.96 * se,
    label = factor(v, levels = quint_vars,
                   labels = c(
                     "Felten et al. (2021)",
                     "Eloundou et al. (2024) (gpt4)",
                     "Eloundou et al. (2024) (human)",
                     "Eisfeldt et al. (2023)",
                     "Webb (2022)"
                   ))
  )

ggplot(results, aes(x = label, y = e)) +
  geom_bar(stat = "identity", width = 0.75, fill = "gray70") +
  geom_errorbar(aes(ymin = dwn, ymax = up), width = 0.2) +
  labs(
    x = NULL,
    y = "change in unemployment",
    title = "Change in Unemployment 2022/23 to 2024/25"
  ) +
  scale_y_continuous(limits = c(-0.2, 0.5), breaks = seq(-0.2, 0.5, by = 0.2)) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  ) +
  coord_cartesian(clip = "off")


ggplot(results, aes(x = label, y = e)) +
  geom_bar(stat = "identity", width = 0.75, fill = "#b3d6dd") +
  geom_errorbar(aes(ymin = dwn, ymax = up), width = 0.2) +
  labs(
    x = NULL,
    y = "Change in Unemployment",
    title = "Change in Unemployment 2022/23 to 2024/25",
    caption = "Source: CPS, exposure from Felten et al. (2021), Eloundou et al. (2024), Eisfeldt et al. (2024), Webb (2022)"
  ) +
  scale_y_continuous(limits = c(-0.2, 0.5), breaks = seq(-0.2, 0.5, by = 0.2)) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.major.y = element_line(color = "grey80"),  # stronger y-gridlines
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),  # keep horizontal grid clean
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),  # box outline
    axis.text.y = element_text(hjust = 1, size = 10),
    axis.line = element_line(color = "black"),
    plot.title = element_text(size = 14),
    legend.position = "none",
    plot.caption.position = "plot",
    plot.caption = element_text(hjust = 0, size = 8)
    
  ) +
  coord_flip()


setwd(output_path)

ggsave("11b_d_in_d.png",  width = 8, height = 5, dpi = 300, bg = "white")



################################################################################
# Cited Statistics

# unemployment rates  
# Between 2022 and the beginning of 2025, 
# the unemployment rate for the most exposed workers 
# rose by X percentage points and X percentage points 
# for the least exposed workers. For all groups, however,
# their unemployment rates are lower today than they were in X. 

    # most exposed
    cps_monthly_xposure %>%
      filter(AIOE_quint_wgt == 5) %>%
      filter(year == 2022 | year == 2025) %>%
      filter(unemp !=99) %>%
      group_by(year, unemp) %>%
      summarise(tot = sum(wtfinl, na.rm = TRUE)) %>%
      ungroup() %>% group_by(year) %>%
      mutate(share = 100*tot/sum(tot)) %>% filter(unemp ==1) %>%
      ungroup() %>%
      arrange(year) %>% mutate(change = share - lag(share))

    # least exposed
    cps_monthly_xposure %>%
      filter(AIOE_quint_wgt == 1) %>%
      filter(year == 2022 | year == 2025) %>%
      filter(unemp !=99) %>%
      group_by(year, unemp) %>%
      summarise(tot = sum(wtfinl, na.rm = TRUE)) %>%
      ungroup() %>% group_by(year) %>%
      mutate(share = 100*tot/sum(tot)) %>% filter(unemp ==1) %>%
      ungroup() %>%
      arrange(year) %>% mutate(change = share - lag(share))


# unemployment rates by group over time -- where are they higher or lower?

    check_time_anchored <- cps_monthly_xposure %>%
      ungroup() %>%
      group_by(AIOE_quint_wgt, year, unemp) %>%
      summarise(tot = sum(wtfinl, na.rm = TRUE), .groups = "drop") %>%
      group_by(year, AIOE_quint_wgt) %>%
      mutate(share = 100 * tot / sum(tot)) %>%
      ungroup() %>%
      filter(unemp == 1) %>%
      select(year, AIOE_quint_wgt, share) %>%
      arrange(AIOE_quint_wgt, year) %>%
      group_by(AIOE_quint_wgt) %>%
      mutate(
        most_recent_year = max(year),
        share_most_recent = share[year == most_recent_year],
        pre_minus_post = share - share_most_recent
      ) %>%
      ungroup() %>%
      select(year, AIOE_quint_wgt, share, pre_minus_post)
