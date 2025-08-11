
# DATE LAST UPDATED: 07/29/2025
# PROJECT: AI Employment Impacts
# AUTHOR: Sarah Eckhardt
# DESCRIPTION: Create charts and cited stats for appendix

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
library(lubridate)
library(purrr)
library(ggcorrplot)

# set project paths
user_path = "ENTER-USER-PATH"
project_path = file.path(user_path, "AI-Unemployment")
data_path = file.path(project_path, "data/1raw")
wrangled_path = file.path(project_path, "data/2wrangled")
output_path = file.path(project_path, "data/3final/appendix")
agglom_path = file.path(project_path, "data/3final/agglomerations")

setwd(wrangled_path)

# read in data
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


quint_xposure_vars <- c(
  "AIOE_quint_wgt",
  "gpt4_beta_quint_admin", "human_beta_quint_admin",
  "estz_core_quint_admin",
  "pct_ai_quint"
)


# read in ORG data
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

# chglf: change in LF status
cps_org[, chglf := fifelse(
  !is.na(lfstat) & !is.na(lfstat_lead) &
    lfstat != lfstat_lead,
  TRUE, FALSE
)]


# add year-month and year-quarter dates
cps_org <- cps_org %>%
  mutate(ym = as.yearmon(paste(year, month), "%Y %m")) %>%
  mutate(yq = as.yearqtr(ym)) # year quarter

cps_org = cps_org %>%
  filter(year < 2024)

# read in EIG logo for figure formatting
logo <- readPNG(file.path(data_path, "EIG_fullcolor.png"))
logo_grob <- rasterGrob(
  logo,
  x = 0.99,  # move slightly left from the right edge (1 → 0.98)
  y = 0.02,  # move slightly up from the bottom (0 → 0.05)
  hjust = 1,
  vjust = 0,
  width = unit(0.15, "npc")
)

colors_measures <- c(
  "Felten" = "#234f8b",
  "Eloundou (human)" = "#e1ad28",
  "Eloundou (gpt4)" = "#d08f00",
  "Eisfeldt" = "#b3d6dd",
  "Webb" = "#984ea3"
)

colors_measures_full <- c(
  "Felten" = "#234f8b",
  "Eloundou (human)" = "#e1ad28",
  "Eloundou (gpt4)" = "#d08f00",
  "Eisfeldt" = "#b3d6dd",
  "Webb" = "#984ea3",
  "All workers" = "grey40"
)

# Linetypes (dotted only for overall)
linetypes <- c(
  "Felten" = "solid",
  "Eloundou (human)" = "solid",
  "Eloundou (gpt4)" = "solid",
  "Eisfeldt" = "solid",
  "Webb" = "solid",
  "All workers" = "dotted"
)


################################################################################
# correlation matrix #

setwd(wrangled_path)
xposure_felten_final <- read.csv("xposure_felten_measures_xwalk.csv")

xposure_felten_final = xposure_felten_final %>%
  rename(`Ability-based ("truth")` = AIOE_wgt,
         `Official crosswalk` = AIOE_admin,
         `O*NET crosswalk` = AIOE_sim)

custom_order <- c(
  'Ability-based ("truth")', "Official crosswalk", "O*NET crosswalk")


cor_matrix <- xposure_felten_final %>%
  select(all_of(custom_order)) %>%
  cor(use = "pairwise.complete.obs", method = "pearson")

cor_matrix <- cor_matrix[custom_order, custom_order]

ggcorrplot(
  cor_matrix,
  type = "lower",
  lab = TRUE,
  lab_size = 3,
  outline.col = "white",
  colors = c("#6D9EC1", "white", "#E46767"),
  show.diag = TRUE  # <- this is key
) +
  coord_fixed(ratio = 1.1) +         # Control tile size ratio (try 1.1–1.5)
  
  theme_minimal(10) +
  labs(x = NULL, y = NULL,
       title = "Figure 9: Evaluation of crosswalking approaches") +
  theme(
    plot.title = element_text(family = "sans", face = "bold", color = "#387e65", size = 14, hjust =4),
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    legend.title = element_blank(),
    legend.text = element_text(size = 8),
    legend.key.height = unit(0.5, "cm"),
    legend.key.width = unit(1, "cm")
  )

setwd(output_path)
ggsave("1_corr_matrix.png",  width = 10, height = 6, dpi = 300, bg = "white")



################################################################################
# unemployment rate for the most exposed workers across measures

# Preallocate an empty list to collect filtered data
all_q5_df <- list()
all_q5_binned <- list()

# Loop over all quintile variables, filtering for quintile 5
for (graph_var in quint_xposure_vars) {
  
  label_name <- case_when(
    grepl("AIOE", graph_var) ~ "Felten",
    
    grepl("beta", graph_var) &  grepl("human", graph_var) ~ "Eloundou (human)",
    grepl("beta", graph_var) &  grepl("gpt4", graph_var) ~ "Eloundou (gpt4)",
    
    grepl("estz", graph_var) ~ "Eisfeldt",
    
    graph_var == "pct_ai_quint" ~ "Webb"
  )
  
  print(label_name)
  
  # Get raw and binned data for quintile 5 only
  q5_data <- cps_monthly_xposure %>%
    filter(unemp != 99, !!sym(graph_var) == 5) %>%
    ungroup() %>%
    group_by(qdate_date) %>%
    summarise(
      unemp = weighted.mean(unemp, wtfinl, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(source = label_name)
  
  # Bin the series (30 bins)
  q5_binned <- q5_data %>%
    mutate(bin = ntile(qdate_date, 30)) %>%
    group_by(bin, source) %>%
    summarise(
      qdate_center = as.Date(median(qdate_date, na.rm = TRUE)),
      unemp_bin = mean(unemp, na.rm = TRUE),
      .groups = "drop"
    )
  
  all_q5_df[[graph_var]] <- q5_data
  all_q5_binned[[graph_var]] <- q5_binned
}

# Combine into single dataframes
combined_q5_df <- bind_rows(all_q5_df)
combined_q5_binned <- bind_rows(all_q5_binned)



overall_unemp_df <- cps_monthly_xposure %>%
  filter(unemp != 99) %>%
  group_by(qdate_date) %>%
  summarise(unemp = weighted.mean(unemp, wtfinl, na.rm = TRUE), .groups = "drop") %>%
  mutate(source = "All workers")

# Combine all data for line plotting
all_data_with_overall <- bind_rows(combined_q5_df, overall_unemp_df)

plot_unemp_all <- ggplot(all_data_with_overall, aes(x = qdate_date, y = unemp, color = source, linetype = source)) +
  geom_smooth(method = "loess", span = 0.5, se = FALSE, linewidth = 0.5) +
  geom_point(data = combined_q5_binned,
             aes(x = qdate_center, y = unemp_bin, color = source),
             size = 2, alpha = 0.4) +
  scale_color_manual(
    values = colors_measures_full,
    name = "AI exposure source"
  ) +
  scale_linetype_manual(
    values = linetypes,
    guide = "none"  # hides linetype legend
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Figure 10: Unemployment for most exposed workers, all exposure measures",
    y = "unemployment (%)",
    x = "",
    caption = "Note: Grey dotted line shows unemployment for all workers.\nSource: CPS, exposure from Felten et al. (2021), Eloundou et al. (2024), Eisfeldt et al. (2024), Webb (2022)."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(family = "sans", face = "bold", color = "#387e65", size = 14, hjust = -1.8),
    legend.position = "bottom",
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    plot.caption.position = "plot",
    plot.caption = element_text(hjust = 0)
  ) +
  guides(color = guide_legend(nrow = 2, byrow = TRUE))

plot_unemp_all <- ggdraw(plot_unemp_all) +  draw_grob(logo_grob)


setwd(output_path)
ggsave("2_unemp_all_measures.png", width = 8, height = 5, dpi = 300)


# v2 for the short agglomerations piece
plot_unemp_all_agglom <- ggplot(all_data_with_overall, aes(x = qdate_date, y = unemp, color = source, linetype = source)) +
  geom_smooth(method = "loess", span = 0.5, se = FALSE, linewidth = 0.5) +
  geom_point(data = combined_q5_binned,
             aes(x = qdate_center, y = unemp_bin, color = source),
             size = 2, alpha = 0.4) +
  scale_color_manual(
    values = colors_measures_full,
    name = "AI exposure source"
  ) +
  scale_linetype_manual(
    values = linetypes,
    guide = "none"  # hides linetype legend
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Figure 1: Unemployment for most exposed workers, all exposure measures",
    y = "unemployment (%)",
    x = "",
    caption = "Note: Grey dotted line shows unemployment for all workers.\nSource: CPS, exposure from Felten et al. (2021), Eloundou et al. (2024), Eisfeldt et al. (2024), Webb (2022)."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(family = "sans", face = "bold", color = "#387e65", size = 14, hjust = -1.2),
    legend.position = "bottom",
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    plot.caption.position = "plot",
    plot.caption = element_text(hjust = 0)
  ) +
  guides(color = guide_legend(nrow = 2, byrow = TRUE))

plot_unemp_all_agglom <- ggdraw(plot_unemp_all_agglom) +  draw_grob(logo_grob)

setwd(agglom_path)
ggsave("1_unemp_all_measures.png", plot_unemp_all_agglom, width = 8, height = 5, dpi = 300)


################################################################################
# share of workers exiting the labor force by AI exposure (quintile 5 only)

lfpr_change_list <- list()
lfpr_change_binned <- list()

for (graph_var in quint_xposure_vars) {
  
  label_name <- case_when(
    grepl("AIOE", graph_var) ~ "Felten",
    grepl("beta", graph_var) & grepl("human", graph_var) ~ "Eloundou (human)",
    grepl("beta", graph_var) & grepl("gpt4", graph_var) ~ "Eloundou (gpt4)",
    grepl("estz", graph_var) & grepl("core", graph_var) ~ "Eisfeldt",
    graph_var == "pct_ai_quint" ~ "Webb"
  )
  
  # Raw data
  tmp_df <- cps_org %>%
    filter(year >= 2015, !is.na(!!sym(graph_var))) %>%
    filter(!!sym(graph_var) == 5) %>%
    group_by(yq) %>%
    summarise(
      toue = weighted.mean(toue, finalwgt, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(source = label_name)
  
  # Binned data
  binned_df <- tmp_df %>%
    mutate(bin = ntile(yq, 30)) %>%
    group_by(bin, source) %>%
    summarise(
      qdate_center = as.Date(median(yq)),
      toue_bin = mean(toue),
      .groups = "drop"
    )
  
  lfpr_change_list[[graph_var]] <- tmp_df
  lfpr_change_binned[[graph_var]] <- binned_df
}

# Combine all
combined_lfpr_df <- bind_rows(lfpr_change_list)
combined_lfpr_binned <- bind_rows(lfpr_change_binned)

combined_lfpr_binned = combined_lfpr_binned %>%
  filter(toue_bin< 0.025)


quarterly_means <- map_dfr(quint_xposure_vars, function(var) {
  cps_org %>%
    filter(year >= 2015, .data[[var]] %in% 1:4) %>%
    group_by(yq) %>%
    summarise(
      varname = var,
      toue = weighted.mean(toue, finalwgt, na.rm = TRUE),
      .groups = "drop"
    )
})

overall_lfpr_df <- quarterly_means %>%
  mutate(source = "All other workers")

# Combine for full line plotting
all_lfpr_df <- bind_rows(combined_lfpr_df, overall_lfpr_df)

# Plot
lf_leaving_all <- ggplot(all_lfpr_df, aes(x = as.Date(yq), y = toue, color = source, linetype = source)) +
  geom_smooth(method = "loess", se = FALSE, span = 0.5, linewidth = 0.6) +
  geom_point(data = combined_lfpr_binned,
             aes(x = qdate_center, y = toue_bin, color = source),
             size = 2, alpha = 0.4) +
  scale_color_manual(
    values = colors_measures_full,
    name = "AI exposure source"
  ) +
  scale_linetype_manual(
    values = linetypes,
    guide = "none"  # Hide linetype from legend
  ) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = "Figure 11: Share of most exposed workers exiting the labor force, all exposure measures",
    y = "% exiting labor force",
    x = NULL,
    caption = "Note: Quarter 2 2020 values above 2.5% removed. Grey dotted line shows labor force exiting trend for\n   quintiles 1–4 combined.\nSource: CPS ORG from Economic Policy Institute, exposure from Felten et al. (2021), Eloundou et al. (2024), \n  Eisfeldt et al. (2024), Webb (2022)."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(family = "sans", face = "bold", color = "#387e65", size = 14, hjust = 1.3),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.position = "bottom",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9),
    plot.caption.position = "plot",
    plot.caption = element_text(hjust = 0)
  ) +
  guides(color = guide_legend(nrow = 2, byrow = TRUE))

lf_leaving_all <- ggdraw(lf_leaving_all) +  draw_grob(logo_grob)


setwd(output_path)
ggsave("3_lfpr_switching_quint5.png", , width = 8.5, height = 5, dpi = 300)



################################################################################
# Share of workers chainging occupation by AI exposure (quinile 5 only)

# Preallocate list for plot data
occ_change_list <- list()
occ_change_binned <- list()

for (graph_var in quint_xposure_vars) {
  
  label_name <- case_when(
    grepl("AIOE", graph_var) ~ "Felten",
    grepl("beta", graph_var) & grepl("human", graph_var) ~ "Eloundou (human)",
    grepl("beta", graph_var) & grepl("gpt4", graph_var) ~ "Eloundou (gpt4)",
    grepl("estz", graph_var) & grepl("core", graph_var) ~ "Eisfeldt",
    graph_var == "pct_ai_quint" ~ "Webb"
  )
  
  # Raw data
  tmp_df <- cps_org %>%
    filter(year >= 2015, !is.na(!!sym(graph_var))) %>%
    filter(!!sym(graph_var) == 5) %>%
    group_by(yq) %>%
    summarise(
      chgocc = weighted.mean(chgocc, finalwgt, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(source = label_name)
  
  # Binned data
  binned_df <- tmp_df %>%
    mutate(bin = ntile(yq, 30)) %>%
    group_by(bin, source) %>%
    summarise(
      qdate_center = as.Date(median(yq)),
      chgocc_bin = mean(chgocc),
      .groups = "drop"
    )
  
  occ_change_list[[graph_var]] <- tmp_df
  occ_change_binned[[graph_var]] <- binned_df
}

# Combine all
combined_occ_df <- bind_rows(occ_change_list)
combined_occ_binned <- bind_rows(occ_change_binned)

quarterly_means <- map_dfr(quint_xposure_vars, function(var) {
  cps_org %>%
    filter(year >= 2015, .data[[var]] %in% 1:4) %>%
    group_by(yq) %>%
    summarise(
      varname = var,
      chgocc = weighted.mean(chgocc, finalwgt, na.rm = TRUE),
      .groups = "drop"
    )
})

overall_occ_df <- quarterly_means %>%
  mutate(source = "All other workers")

all_occ_df <- bind_rows(combined_occ_df, overall_occ_df)

occ_switching_all <- ggplot(all_occ_df, aes(x = as.Date(yq), y = chgocc, color = source, linetype = source)) +
  geom_smooth(method = "loess", se = FALSE, span = 0.5, linewidth = 0.6) +
  geom_point(data = combined_occ_binned,
             aes(x = qdate_center, y = chgocc_bin, color = source),
             size = 2, alpha = 0.4) +
  scale_color_manual(
    values = colors_measures_full,
    name = "AI exposure source"
  ) +
  scale_linetype_manual(
    values = linetypes,
    guide = "none"  # hides linetype from legend
  ) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = "Figure 12: Share of most exposed workers switching occupations, all exposure measures",
    y = "% changing occupation",
    x = NULL,
    caption = "Note: Grey dotted line shows occupation switching trend for quintiles 1–4 combined.\nSource: CPS ORG from Economic Policy Institute, exposure from Felten et al. (2021), Eloundou et al. (2024), \n  Eisfeldt et al. (2024), Webb (2022)."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(family = "sans", face = "bold", color = "#387e65", size = 14, hjust = 1.15),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.position = "bottom",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9),
    plot.caption.position = "plot",
    plot.caption = element_text(hjust = 0)
  ) +
  guides(color = guide_legend(nrow = 2, byrow = TRUE))

occ_switching_all <- ggdraw(occ_switching_all) +  draw_grob(logo_grob)


setwd(output_path)
ggsave("4_occ_switching_quint5.png", occ_switching_all, width = 8.5, height = 5, dpi = 300)


################################################################################
# Share of workers switching to lower-exposure jobs


exposure_pairs <- list(
  AIOE_quint_wgt = "chgocc_lowerexp_AIOE_quint_wgt",
  gpt4_beta_quint_admin = "chgocc_lowerexp_gpt4_beta_quint_admin",
  human_beta_quint_admin = "chgocc_lowerexp_human_beta_quint_admin",
  estz_core_quint_admin = "chgocc_lowerexp_estz_core_quint_admin",
  pct_ai_quint = "chgocc_lowerexp_pct_ai_quint"
)


# data for quint 5
org_charts_long_multi <- purrr::imap_dfr(
  exposure_pairs,
  function(switch_var, quintile_var) {
    cps_org %>%
      filter(get(quintile_var) == 5) %>%
      group_by(yq) %>%
      summarise(
        value = weighted.mean(get(switch_var), finalwgt, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        exposure_measure = quintile_var
      )
  }
)

overall_trend <- purrr::imap_dfr(
  exposure_pairs,
  function(switch_var, quintile_var) {
    cps_org %>%
      filter(.data[[quintile_var]] %in% 1:4) %>%
      group_by(yq) %>%
      summarise(
        value = weighted.mean(.data[[switch_var]], finalwgt, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        exposure_label = "All other workers"
      )
  }
)


org_charts_long_multi <- org_charts_long_multi %>%
  mutate(
    exposure_label = recode(exposure_measure,
                            "AIOE_quint_wgt" = "Felten",
                            "gpt4_beta_quint_admin" = "Eloundou (gpt4)",
                            "human_beta_quint_admin" = "Eloundou (human)",
                            "estz_core_quint_admin" = "Eisfeldt",
                            "pct_ai_quint" = "Webb"
    )
  )



combined_data <- bind_rows(org_charts_long_multi, overall_trend)

lower_exposure_overall <- ggplot(combined_data, aes(x = as.Date(yq), y = value, color = exposure_label, linetype = exposure_label)) +
  geom_smooth(method = "loess", se = FALSE, span = 0.5, linewidth = 0.5) +
  geom_point(alpha = 0.3, size = 1.5) +
  theme_minimal(base_size = 12) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_color_manual(values = colors_measures_full) +
  scale_linetype_manual(values = linetypes, guide = "none") +
  labs(
    x = "",
    y = "% switching occupation",
    title = "Figure 13: Share of most exposed workers switching to lower exposure occupations, all exposure measures",
    subtitle = "Among most-exposed workers (quintile 5), by AI exposure measure",
    color = "AI exposure source",
    caption = "Note: Grey dashed line shows occupation switching trend for quintiles 1–4 combined.\nSource: CPS ORG from the Economic Policy Institute, exposure from Felten et al. (2021), Eloundou et al. (2024), \n   Eisfeldt et al. (2024)."
  ) +
  theme(
    plot.title = element_text( family = "sans", face = "bold", color = "#387e65", size = 14, hjust = 0.9),
    plot.subtitle = element_text(hjust  = -0.14),
    legend.position = "bottom",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9),
    plot.caption = element_text(hjust = 0)
  ) +
  guides(color = guide_legend(nrow = 2, byrow = TRUE))


lower_exposure_overall <- ggdraw(lower_exposure_overall) +  draw_grob(logo_grob)



setwd(output_path)
ggsave("5_switching_low_xposure_quint5.png", lower_exposure_overall, width = 10, height = 5, dpi = 300, bg = "white")


################################################################################
# Percent of businesses using AI (last 2 weeks)

setwd(data_path)
btos_df = read_excel("National.xlsx") %>%
  mutate(state = "National") %>%
  filter(`Question ID`=="7", `Answer ID` =="1") %>%
  select(-`Question ID`, -`Answer ID`, -`Question`, -`Answer`) %>%
  pivot_longer(cols = c(contains("20")), names_to = "period", values_to = "use_ai")

btos_plot_df = btos_df %>%
  
  # create a functional date variable
  mutate(
    period = as.numeric(period),
    use_ai = as.numeric(gsub("%","",use_ai))/100,
    year = period %/% 100,
    sample = period %% 100,
    # Approximate date: assume each biweekly sample is 14 days apart, starting Jan 1
    date = as.Date(paste0(year, "-01-01")) + (sample - 1) * 14
  ) %>%
  
  # add fit line
  mutate(
    t = as.numeric(date),
    t2 = t^2,
    t3 = t^3
  )

model <- lm(use_ai ~ t + t2 + t3, data = btos_plot_df)

# Get predictions + confidence intervals
btos_plot_df <- btos_plot_df %>%
  mutate(
    yhat = predict(model),
    se = predict(model, se.fit = TRUE)$se.fit,
    ub = yhat + 1.96 * se,
    lb = yhat - 1.96 * se
  )

plot_ai_use <- ggplot(btos_plot_df, aes(x = date, y = use_ai)) +
  geom_point(color = "#1f77b4", alpha = 0.5) +
  geom_line(aes(y = yhat), color = "#1f77b4") +
  geom_ribbon(aes(ymin = lb, ymax = ub), alpha = 0.2, fill = "#1f77b4") +
  scale_y_continuous(labels = scales::percent_format()) +
  
  labs(
    x = "", 
    y = "share using AI",
    title = "Figure 14: Percent of businesses using AI (last 2 weeks)",
    caption = "Source: Business Trends and Outlook Survey"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text( family = "sans", face = "bold", color = "#387e65", size = 14, hjust = -0.3),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.position = "none",
    plot.caption.position = "plot",
    plot.caption = element_text(hjust = 0)
  )

plot_ai_use = ggdraw(plot_ai_use) + draw_grob(logo_grob)

setwd(output_path)
ggsave("6_ai_use_btos.png", plot = plot_ai_use, width = 8, height = 5, dpi = 300, bg = "white")



################################################################################
# Share of companies that report using AI in....


setwd(data_path)
adoption_industry <- read_excel("Sector.xlsx") %>%
  filter(`Question ID`%in% c("7", "26"),
         `Answer ID` =="1") %>%
  select(Sector, `Question`,`Question ID`, `202513`) %>%
  filter(!is.na(Sector),
         `202513` !="S") %>%
  rename(response = `202513`) %>%
  mutate(period = 202513,
         response = as.numeric(gsub("%", "", response))/100,
         year = period %/% 100,
         sample = period %% 100,
         # Approximate date: assume each biweekly sample is 14 days apart, starting Jan 1
         date = as.Date(paste0(year, "-01-01")) + (sample - 1) * 14
  ) %>% select(-period, -sample, -year) %>%
  mutate(question = case_when(
    `Question ID` == "7" ~ "last two weeks",
    `Question ID` == "26" ~ "next six months"
  ))


adoption_industry_graph = adoption_industry %>%
  select(Sector, question, response) %>%
  mutate(Sector_label = case_when(
    Sector == "23" ~ "Construction",
    Sector == "31" ~ "Manufacturing",
    Sector == "42" ~ "Wholesale Trade",
    Sector == "44" ~ "Retail Trade",
    Sector == "48" ~ "Transportation",
    Sector == "51" ~ "Information",
    Sector == "52" ~ "Finance & Insurance",
    Sector == "53" ~ "Real Estate",
    Sector == "54" ~ "Professional Services",
    Sector == "55" ~ "Management",
    Sector == "56" ~ "Admin Support",
    Sector == "61" ~ "Education",
    Sector == "62" ~ "Health Care",
    Sector == "71" ~ "Entertainment",
    Sector == "72" ~ "Accomodation and Food",
    Sector == "81" ~ "Other"
  ))


sector_order <- adoption_industry_graph %>%
  distinct(Sector, Sector_label) %>%
  arrange(as.numeric(Sector)) %>%
  pull(Sector_label)

# Apply ordered factor to Sector_label
adoption_industry_graph <- adoption_industry_graph %>%
  mutate(Sector_label = factor(Sector_label, levels = sector_order))


sector_ai_use <- ggplot(adoption_industry_graph, aes(x = Sector_label, y = response, fill = question)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  labs(
    x = "",
    y = "",
    fill = "",
    title = "Figure 15: Percent of businesses using AI by sector",
    legend = NULL,
    caption = "Source: Buisness Trends and Outlook Survey, survey period June 18th, 2025",
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_manual(
    values = c("#e1ad28", "#b3d6dd")  # gold and light blue
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text( family = "sans", face = "bold", color = "#387e65", size = 14, hjust = -0.25),
    axis.text.x = element_text(angle = 50, hjust = 1),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.position = "top",
    plot.caption.position = "plot",
    plot.caption = element_text(hjust = 0))

sector_ai_use = ggdraw(sector_ai_use) + draw_grob(logo_grob)


setwd(output_path)
ggsave("7_sector_ai_use.png", sector_ai_use, width = 8, height = 5, dpi = 300, bg = "white")
