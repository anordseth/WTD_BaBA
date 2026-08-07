# Table 1 — Top model coefficient table (revised)
# Addresses R1-SI Table 2: moved to main text; removes asterisks; adds odds ratios and 95% CIs.
# Updated 07-06-26: switched to model_top_coefs.csv (linear terms selected via AIC); removed ln() rows.
# AEN 05-25-26

library(tidyverse)
library(flextable)
library(here)

data_dir <- here("1_Data/revisions")

# ── Load coefficients ─────────────────────────────────────────────────────────

coefs <- read.csv(file.path(data_dir, "model_top_coefs.csv"))

# ── Clean and label ───────────────────────────────────────────────────────────

coefs_clean <- coefs %>%
  filter(term != "(Intercept)") %>%
  mutate(
    group = case_when(
      str_detect(term, "forest|agri|dev") ~ "Land cover",
      str_detect(term, "season") ~ "Season",
      str_detect(term, "time\\.cat") ~ "Diel period",
      str_detect(term, "rdden|road_category") ~ "Road"
    ),
    label = case_when(
      term == "std_forest" ~ "Forest",
      term == "std_agri" ~ "Agriculture",
      term == "std_dev" ~ "Development",
      term == "std_rdden" ~ "Road density",
      term == "seasonpostfawn" ~ "Post-fawning",
      term == "seasonprefawn" ~ "Pre-fawning",
      term == "seasonrut" ~ "Rut",
      term == "time.catnighttime" ~ "Night",
      term == "time.cattwilight" ~ "Twilight",
      term == "road_categoryM" ~ "Medium",
      term == "road_categoryS" ~ "Small"
    ),
    group = factor(group, levels = c("Land cover", "Season", "Diel period", "Road"))
  ) %>%
  arrange(group) %>%
  mutate(
    p = case_when(
      p < 0.001 ~ "< 0.001",
      TRUE ~ sprintf("%.3f", p)
    ),
    or = sprintf("%.2f", or),
    ci = paste0(sprintf("%.2f", ci_low), "–", sprintf("%.2f", ci_high))
  ) %>%
  select(group, label, or, ci, p)

# ── Build flextable ───────────────────────────────────────────────────────────

ft <- flextable(coefs_clean) %>%
  set_header_labels(
    group = "Group",
    label = "Variable",
    or = "OR",
    ci = "95% CI",
    p = "p"
  ) %>%
  merge_v(j = "group") %>%
  valign(j = "group", valign = "top") %>%
  align(j = c("or", "ci", "p"), align = "right", part = "all") %>%
  autofit() %>%
  theme_booktabs() %>%
  font(fontname = "Times New Roman", part = "all") %>%
  fontsize(size = 10, part = "all")

# ── Save ──────────────────────────────────────────────────────────────────────

save_as_docx(ft, path = here("4_Manuscript/round2/Supplement/table1_model_coefs_round2.docx"))

ft
