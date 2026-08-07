# Supplemental Table 1 — Model selection table (revised)
# Addresses R1-SI Table 1: no word descriptions; consistent covariate order;
# sorted by ascending delta AIC; top model bolded.
# AEN 05-25-26

library(tidyverse)
library(flextable)
library(here)

# ── Model data ────────────────────────────────────────────────────────────────
# AIC values are fit in 07_model_selection.R (all 16 combinations of the
# four predictor groups, each with random intercepts for site and AnimalID) and
# read here so the table always matches the fitted models.
#   Land cover  : std_forest + std_agri + std_dev
#   Season/diel : season + time.cat
#   Road        : std_rdden + road_category
#   Demographics: Age + Sex

models <- read.csv(here("1_Data/revisions/model_selection_full_aic.csv")) %>%
  mutate(across(c(land_cover, season_diel, road, demo), ~ ifelse(is.na(.x), "", .x))) %>%
  arrange(delta_AIC) %>%
  mutate(
    AIC = formatC(AIC, format = "d", big.mark = ","),
    delta_AIC = formatC(delta_AIC, format = "d", big.mark = ",")
  )

# ── Build flextable ───────────────────────────────────────────────────────────

ft <- flextable(models) %>%
  set_header_labels(
    land_cover   = "Land cover",
    season_diel  = "Season + diel period",
    road         = "Road",
    demo         = "Demographics",
    AIC          = "AIC",
    delta_AIC    = "ΔAIC"
  ) %>%
  bold(i = ~ delta_AIC == "0") %>%
  align(j = c("land_cover", "season_diel", "road", "demo"), align = "center", part = "all") %>%
  align(j = c("AIC", "delta_AIC"), align = "right", part = "all") %>%
  autofit() %>%
  theme_booktabs() %>%
  font(fontname = "Times New Roman", part = "all") %>%
  fontsize(size = 10, part = "all")

# ── Save ──────────────────────────────────────────────────────────────────────

save_as_docx(ft, path = here("4_Manuscript/round2/Supplement/table_model_selection_round2.docx"))

ft
