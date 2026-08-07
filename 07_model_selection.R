# Model selection AIC values — fit all 16 candidate models
# Fits every combination of the four predictor groups shown in the model
# selection table, each with random intercepts for site and AnimalID, on a
# single common dataset so the AIC values are directly comparable. Writes
# model_selection_full_aic.csv, which tables/table_model_selection.R reads.
# Predictor groups:
#   Land cover  : std_forest + std_agri + std_dev
#   Season+diel : season + time.cat
#   Road        : std_rdden + road_category
#   Demographics: Age + Sex
# AEN 07-13-26
# AEN 08-06-26: switched to the merged deposit file (road_encounter_covariates.csv);
#   see 06_logistic_model.R for the equivalent-join verification.

library(tidyverse)
library(glmmTMB)
library(here)

data_dir <- here("7_Dryad_Upload/data")
out_dir <- here("1_Data/revisions")

# ── Load data ──────────────────────────────────────────────────────────────────

df5 <- read.csv(file.path(data_dir, "road_encounter_covariates.csv")) %>%
  rename(site = Study_area) %>%
  mutate(
    type = factor(type),
    road_category = factor(road_category),
    season = factor(season),
    time.cat = factor(time.cat),
    Sex = factor(Sex),
    Age = factor(Age)
  )

# All models must fit on identical rows for AIC to be comparable
stopifnot(all(!is.na(df5$road_category)), all(!is.na(df5$Sex)), all(!is.na(df5$Age)))
cat("Fitting 16 models on", nrow(df5), "encounters\n")

# ── Predictor groups ──────────────────────────────────────────────────────────

groups <- list(
  land_cover = c("std_forest", "std_agri", "std_dev"),
  season_diel = c("season", "time.cat"),
  road = c("std_rdden", "road_category"),
  demo = c("Age", "Sex")
)
re_terms <- "(1 | site) + (1 | AnimalID)"

# ── Fit all 2^4 combinations ──────────────────────────────────────────────────

combos <- expand.grid(rep(list(c(FALSE, TRUE)), length(groups)))
names(combos) <- names(groups)

fit_aic <- function(incl) {
  fixed <- unlist(groups[as.logical(incl)])
  rhs <- paste(c(fixed, re_terms), collapse = " + ")
  form <- as.formula(paste("type ~", rhs))
  AIC(glmmTMB(form, data = df5, family = binomial(link = "logit")))
}

combos$AIC <- vapply(seq_len(nrow(combos)),
  function(i) fit_aic(combos[i, names(groups)]), numeric(1))

results <- combos %>%
  mutate(across(all_of(names(groups)), ~ ifelse(.x, "✓", ""))) %>%
  mutate(delta_AIC = AIC - min(AIC)) %>%
  arrange(delta_AIC)

write.csv(results, file.path(out_dir, "model_selection_full_aic.csv"), row.names = FALSE)
results
