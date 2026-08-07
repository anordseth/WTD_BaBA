# Temporal autocorrelation diagnostics
# (1) Inter-encounter interval summary per individual
# (2) DHARMa testTemporalAutocorrelation on model255 residuals
# Supports response to R2-M4 (temporal autocorrelation concern).
# AEN 05-26-26
# AEN 08-06-26: switched to the merged deposit file (road_encounter_covariates.csv);
#   see 06_logistic_model.R for the equivalent-join verification.

library(tidyverse)
library(glmmTMB)
library(DHARMa)
library(here)

out_dir <- here("1_Data/revisions")

load(file.path(out_dir, "BaBA_all_AEN.Rdata"))
cat("Total encounters:", nrow(BaBA_all), "\n")

# ── Calculate inter-encounter intervals ───────────────────────────────────────

intervals <- BaBA_all %>%
  arrange(AnimalID, start_time) %>%
  group_by(AnimalID) %>%
  mutate(
    interval_hrs = as.numeric(difftime(start_time, lag(end_time), units = "hours"))
  ) %>%
  filter(!is.na(interval_hrs), interval_hrs >= 0) %>%
  ungroup()

cat("\n--- Inter-encounter interval summary (hours) ---\n")
summary(intervals$interval_hrs)

cat("\nMedian:", round(median(intervals$interval_hrs), 1), "hours\n")
cat("Mean:  ", round(mean(intervals$interval_hrs), 1), "hours\n")
cat("Min:   ", round(min(intervals$interval_hrs), 1), "hours\n")
cat("Max:   ", round(max(intervals$interval_hrs), 1), "hours\n")
cat("% > 1 hr:", round(mean(intervals$interval_hrs > 1) * 100, 1), "%\n")
cat("% > 4 hrs:", round(mean(intervals$interval_hrs > 4) * 100, 1), "%\n")

# ── Temporal autocorrelation test on model255 residuals ───────────────────────
# Fit model255 on df5 (quick cross vs bounce), join encounter start times via
# burstID, then test DHARMa residuals sorted by time within individual.

data_dir <- here("7_Dryad_Upload/data")

df5 <- read.csv(file.path(data_dir, "road_encounter_covariates.csv")) %>%
  rename(site = Study_area) %>%
  mutate(
    type = factor(type),
    road_category = factor(road_category),
    season = factor(season),
    time.cat = factor(time.cat)
  )

model255 <- glmmTMB(
  type ~ std_lnforest + std_lndev + std_lnagri +
    std_forest + std_agri + std_dev +
    (1 | site) + (1 | AnimalID) +
    season + time.cat + std_rdden + stdln_rdden + road_category,
  data = df5,
  family = binomial(link = "logit")
)

# Join start_time from our rerun BaBA data (all 3 sites) via burstID
# BaBA_all uses ILSI/ILSV/ILTN prefix; df5 uses D prefix — strip to match
encounter_times <- BaBA_all %>%
  select(burstID, start_time) %>%
  mutate(
    burstID = sub("^IL[A-Z]{2}", "D", burstID),
    start_time = as.POSIXct(start_time)
  )

df5_times <- df5 %>%
  left_join(encounter_times, by = "burstID") %>%
  arrange(start_time)

sim_res <- simulateResiduals(model255)

# Test within each individual (unique time values per individual)
df5_times$dharma_resid <- residuals(sim_res)

cat("\n--- Join diagnostics ---\n")
cat("start_time NAs in df5_times:", sum(is.na(df5_times$start_time)), "\n")
cat("burstIDs in df5 matching BaBA_all:", sum(df5$burstID %in% BaBA_all$burstID), "\n")
cat("Sample burstIDs in df5:", head(df5$burstID), "\n")
cat("Sample burstIDs in BaBA_all:", head(BaBA_all$burstID), "\n")

autocor_results <- df5_times %>%
  filter(!is.na(start_time), !is.na(dharma_resid), is.finite(dharma_resid)) %>%
  arrange(AnimalID, start_time) %>%
  group_by(AnimalID) %>%
  filter(n() >= 5) %>%
  nest() %>%
  mutate(
    n_encounters = map_int(data, nrow),
    dw_p = map_dbl(data, ~ car::durbinWatsonTest(lm(.$dharma_resid ~ 1))$p)
  ) %>%
  select(-data)

cat("\n--- Temporal autocorrelation test per individual (Durbin-Watson) ---\n")
cat("N individuals tested:", nrow(autocor_results), "\n")
cat("% with p < 0.05:", round(mean(autocor_results$dw_p < 0.05) * 100, 1), "%\n")
cat("Median p-value:", round(median(autocor_results$dw_p), 3), "\n")
print(summary(autocor_results$dw_p))
