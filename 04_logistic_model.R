# Logistic mixed-effects model: quick cross vs. bounce
# AIC-based selection between linear and log-transformed land cover / road density terms.
# AEN 05-22-26; revised 07-06-26 (AIC-based selection between linear and log
# land cover / road density terms; removed combined linear+log model per AE comment)
# AEN 08-06-26: now reads road_encounter_covariates.csv directly — used to join
#   two derived files here to fix road_category, that's done once at deposit time now.
#   road_category: L = large (primary/secondary/motorway), M = medium
#   (tertiary/unclassified/residential), S = small, per OSM (2024)

library(tidyverse)
library(glmmTMB)
library(ggeffects)
library(DHARMa)
library(performance)
library(here)

data_dir <- here("7_Dryad_Upload/data")

# --- Load data ---
df5 <- read.csv(file.path(data_dir, "road_encounter_covariates.csv")) %>%
  rename(site = Study_area) %>%
  mutate(
    type = factor(type),
    road_category = factor(road_category),
    season = factor(season),
    time.cat = factor(time.cat)
  )

# Confirm categories and no NAs introduced by join
stopifnot(all(!is.na(df5$road_category)))
cat("Road category counts:\n")
print(table(df5$road_category))

# --- Fit candidate models ---
# Two candidates: linear vs. log-transformed land cover and road density terms.
# All other predictors (season, diel period, road category, random effects) are
# held constant. AIC selects the transformation for the final model.

out_dir <- here("1_Data/revisions")

model_lin <- glmmTMB(
  type ~ std_forest + std_agri + std_dev +
    (1 | site) + (1 | AnimalID) +
    season + time.cat + std_rdden + road_category,
  data = df5,
  family = binomial(link = "logit")
)

model_log <- glmmTMB(
  type ~ std_lnforest + std_lndev + std_lnagri +
    (1 | site) + (1 | AnimalID) +
    season + time.cat + stdln_rdden + road_category,
  data = df5,
  family = binomial(link = "logit")
)

# --- AIC comparison ---
aic_tab <- data.frame(
  model = c("linear", "log"),
  AIC = c(AIC(model_lin), AIC(model_log))
) %>%
  mutate(delta_AIC = AIC - min(AIC)) %>%
  arrange(delta_AIC)
aic_tab

write.csv(aic_tab, file.path(out_dir, "model_selection_aic.csv"), row.names = FALSE)

# --- Select the top model ---
top_model <- if (aic_tab$model[1] == "linear") model_lin else model_log

summary(top_model)
diagnose(top_model)

# --- Save model coefficients ---
coef_mat <- summary(top_model)$coefficients$cond
coefs <- as.data.frame(coef_mat) %>%
  tibble::rownames_to_column("term") %>%
  rename(estimate = Estimate, se = `Std. Error`, z = `z value`, p = `Pr(>|z|)`) %>%
  mutate(
    or = exp(estimate),
    ci_low = exp(estimate - 1.96 * se),
    ci_high = exp(estimate + 1.96 * se)
  )

write.csv(coefs, file.path(out_dir, "model_top_coefs.csv"), row.names = FALSE)

# --- Model diagnostics ---
sim_res <- simulateResiduals(top_model)
plot(sim_res)
testDispersion(sim_res)

check_collinearity(top_model)

# --- Predicted probability plots ---
pr_road <- ggpredict(top_model, "road_category", bias_correction = TRUE)
pr_season <- ggpredict(top_model, "season", bias_correction = TRUE)
pr_time <- ggpredict(top_model, "time.cat", bias_correction = TRUE)

plot(pr_road)
plot(pr_season)
plot(pr_time)
