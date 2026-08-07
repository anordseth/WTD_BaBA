# Per-deer summaries: tracking effort, site totals, and road encounter rates
#
# Requires: all_23-25_GPS_cleandf.rds (script 01), BaBA_all_AEN.Rdata (script 02),
#           deer_meta.csv
# Writes:   site_summary_AEN.csv        site totals for the SI
#           individual_summary_AEN.csv  per-deer tracking summary
#           encounter_rates_AEN.csv     road encounters per deer per day (feeds Fig. 2B)
#           table_data_summary_round2.docx  formatted SI table
#
# Fix counts describe data surviving the fix-level cleaning in script 01, which is
# what the SI table reports: data collected and cleaned, rather than the subset the
# model was fit on. The analysis-set restriction is applied first so every output
# here covers the same 125 animals.

library(tidyverse)
library(flextable)
library(here)

out_dir <- here("1_Data/revisions")

gps_cleaned_df <- readRDS(file.path(out_dir, "all_23-25_GPS_cleandf.rds"))
deer_meta <- read.csv(file.path(here("7_Dryad_Upload/data"), "deer_meta.csv"))
load(file.path(out_dir, "BaBA_all_AEN.Rdata"))

sex_lookup <- deer_meta %>% select(usdaID, Sex) %>% distinct()
gps_cleaned_df <- gps_cleaned_df %>%
  mutate(animal_id = as.character(animal_id)) %>%
  left_join(sex_lookup, by = c("animal_id" = "usdaID"))

# Restrict to the animals carried into the BaBA analysis. Script 01 keeps 129 in
# the fix-level file; the classified set is 125. Without this the site summary
# reported TON as 72 rather than 68.
analyzed_ids <- unique(as.character(BaBA_all$AnimalID))
gps_cleaned_df <- gps_cleaned_df %>% filter(animal_id %in% analyzed_ids)

# ── Site-level summary ────────────────────────────────────────────────────────
site_summary <- gps_cleaned_df %>%
  group_by(Study_area) %>%
  summarise(
    `N animals` = n_distinct(animal_id),
    `N locations` = n(),
    `Sex (F/M)` = paste0(n_distinct(animal_id[Sex == "Female"]), "/", n_distinct(animal_id[Sex == "Male"]))
  ) %>%
  rename(Site = Study_area)

write.csv(site_summary, file.path(out_dir, "site_summary_AEN.csv"), row.names = FALSE)

# ── Individual-level summary ─────────────────────────────────────────────────
individual_summary <- gps_cleaned_df %>%
  group_by(animal_id) %>%
  summarise(
    Site = first(Study_area),
    Sex = first(Sex),
    Age = first(age),
    `N fixes` = n(),
    `Track start` = format(min(t_), "%Y-%m-%d"),
    `Track end` = format(max(t_), "%Y-%m-%d"),
    `Duration (days)` = round(as.numeric(difftime(max(t_), min(t_), units = "days")), 1)
  ) %>%
  rename(`Animal ID` = animal_id) %>%
  relocate(Site, .before = `Animal ID`) %>%
  arrange(Site, `Animal ID`) %>%
  mutate(
    Site = recode(Site,
      "SIUC" = "SIU", "Shelbyville" = "LSV", "Touch of Nature" = "TON"),
    Sex = recode(Sex, "Female" = "F", "Male" = "M"),
    Age = recode(Age, "YoungOfYear" = "Young of year"))

# Encounter rates join on the full ID, so keep it until after that join. The
# displayed table drops the 4-char state/site prefix (e.g. ILSV1172 -> 1172); the
# Site column carries the site and the stripped numbers remain unique.
individual_full_id <- individual_summary
individual_summary <- individual_summary %>% mutate(`Animal ID` = str_sub(`Animal ID`, 5))

write.csv(individual_summary, file.path(out_dir, "individual_summary_AEN.csv"), row.names = FALSE)

# The formatted table goes to the manuscript supplement folder when it is present
# (the full project), and otherwise next to the csv (a clone of the code alone).
supp_dir <- here("4_Manuscript/round2/Supplement")
docx_dir <- if (dir.exists(supp_dir)) supp_dir else out_dir

flextable(individual_summary) %>%
  align(align = "left", part = "all") %>%
  autofit() %>%
  save_as_docx(path = file.path(docx_dir, "table_data_summary_round2.docx"))

# ── Road encounter rate per individual per day ───────────────────────────────
# Each deer's rate is its total BaBA encounters divided by its days tracked (first
# fix to last fix). Days tracked includes days with no encounters, so this is
# equivalent to averaging the deer's daily encounter counts.
encounter_rates <- BaBA_all %>%
  group_by(AnimalID, Study_area) %>%
  summarise(n_encounters = n(), .groups = "drop") %>%
  mutate(AnimalID = as.character(AnimalID)) %>%
  left_join(
    individual_full_id %>% ungroup() %>% select(`Animal ID`, `Duration (days)`),
    by = c("AnimalID" = "Animal ID")
  ) %>%
  mutate(
    site = case_when(
      Study_area == "SIUC" ~ "SIU",
      Study_area == "Shelbyville" ~ "LSV",
      Study_area == "Touch of Nature" ~ "TON"
    ),
    days_tracked = `Duration (days)`,
    enc_per_day = n_encounters / days_tracked
  ) %>%
  select(AnimalID, site, n_encounters, days_tracked, enc_per_day)

stopifnot(
  nrow(encounter_rates) == 125,
  !any(is.na(encounter_rates$days_tracked)),
  all(encounter_rates$days_tracked > 0)
)

write.csv(encounter_rates, file.path(out_dir, "encounter_rates_AEN.csv"), row.names = FALSE)

# ── Summaries reported in the manuscript ─────────────────────────────────────
se <- function(x) sd(x) / sqrt(length(x))

overall <- encounter_rates %>%
  summarise(
    n_deer = n(),
    mean = mean(enc_per_day),
    SE = se(enc_per_day),
    median = median(enc_per_day),
    min = min(enc_per_day),
    max = max(enc_per_day)
  )

by_site <- encounter_rates %>%
  group_by(site) %>%
  summarise(
    n_deer = n(),
    mean = mean(enc_per_day),
    SE = se(enc_per_day),
    median = median(enc_per_day),
    min = min(enc_per_day),
    max = max(enc_per_day),
    .groups = "drop"
  )

cat("\n=== Encounter rate per individual per day, all sites ===\n")
print(as.data.frame(overall %>% mutate(across(where(is.numeric), ~ round(.x, 2)))))

cat("\n=== By site ===\n")
print(as.data.frame(by_site %>% mutate(across(where(is.numeric), ~ round(.x, 2)))))

cat("\nPooled check (total encounters / total deer-days):",
    round(sum(encounter_rates$n_encounters) / sum(encounter_rates$days_tracked), 2), "\n")
