# GPS data cleaning
# Original author: Dani Berger (danielle.berger@usu.edu)
# Adapted by Kezia Manlove for SCV2 data structure
# AEN 05-06-26: outputs go to revisions, inputs read from leyna_data
# AEN 05-11-26: updated input to full dataset (Jan 2023 - Mar 2025)
# AEN 07-30-26: merged the former 01b (analysis-set restriction + DOP filter)
#   into this script so all GPS cleaning happens in one place
# AEN 08-06-26: switched to the deposited, pre-filtered GPS input (already
#   restricted to the 125-animal analysis set) and the merged covariate file
#   for the animal-ID crosswalk. Part B's restriction step is now a no-op check
#   rather than an actual filter, but is left in place as a safeguard.
#
# Part A (steps I-V) is the fix-level cleaning adapted from Dani Berger's code and
# mirrors Leyna's original pipeline. Part B is new in the revision: it confirms the
# 125-animal analysis set and applies a stricter DOP threshold.
#
# Input:  USDA_deer_23_Mar25_clean_125animals.csv (GPS, pre-filtered to 125 animals)
# Output: all_23-25_GPS_cleandf.rds    (fix-level cleaning only, 125 animals)
#         all_23-25_GPS_cleandf_v2.rds (analysis set, 125 animals) <- used downstream
#         {animal}_track2.csv          (per-animal tracks, used by the track figures)

# I.   Load required libraries and data
# II.  Specify cleaning parameters
# III. Set up data and reformat for AMT
# IV.  AMT cleaning steps
# V.   Convert the dataset to a df and save
# VI.  Restrict to the analysis set
# VII. DOP filter and save

library(devtools)
library(amt)
library(tidyverse)
library(sf)
library(lubridate)
library(here)

data_dir <- here("7_Dryad_Upload/data")
out_dir <- here("1_Data/revisions")

locs_in <- read.csv(here("7_Dryad_Upload/data/USDA_deer_23_Mar25_clean_125animals.csv"))

# CHECK: raw input
cat("--- Raw input ---\n")
cat("Rows:", nrow(locs_in), " (expect ~1,540,191)\n")
cat("Animals:", length(unique(locs_in$usdaID)), " (expect 125)\n")
cat("Date range:", min(locs_in$date_gmt), "to", max(locs_in$date_gmt), "\n")
cat("Study areas:", paste(sort(unique(locs_in$Study_area)), collapse = ", "), "\n")

dat_in <- locs_in

# II. Specify cleaning parameters
latlong_epsg <- 4326
utm_epsg <- 32616
cut_days <- 1
dop_highval <- 25   # value substituted for missing DOP, not a filter threshold
dop_max <- 10       # fixes above this are dropped in Part B
duplicate_interval_mins <- 10
speed_kph <- 3
epsilon_in <- 10

# III. Set up data and reformat for AMT
dat_in_sf <- st_as_sf(x = dat_in,
                      coords = c("Longitude.x", "Latitude.x"),
                      crs = latlong_epsg)
dat_in_sf <- st_transform(dat_in_sf, crs = st_crs(utm_epsg))
dat_in_coords <- as.data.frame(st_coordinates(dat_in_sf))

dat_in_sf$collarID_char <- as.character(dat_in_sf$DeviceID)
dat_in_sf$.x <- dat_in_coords$X
dat_in_sf$.y <- dat_in_coords$Y
dat_in_sf$timestamp <- as.POSIXct(dat_in_sf$date_gmt, format="%Y-%m-%d %H:%M:%S", tz="GMT", na.rm=TRUE)
dat_in_sf <- dat_in_sf[!(is.na(dat_in_sf$timestamp)), ]

data_track <- dat_in_sf %>%
  make_track(.x = .x,
             .y = .y,
             .t = timestamp,
             crs = utm_epsg,
             dop = PDOP,
             animal_id = usdaID,
             age = Age,
             sex = Sex,
             collar = DeviceID,
             collar_deployment_date = Capture_date,
             collar_end_date = Fate_date,
             Study_area = Study_area)

data_track <- data_track %>%
  mutate(animal_id = as.factor(animal_id)) %>%
  group_by(animal_id) %>%
  arrange(t_, .by_group = TRUE) %>%
  ungroup()

data_track_nest <- data_track %>% nest(data = -"animal_id")

# IV. AMT cleaning steps

## A. Remove fixes close to time of capture
data_track_nest <- data_track_nest %>%
  mutate(data = map(data, function(x)
    x %>% remove_capture_effect(start = days(cut_days))))

## B. Replace NA DOPs
data_track_nest1a <- data_track_nest %>%
  mutate(data = map(data, function(x)
    x %>% replace_na(list(dop = dop_highval))))

## C. Flag duplicates
data_track_nest1 <- data_track_nest1a %>%
  mutate(data = map(data, function(x)
    x %>% flag_duplicates(gamma = minutes(duplicate_interval_mins))))

data_track_nest1 %>%
  mutate(n_dup = map_int(data, ~sum(.x$duplicate_))) %>%
  select(animal_id, n_dup) %>%
  print(n = Inf)

data_track_nest2 <- data_track_nest1 %>%
  mutate(data = map(data, function(x)
    x %>% filter(duplicate_ == FALSE)))

## D. Flag fast steps
delta <- calculate_sdr(speed = speed_kph,
                       time = minutes(60), speed_unit = ("km/h"))

data_track_nest3 <- data_track_nest2 %>%
  mutate(data = map(data, function(x)
    x %>% flag_fast_steps(delta = delta)))

## E. Remove flagged rows
gps_unnest <- data_track_nest3 %>%
  mutate(nrow = map_dbl(data, nrow)) %>%
  filter(nrow > 0) %>%
  unnest(cols = data) %>%
  mutate(animal_id = factor(animal_id))

# CHECK: post-flagging, pre-drop
cat("--- Post-flagging (pre-drop) ---\n")
cat("Rows:    ", nrow(gps_unnest), "\n")
cat("Animals: ", length(unique(gps_unnest$animal_id)), " (expect 125+)\n")
cat("Study areas: ", paste(sort(unique(gps_unnest$Study_area)), collapse = ", "), "\n")

gps_flagdrop <- gps_unnest %>%
  filter(duplicate_ == FALSE &
           fast_step_ == FALSE)

# V. Convert and save
gps_cleaned_df <- as.data.frame(gps_flagdrop)

gps_cleaned_sf <- st_as_sf(x = gps_cleaned_df,
                            coords = c("x_", "y_"),
                            crs = utm_epsg)
gps_cleaned_latlongs <- st_transform(gps_cleaned_sf, crs = st_crs(latlong_epsg))
gps_latlong_coords <- as.data.frame(st_coordinates(gps_cleaned_latlongs))
gps_cleaned_df <- cbind(gps_cleaned_df, gps_latlong_coords)
names(gps_cleaned_df)[which(names(gps_cleaned_df) == "X")] <- "Longitude"
names(gps_cleaned_df)[which(names(gps_cleaned_df) == "Y")] <- "Latitude"
names(gps_cleaned_df)[which(names(gps_cleaned_df) == "x_")] <- "UTME"
names(gps_cleaned_df)[which(names(gps_cleaned_df) == "y_")] <- "UTMN"

# CHECK: fix-level cleaned data
cat("--- Fix-level cleaned output ---\n")
cat("Rows:", nrow(gps_cleaned_df), "(fix-level cleaning only, some rows dropped from the",
    nrow(locs_in), "input fixes; further filtered by DOP in Part B)\n")
cat("Animals:", length(unique(gps_cleaned_df$animal_id)), "(expect 125)\n")
cat("Date range:", format(min(gps_cleaned_df$t_)), "to", format(max(gps_cleaned_df$t_)), "\n")
cat("Study areas:", paste(sort(unique(gps_cleaned_df$Study_area)), collapse = ", "), "\n")

# Per-animal track files. Written at this stage (before the analysis-set
# restriction) because the track figures read them directly.
individs <- levels(factor(gps_cleaned_df$animal_id))
for(i in 1:length(individs)){
  k <- subset(gps_cleaned_df, animal_id == individs[i])
  write.csv(k, file.path(out_dir, paste0(individs[i], "_track2.csv")))
  print(i)
}

write.csv(gps_cleaned_df, file.path(out_dir, "all_23-25_GPS_clean.csv"))
write_rds(gps_cleaned_sf,  file.path(out_dir, "all_23-25_GPS_clean.rds"))
write.csv(gps_unnest,      file.path(out_dir, "all_23-25_GPS_unestTF.csv"))
write_rds(gps_cleaned_df,  file.path(out_dir, "all_23-25_GPS_cleandf.rds"))

# ─────────────────────────────────────────────────────────────────────────────
# Part B — restrict to the analysis set and apply the DOP filter
# (formerly script 01b, AEN 05-19-26)
# ─────────────────────────────────────────────────────────────────────────────

gps_df <- gps_cleaned_df

# VI. Confirm the 125-animal analysis set
# The GPS input above is already restricted to the 125-animal analysis set
# (see 7_Dryad_Upload/dryad_manifest.md). This step re-derives the same animal
# list from road_encounter_covariates.csv (D-format IDs, mapped to usdaIDs via
# ID.1 in deer_meta.csv) and filters again as a safeguard — it should be a no-op.
# ILTN1049 appears in the covariate file as D1124 (recaptured animal given a new
# low tag); ID.1 resolves it.
meta       <- read.csv(file.path(data_dir, "deer_meta.csv"))
analysis_ids <- unique(read.csv(file.path(data_dir,
  "road_encounter_covariates.csv"))$AnimalID)

keep_ids <- meta$usdaID[meta$ID.1 %in% analysis_ids]

n_before_restrict <- length(unique(gps_df$animal_id))
gps_df <- gps_df %>%
  filter(as.character(animal_id) %in% keep_ids)

cat("\n--- After confirming the analysis set ---\n")
cat("Animals:", length(unique(gps_df$animal_id)), "(expect 125; before this step:",
    n_before_restrict, ")\n")

# Fix interval diagnostic (reporting only, filters nothing).
# BaBA errors when an animal's modal interval exceeds the interval parameter
# (0.5 hr = 30 min), so check before running script 02.
interval_summary <- gps_df %>%
  arrange(animal_id, t_) %>%
  group_by(animal_id) %>%
  mutate(interval_min = as.numeric(difftime(t_, lag(t_), units = "mins"))) %>%
  summarise(
    modal_interval_min = as.numeric(names(which.max(table(round(interval_min, 0))))),
    min_interval_min   = min(interval_min, na.rm = TRUE),
    .groups = "drop"
  )

cat("\n--- Animals with modal interval > 30 min (would cause a BaBA error) ---\n")
print(interval_summary %>% filter(modal_interval_min > 30) %>% arrange(desc(modal_interval_min)))

cat("\n--- Animals with minimum interval < 10 min (data quality concern) ---\n")
print(interval_summary %>% filter(min_interval_min < 10) %>% arrange(min_interval_min))

# VII. DOP filter and save
# DOP_type is not carried through make_track, so a single threshold is applied
# (PDOP cutoff from WTDiSSA; removes clearly unreliable fixes).
n_before_dop <- nrow(gps_df)
gps_df <- gps_df %>% filter(dop <= dop_max)

cat("\n--- After DOP filter (dop >", dop_max, "removed) ---\n")
cat("Fixes removed:", n_before_dop - nrow(gps_df), "\n")
cat("Rows:", nrow(gps_df), "\n")
cat("Animals:", length(unique(gps_df$animal_id)), "(expect 125)\n")

write_rds(gps_df, file.path(out_dir, "all_23-25_GPS_cleandf_v2.rds"))
cat("\nSaved: all_23-25_GPS_cleandf_v2.rds\n")
