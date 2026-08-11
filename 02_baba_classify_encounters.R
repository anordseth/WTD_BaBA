# animal = deer
# barrier = roads
#
# AEN 05-06-26: Added Study_area merge (Study_area not in cleaned RDS)
# AEN 05-07-26: Extended from SIU-only to all three sites (SIU, TON, LSV)
#   - Added prep_gps() helper to avoid repeated setup code
#   - Road shapefile assignment: South roads (Trans_Road_Sroadsutm) cover TON and SIU
#     (37.6-37.7N); North roads (Trans_Road_Nroadsutm) cover LSV (39.5N) — verified
#     by bbox check on both shapefiles
#   - max_cross = 2 used for all sites to match original SIU run; Leyna's original
#     scripts used max_cross = 0 for TON and LSV — CONFIRM with Leyna which was used
#     for the deposited data (affects back-and-forth/trace/unknown counts in Fig 2;
#     does not affect bounce or quick cross)
#   - Saves per-site Rdata files plus combined BaBA_all_AEN.Rdata
#   - Added encounter frequency summary for Fig 2
# AEN 08-10-26: dropped the unused deer_meta.csv read. Road shapefiles aren't in
#   the deposit (public USGS NTD geometry, not redistributed) — see README's
#   spatial data setup section

# Load libraries
library(tidyverse)
library(ggplot2)
library(here)
#install.packages("devtools")
#devtools::install_github("wx-ecology/BaBA")
library(BaBA)
library(dplyr)
#install.packages("osmdata")
#library(osmdata)
library(sf)
library(mapview)
#install.packages("epiDisplay")
library(epiDisplay)

data_dir <- here("1_Data/leyna_data")       # road shapefiles — see note above
deposit_dir <- here("7_Dryad_Upload/data")  # GPS_cleaned_analysisset_125animals.csv
out_dir <- here("1_Data/revisions")

gps_cleaned_df <- read.csv(file.path(deposit_dir, "GPS_cleaned_analysisset_125animals.csv"))
gps_cleaned_df$t_ <- as.POSIXct(gps_cleaned_df$t_, format = "%Y-%m-%d %H:%M:%S", tz = "GMT")

cat("--- Input ---\n")
cat("Animals:", length(unique(gps_cleaned_df$animal_id)), "\n")

# Study_area carried through from the GPS-cleaning step (not published here) via make_track
# Note: short-track animals (ILSI1201, ILSV1185, ILTN1142, ILTN3229) are present in
# Leyna's 125 — do not filter by track duration. Monitor for BaBA errors on these animals.

# road shapefiles
# South roads cover SIU (37.71N) and TON (37.63N)
# North roads cover LSV (39.51N)
spatial_dir <- file.path(data_dir, "spatial")
unzip(file.path(spatial_dir, "Trans_Road_Sroadsutm.zip"), exdir = spatial_dir, overwrite = FALSE)
shp_roadSouth <- st_transform(st_read(file.path(spatial_dir, "Trans_Road_Sroadsutm.shp")), crs = 32616)
unzip(file.path(spatial_dir, "Trans_Road_Nroadsutm.zip"), exdir = spatial_dir, overwrite = FALSE)
shp_roadNorth <- st_transform(st_read(file.path(spatial_dir, "Trans_Road_Nroadsutm.shp")), crs = 32616)

prep_gps <- function(df) {
  sf_obj <- st_as_sf(df, coords = c("Longitude", "Latitude"), crs = 4326)
  sf_obj <- st_transform(sf_obj, crs = 32616)
  sf_obj$date <- sf_obj$t_
  sf_obj$Animal.ID <- sf_obj$animal_id
  sf_obj
}

d <- 50

# --- SIU ---
gps_SIU <- prep_gps(subset(gps_cleaned_df, Study_area == "SIUC"))
BaBA_SIU <- BaBA(gps_SIU, shp_roadSouth, d, interval = 0.5, b_time = 4, p_time = 36, w = 168,
                 tolerance = 0, units = "hours", max_cross = 2, sd_multiplier = 1,
                 exclude_buffer = FALSE, round_fixes = FALSE, export_images = FALSE)
save(BaBA_SIU, file = file.path(out_dir, "BaBA_SIU_AEN.Rdata"))

# --- TON ---
gps_TON <- prep_gps(subset(gps_cleaned_df, Study_area == "Touch of Nature"))
BaBA_TON <- BaBA(gps_TON, shp_roadSouth, d, interval = 0.5, b_time = 4, p_time = 36, w = 168,
                 tolerance = 0, units = "hours", max_cross = 2, sd_multiplier = 1,
                 exclude_buffer = FALSE, round_fixes = FALSE, export_images = FALSE)
save(BaBA_TON, file = file.path(out_dir, "BaBA_TON_AEN.Rdata"))

# --- LSV ---
gps_LSV <- prep_gps(subset(gps_cleaned_df, Study_area == "Shelbyville"))
individs_LSV <- levels(factor(gps_LSV$Animal.ID))

lsv_results  <- list()
lsv_skipped  <- c()

# ILSV1185 is a short-track animal (~27 hrs of data). Its modal fix interval is
# 0.5 hr but BaBA's internal interval check fails, likely due to irregular gaps
# in the short track. Running with interval = 1 succeeds and produces 4
# encounters; included to match Leyna's n = 125. Produces a straightness-index
# warning (too few fixes in window) but all encounters are classified.
lsv_interval_override <- c(ILSV1185 = 1)

for (i in seq_along(individs_LSV)) {
  id <- individs_LSV[i]
  k  <- subset(gps_LSV, Animal.ID == id)
  iv <- if (id %in% names(lsv_interval_override)) lsv_interval_override[[id]] else 0.5
  result <- tryCatch(
    BaBA(k, shp_roadNorth, d, interval = iv, b_time = 4, p_time = 36, w = 168,
         tolerance = 0, units = "hours", max_cross = 2, sd_multiplier = 1,
         exclude_buffer = FALSE, round_fixes = FALSE, export_images = FALSE),
    error = function(e) { message("Skipping ", id, ": ", e$message); NULL }
  )
  if (!is.null(result)) lsv_results[[id]] <- result$classification
  else lsv_skipped <- c(lsv_skipped, id)
  cat(i, "/", length(individs_LSV), "\n")
}

cat("LSV skipped:", length(lsv_skipped), "\n")
print(lsv_skipped)

enc_LSV <- do.call(rbind, lsv_results)
BaBA_LSV <- list(classification = enc_LSV)
save(BaBA_LSV, file = file.path(out_dir, "BaBA_LSV_AEN.Rdata"))

# --- Combine and save ---
enc_SIU <- BaBA_SIU$classification; enc_SIU$Study_area <- "SIUC"
enc_TON <- BaBA_TON$classification; enc_TON$Study_area <- "Touch of Nature"
enc_LSV <- BaBA_LSV$classification; enc_LSV$Study_area <- "Shelbyville"

BaBA_all <- rbind(enc_SIU, enc_TON, enc_LSV)
save(BaBA_all, file = file.path(out_dir, "BaBA_all_AEN.Rdata"))

# CHECK: BaBA output
cat("--- BaBA encounter counts ---\n")
cat("SIU:", nrow(enc_SIU), "\n")
cat("TON:", nrow(enc_TON), "\n")
cat("LSV:", nrow(enc_LSV), "\n")
cat("Total:", nrow(BaBA_all), "(expect ~110,000)\n")
print(table(BaBA_all$eventTYPE))

# --- Encounter frequency summary ---
freq_summary <- BaBA_all %>%
  count(Study_area, eventTYPE) %>%
  group_by(Study_area) %>%
  mutate(prop = n / sum(n))

freq_summary
write.csv(freq_summary, file.path(out_dir, "freq_summary_AEN.csv"), row.names = FALSE)

##FXNS - source kept in leyna_scripts/BaBA_deer_SIUcondense_github.R for reference #AEN 05-06-26
