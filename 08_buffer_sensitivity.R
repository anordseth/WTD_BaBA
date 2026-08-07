# Buffer-size sensitivity analysis
# Reruns BaBA encounter detection/classification across a range of road buffer
# distances (20-80 m) to check how encounter detection responds to buffer size,
# and to justify the 50 m buffer used in the main analysis.
#
# Uses the same parameter-testing subset as the original sensitivity analysis
# (all three sites, January 2023 - August 2024) and the same BaBA() settings as
# 02_baba_classify_encounters.R.
#
# Output: 1_Data/revisions/buffer_sensitivity_results.csv
# Runtime note: this is slow (BaBA is rerun once per buffer per site, and LSV runs
# per-animal). Best run on the workstation. Results are written after each buffer
# so partial progress is saved if interrupted.
# AEN 07-11-26

library(tidyverse)
library(sf)
library(BaBA)
library(here)

data_dir <- here("1_Data/leyna_data")
out_dir  <- here("1_Data/revisions")

# ── Load GPS data and subset to the parameter-testing window ──────────────────

gps <- readRDS(file.path(out_dir, "all_23-25_GPS_cleandf_v2.rds")) %>%
  filter(t_ >= as.POSIXct("2023-01-01", tz = "UTC"),
         t_ <= as.POSIXct("2024-08-31", tz = "UTC"))
cat("Subset fixes:", nrow(gps), " animals:", length(unique(gps$animal_id)), "\n")

# ── Roads (South = SIU, TON; North = LSV) ─────────────────────────────────────

spatial_dir <- file.path(data_dir, "spatial")
unzip(file.path(spatial_dir, "Trans_Road_Sroadsutm.zip"), exdir = spatial_dir, overwrite = FALSE)
unzip(file.path(spatial_dir, "Trans_Road_Nroadsutm.zip"), exdir = spatial_dir, overwrite = FALSE)
shp_S <- st_transform(st_read(file.path(spatial_dir, "Trans_Road_Sroadsutm.shp"), quiet = TRUE), 32616)
shp_N <- st_transform(st_read(file.path(spatial_dir, "Trans_Road_Nroadsutm.shp"), quiet = TRUE), 32616)

prep_gps <- function(df) {
  s <- st_as_sf(df, coords = c("Longitude", "Latitude"), crs = 4326)
  s <- st_transform(s, 32616); s$date <- s$t_; s$Animal.ID <- s$animal_id; s
}
gps_SIU <- prep_gps(subset(gps, Study_area == "SIUC"))
gps_TON <- prep_gps(subset(gps, Study_area == "Touch of Nature"))
gps_LSV <- prep_gps(subset(gps, Study_area == "Shelbyville"))

# ILSV1185 is a short-track animal whose interval check fails at 0.5 hr; run at 1
# (same handling as 02_baba_classify_encounters.R).
lsv_override <- c(ILSV1185 = 1)

# ── BaBA wrappers (settings identical to script 02) ───────────────────────────

run_baba <- function(g, roads, d, iv = 0.5) {
  tryCatch(
    BaBA(g, roads, d, interval = iv, b_time = 4, p_time = 36, w = 168,
      tolerance = 0, units = "hours", max_cross = 2, sd_multiplier = 1,
      exclude_buffer = FALSE, round_fixes = FALSE, export_images = FALSE)$classification,
    error = function(e) { message("   err: ", e$message); NULL })
}
run_lsv <- function(g, roads, d) {
  res <- list()
  for (id in levels(factor(g$Animal.ID))) {
    iv <- if (id %in% names(lsv_override)) lsv_override[[id]] else 0.5
    c1 <- run_baba(subset(g, Animal.ID == id), roads, d, iv)
    if (!is.null(c1)) res[[id]] <- c1
  }
  if (length(res)) do.call(rbind, res) else NULL
}

# ── Loop over buffer distances, tally event types per site ────────────────────

ba_types <- c("Quick_Cross", "Average_Movement", "Bounce",
              "Back_n_forth", "Trace", "Trapped", "unknown")

tally <- function(c1, d, site) {
  if (is.null(c1)) return(NULL)
  counts <- as.list(table(factor(c1$eventTYPE, levels = ba_types)))
  data.frame(buffer = d, site = site, total_enc = nrow(c1), counts)
}

buffers <- c(20, 30, 40, 50, 60, 70, 80)
rows <- list()
for (d in buffers) {
  cat("=== buffer", d, "m ===\n")
  t0 <- proc.time()[3]
  rows[[length(rows) + 1]] <- tally(run_baba(gps_SIU, shp_S, d), d, "SIU")
  rows[[length(rows) + 1]] <- tally(run_baba(gps_TON, shp_S, d), d, "TON")
  rows[[length(rows) + 1]] <- tally(run_lsv(gps_LSV, shp_N, d), d, "LSV")
  cat(sprintf("  buffer %d done (%.0f min elapsed)\n", d, (proc.time()[3] - t0) / 60))
  write.csv(bind_rows(rows), file.path(out_dir, "buffer_sensitivity_results.csv"),
            row.names = FALSE)
}

# ── Summary: encounter detection relative to the 50 m value ───────────────────

res <- bind_rows(rows)
cat("\n===== total encounters and quick cross, as % of the 50 m value =====\n")
res %>% group_by(site) %>% arrange(buffer) %>%
  mutate(total_pct_50 = round(100 * total_enc / total_enc[buffer == 50], 1),
         qc_pct_50     = round(100 * Quick_Cross / Quick_Cross[buffer == 50], 1)) %>%
  select(site, buffer, total_enc, total_pct_50, Quick_Cross, qc_pct_50) %>%
  as.data.frame() %>% print()

cat("\nSaved: 1_Data/revisions/buffer_sensitivity_results.csv\n")
