# BaBA trajectory examples figure
# Real GPS track panels from BaBA output, grouped by altered/unaltered status
# cf. Joly et al. 2025 (https://doi.org/10.1038/s41598-025-10216-6) Fig. 2
# AEN 05-19-26

library(tidyverse)
library(sf)
library(patchwork)
library(cowplot)
library(grid)
library(here)

out_dir  <- here("1_Data/revisions")
data_dir <- here("1_Data/leyna_data")
fig_dir  <- here("3_Figures/round2/supplemental")

# ── Load data ─────────────────────────────────────────────────────────────────

load(file.path(out_dir, "BaBA_all_AEN.Rdata"))
meta <- read.csv(file.path(data_dir, "deer_meta.csv"))

d_to_usda <- setNames(meta$usdaID, meta$ID.1)

resolve_id <- function(id) {
  id <- as.character(id)
  ifelse(startsWith(id, "D"), d_to_usda[id], id)
}

spatial_dir <- file.path(data_dir, "spatial")
unzip(file.path(spatial_dir, "Trans_Road_Sroadsutm.zip"), exdir = spatial_dir, overwrite = FALSE)
shp_S <- st_transform(st_read(file.path(spatial_dir, "Trans_Road_Sroadsutm.shp")), crs = 32616)
unzip(file.path(spatial_dir, "Trans_Road_Nroadsutm.zip"), exdir = spatial_dir, overwrite = FALSE)
shp_N <- st_transform(st_read(file.path(spatial_dir, "Trans_Road_Nroadsutm.shp")), crs = 32616)

get_road <- function(study_area) {
  if (study_area == "Shelbyville") shp_N else shp_S
}

get_track <- function(animal_id) {
  usda_id <- resolve_id(animal_id)
  f <- file.path(out_dir, paste0(usda_id, "_track2.csv"))
  if (!file.exists(f)) { warning("No track2 for: ", usda_id); return(NULL) }
  df <- read.csv(f)
  df$t_ <- as.POSIXct(df$t_, tz = "UTC")
  df
}

get_encounter_fixes <- function(enc, context_n = 4, enc_extend = 1) {
  track <- get_track(enc$AnimalID)
  if (is.null(track)) return(NULL)
  win_fixes <- track %>% filter(t_ >= enc$start_time, t_ <= enc$end_time)
  if (nrow(win_fixes) == 0) return(NULL)
  first_enc_idx <- which(track$t_ == min(win_fixes$t_))
  last_enc_idx  <- which(track$t_ == max(win_fixes$t_))
  # Extend the highlighted encounter by enc_extend fixes on each side. A fast
  # crossing often puts the first far-side fix just outside the encounter window,
  # so without this the crossing fix reads as "after" (grey) and the panel looks
  # one-sided; extending by one fix colors the crossing itself.
  enc_lo <- max(1, first_enc_idx - enc_extend)
  enc_hi <- min(nrow(track), last_enc_idx + enc_extend)
  idx_start <- max(1, first_enc_idx - context_n)
  idx_end   <- min(nrow(track), last_enc_idx + context_n)
  track[idx_start:idx_end, ] %>%
    mutate(row_idx = idx_start:idx_end,
           phase = case_when(
             row_idx < enc_lo ~ "before",
             row_idx > enc_hi ~ "after",
             TRUE             ~ "encounter"
           )) %>%
    select(-row_idx)
}

# ── Per-panel settings ────────────────────────────────────────────────────────
# manual:       single-row tibble to override auto-selection; NULL = auto
# context_n:    number of GPS fixes shown before/after encounter window
# spatial_half: half-extent in metres; NULL = zoom to track automatically

# Manual overrides — set to NULL to use auto_select, or specify a single-row
# tibble from BaBA_all to pin a specific encounter.
# Original intended IDs (verify format in BaBA_all before restoring):
#   Quick_Cross:  "ILSV1192_1270.5"
#   Back_n_forth: "D3013_2928" (may need IL-format: "ILTN3013_2928")
#   Trapped:      "ILSV3067_764.6"

manual <- list(
  Bounce           = NULL,
  Quick_Cross      = NULL,
  Back_n_forth     = NULL,
  Trace            = NULL,
  Trapped          = NULL,
  Average_Movement = NULL
)

context_n <- list(
  Bounce           = 2,
  Quick_Cross      = 2,
  Back_n_forth     = 2,
  Trace            = 2,
  Trapped          = 2,
  Average_Movement = 2
)

# Fixes to add to each side of the encounter window when highlighting (purple).
# Back-and-forth needs a wider reach so the across-road fix that completes the
# pattern is included; a fast quick cross needs one to color the far-side fix.
enc_extend <- list(
  Bounce           = 1,
  Quick_Cross      = 1,
  Back_n_forth     = 2,
  Trace            = 1,
  Trapped          = 1,
  Average_Movement = 1
)

spatial_half <- list(
  Bounce           = NULL,
  Quick_Cross      = NULL,
  Back_n_forth     = NULL,
  Trace            = NULL,
  Trapped          = NULL,
  Average_Movement = NULL
)

target_types <- c("Average_Movement", "Quick_Cross", "Bounce",
                  "Back_n_forth", "Trace", "Trapped")

type_labels <- c(
  Average_Movement = "Average movement",
  Quick_Cross      = "Quick cross",
  Bounce           = "Bounce",
  Back_n_forth     = "Back-and-forth",
  Trace            = "Trace",
  Trapped          = "Trapped"
)

select_criteria <- list(
  Bounce           = quote(dur_hr >= 0),
  Quick_Cross      = quote(dur_hr >= 0),
  Back_n_forth     = quote(dur_hr >= 0),
  Trace            = quote(dur_hr >= 0),
  Trapped          = quote(dur_hr >= 0),
  Average_Movement = quote(dur_hr >= 0)
)

BaBA_dur <- BaBA_all %>%
  mutate(dur_hr = as.numeric(difftime(end_time, start_time, units = "hours")))

has_clean_buffer <- function(enc, buf_hrs = 2) {
  animal_encs <- BaBA_dur %>%
    filter(as.character(AnimalID) == as.character(enc$AnimalID),
           burstID != enc$burstID)
  t0 <- enc$start_time - buf_hrs * 3600
  t1 <- enc$end_time + buf_hrs * 3600
  !any(animal_encs$start_time < t1 & animal_encs$end_time > t0)
}

# skip: number of valid candidates to pass over before selecting
auto_select <- function(type, skip = 0) {
  crit <- select_criteria[[type]]
  candidates <- BaBA_dur %>%
    filter(eventTYPE == type) %>%
    filter(!!crit)
  if (nrow(candidates) == 0) candidates <- BaBA_dur %>% filter(eventTYPE == type)
  valid <- list()
  for (i in seq_len(nrow(candidates))) {
    enc <- candidates[i, ]
    fixes <- get_encounter_fixes(enc)
    if (!is.null(fixes) && nrow(fixes) >= 5) valid <- c(valid, list(enc))
    if (length(valid) > skip) return(valid[[skip + 1]])
  }
  if (length(valid) > 0) return(valid[[min(skip + 1, length(valid))]])
  candidates[1, ]
}

skip_n <- list(
  Bounce           = 14,
  Quick_Cross      = 16,
  Back_n_forth     = 7,
  Trace            = 0, # good
  Trapped          = 2, # maybe
  Average_Movement = 0  # good
)

examples <- lapply(target_types, function(type) {
  if (!is.null(manual[[type]])) return(manual[[type]])
  auto_select(type, skip = skip_n[[type]])
})
names(examples) <- target_types

encounter_color  <- "#7B6FAD"
context_color    <- "#999999"
unaltered_color  <- "#4A8C7E"
altered_color    <- "#C49A3C"

group_color <- c(
  Average_Movement = "#4A8C7E",
  Quick_Cross      = "#4A8C7E",
  Bounce           = "#C49A3C",
  Back_n_forth     = "#C49A3C",
  Trace            = "#C49A3C",
  Trapped          = "#C49A3C"
)

all_fixes <- lapply(target_types, function(type) {
  get_encounter_fixes(examples[[type]], context_n = context_n[[type]],
                      enc_extend = enc_extend[[type]])
})
names(all_fixes) <- target_types

make_panel <- function(type) {
  enc  <- examples[[type]]
  fixes <- all_fixes[[type]]
  if (is.null(fixes) || nrow(fixes) < 3) {
    return(ggplot() + labs(title = type_labels[type]) + theme_void())
  }

  cx   <- mean(range(fixes$UTME))
  cy   <- mean(range(fixes$UTMN))
  half <- if (!is.null(spatial_half[[type]])) {
    spatial_half[[type]]
  } else {
    max(max(fixes$UTME) - cx, cx - min(fixes$UTME),
        max(fixes$UTMN) - cy, cy - min(fixes$UTMN)) + 50
  }
  xlim <- c(cx - half, cx + half)
  ylim <- c(cy - half, cy + half)
  clip_bbox <- st_bbox(c(xmin = xlim[1] - 500, xmax = xlim[2] + 500,
                         ymin = ylim[1] - 500, ymax = ylim[2] + 500),
                       crs = st_crs(32616))
  road <- tryCatch(st_crop(get_road(enc$Study_area), clip_bbox), error = function(e) NULL)

  p <- ggplot()

  if (!is.null(road))
    p <- p + geom_sf(data = road, color = "grey40", linewidth = 1.2)

  p <- p +
    geom_path(data = fixes, aes(x = UTME, y = UTMN),
              color = "grey70", linewidth = 0.4, linetype = "dashed") +
    geom_point(data = fixes %>% filter(phase != "encounter"),
               aes(x = UTME, y = UTMN),
               color = context_color, size = 1.2, alpha = 0.7) +
    geom_point(data = fixes %>% filter(phase == "encounter"),
               aes(x = UTME, y = UTMN),
               color = encounter_color, size = 2, alpha = 0.75) +
    coord_sf(crs = 32616, xlim = xlim, ylim = ylim, expand = FALSE) +
    labs(title = type_labels[type]) +
    theme_void() +
    theme(
      plot.title      = element_text(size = 10, hjust = 0.5, margin = margin(b = 4)),
      plot.background = element_rect(fill = "white", color = group_color[type], linewidth = 1.2),
      plot.margin     = margin(6, 6, 6, 6)
    )
  p
}

panels <- lapply(target_types, make_panel)
names(panels) <- target_types

# ── Grouped layout: Unaltered (left col) | Altered (right 2 cols) ────────────

fig_base <- (panels$Average_Movement | panels$Bounce    | panels$Back_n_forth) /
            (panels$Quick_Cross      | panels$Trace     | panels$Trapped)

fig_real <- ggdraw() +
  draw_plot(fig_base, x = 0, y = 0, width = 1, height = 0.93) +
  draw_label("Unaltered", x = 0.27, y = 0.95, fontface = "bold", size = 10,
             color = unaltered_color, hjust = 0.5, vjust = 0.5) +
  draw_label("Altered",   x = 0.61, y = 0.95, fontface = "bold", size = 10,
             color = altered_color,   hjust = 0.5, vjust = 0.5)

fig_real

ggsave(file.path(fig_dir, "figS_trajectory_examples.png"),
       fig_real, width = 9, height = 6, dpi = 300, bg = "white")
cat("Saved: figS_trajectory_examples.png\n")

# ── Candidate browser — run this block to compare options for a type ──────────
# Change browse_type and browse_n to inspect candidates before committing to skip_n
# Guarded so it only runs interactively (not when the script is sourced).
if (interactive()) {

browse_type <- "Back-and-forth"
browse_n    <- 9   # how many candidates to show

browse_candidates <- BaBA_dur %>% filter(eventTYPE == browse_type)
browse_valid <- list()
for (i in seq_len(nrow(browse_candidates))) {
  enc <- browse_candidates[i, ]
  fixes <- get_encounter_fixes(enc, context_n = 2)
  if (!is.null(fixes) && nrow(fixes) >= 5) browse_valid <- c(browse_valid, list(list(enc = enc, fixes = fixes)))
  if (length(browse_valid) >= browse_n) break
}

browse_panels <- lapply(seq_along(browse_valid), function(k) {
  enc   <- browse_valid[[k]]$enc
  fixes <- browse_valid[[k]]$fixes
  cx   <- mean(range(fixes$UTME))
  cy   <- mean(range(fixes$UTMN))
  half <- max(max(fixes$UTME) - cx, cx - min(fixes$UTME),
              max(fixes$UTMN) - cy, cy - min(fixes$UTMN)) + 50
  xlim <- c(cx - half, cx + half)
  ylim <- c(cy - half, cy + half)
  clip_bbox <- st_bbox(c(xmin = xlim[1] - 500, xmax = xlim[2] + 500,
                         ymin = ylim[1] - 500, ymax = ylim[2] + 500),
                       crs = st_crs(32616))
  road <- tryCatch(st_crop(get_road(enc$Study_area), clip_bbox), error = function(e) NULL)
  p <- ggplot()
  if (!is.null(road))
    p <- p + geom_sf(data = road, color = "grey40", linewidth = 1.2)
  p +
    geom_path(data = fixes, aes(x = UTME, y = UTMN),
              color = "grey70", linewidth = 0.4, linetype = "dashed") +
    geom_point(data = fixes %>% filter(phase != "encounter"),
               aes(x = UTME, y = UTMN), color = context_color, size = 1.2, alpha = 0.7) +
    geom_point(data = fixes %>% filter(phase == "encounter"),
               aes(x = UTME, y = UTMN), color = encounter_color, size = 2) +
    coord_sf(crs = 32616, xlim = xlim, ylim = ylim, expand = FALSE) +
    labs(title = paste0("skip_n = ", k - 1, "  |  ", enc$AnimalID)) +
    theme_void() +
    theme(
      plot.title      = element_text(size = 7, hjust = 0.5, margin = margin(b = 3)),
      plot.background = element_rect(fill = "white", color = "grey60", linewidth = 0.8),
      plot.margin     = margin(4, 4, 4, 4)
    )
})

print(wrap_plots(browse_panels, ncol = 4))

}

