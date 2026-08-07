# Supplemental Figure — Site-level movement track overview
# 10 individuals per site (LSV, TON: males meeting >=300 tracked days + females
# sampled from the >=300-day pool; SIU: all individuals), excluding any with an
# implausible speed jump OR a track extending outside the site frame (no
# clipping — a non-fitting individual is replaced rather than cut off).
# Tracks overlay the shared land-cover site panels (site_panel_base.R), so
# extent, scale bars, and labels match Fig. 1 and the road-category figure.
# AEN 07-08-26

library(patchwork)
library(cowplot)
library(here)

set.seed(707)

source(here("2_Scripts/revisions/figures/_site_panel_base.R"))

rev_dir <- here("1_Data/revisions")

# ── Select individuals per site ───────────────────────────────────────────────

ind <- read.csv(file.path(rev_dir, "individual_summary_AEN.csv")) %>%
  rename(AnimalID = Animal.ID, Duration = Duration..days.) %>%
  mutate(Site = case_when(
    Site == "SIUC"        ~ "SIU",
    Site == "Shelbyville" ~ "LSV",
    Site == "Touch of Nature" ~ "TON"
  ))

# Implausible single-step speed = likely GPS error (uses speed, so a fix after
# a long recording gap isn't falsely flagged).
has_extreme_jump <- function(animal_id, max_speed_kmh = 15) {
  f <- file.path(rev_dir, paste0(animal_id, "_track2.csv"))
  if (!file.exists(f)) return(TRUE)
  track <- read.csv(f) %>% mutate(t_ = as.POSIXct(t_, tz = "UTC")) %>% arrange(t_)
  if (nrow(track) < 2) return(TRUE)
  dist_m <- sqrt(diff(track$UTME)^2 + diff(track$UTMN)^2)
  hrs <- as.numeric(diff(track$t_), units = "hours")
  speed_kmh <- (dist_m / 1000) / hrs
  speed_kmh <- speed_kmh[is.finite(speed_kmh)]
  length(speed_kmh) == 0 || max(speed_kmh) > max_speed_kmh
}

# Whole track must sit inside the site frame — no clipped tracks at the edge.
track_fits_bbox <- function(animal_id, bbox) {
  f <- file.path(rev_dir, paste0(animal_id, "_track2.csv"))
  if (!file.exists(f)) return(FALSE)
  track <- read.csv(f)
  all(track$UTME >= bbox[["xmin"]], track$UTME <= bbox[["xmax"]],
      track$UTMN >= bbox[["ymin"]], track$UTMN <= bbox[["ymax"]])
}

select_site <- function(site_name, n = 10, min_days = 300) {
  pool <- ind %>% filter(Site == site_name)
  bbox <- site_bbox[[site_name]]
  is_usable <- function(id) !has_extreme_jump(id) && track_fits_bbox(id, bbox)

  if (nrow(pool) <= n) {
    clean <- pool$AnimalID[sapply(pool$AnimalID, is_usable)]
    if (length(clean) < nrow(pool)) {
      warning(site_name, ": excluded ", nrow(pool) - length(clean),
        " of ", nrow(pool), " individuals (speed jump or track outside site box)")
    }
    return(clean)
  }

  qualifying <- pool %>% filter(Duration >= min_days)
  males   <- qualifying %>% filter(Sex == "Male")   %>% slice_sample(prop = 1)
  females <- qualifying %>% filter(Sex == "Female") %>% slice_sample(prop = 1)
  clean_males   <- males$AnimalID[sapply(males$AnimalID, is_usable)]
  clean_females <- females$AnimalID[sapply(females$AnimalID, is_usable)]
  n_female <- n - length(clean_males)
  c(clean_males, head(clean_females, n_female))
}

selected <- list(
  LSV = select_site("LSV"),
  TON = select_site("TON"),
  SIU = select_site("SIU")
)

cat("Selected per site:\n")
print(lapply(selected, length))

# ── Load tracks ────────────────────────────────────────────────────────────────

get_track <- function(animal_id) {
  f <- file.path(rev_dir, paste0(animal_id, "_track2.csv"))
  if (!file.exists(f)) { warning("No track2 for: ", animal_id); return(NULL) }
  read.csv(f) %>% select(animal_id, UTME, UTMN)
}

site_tracks <- lapply(names(selected), function(s) {
  bind_rows(lapply(selected[[s]], get_track)) %>% mutate(Site = s)
})
names(site_tracks) <- names(selected)

# ── Panels — shared land-cover base + track overlay + shared frame ───────────
# Individuals get distinct shades of the site's hue (light to dark), read through
# transparency where they overlap (cf. Xu et al. 2021 track figure).

# GPS locations as semi-transparent points (cf. Xu et al. 2021), piling up to
# show where individuals concentrate. Each individual gets a light-to-dark shade
# of its site's hue; the border keeps the exact site color.
site_point_ramp <- list(
  LSV = c("#8fc1e6", "#0c3a5a"),  # light blue  -> dark navy
  TON = c("#bfe57f", "#48690f"),  # light lime  -> dark green
  SIU = c("#b39ad3", "#3a2957")   # light lilac -> dark purple
)
point_shades <- function(site_name, ids) {
  setNames(colorRampPalette(site_point_ramp[[site_name]])(length(ids)), ids)
}
# Panel: roads by size class on white + shaded site-color track points. No land
# cover (that lives in main-text Fig. 1). Points use the color scale, roads use
# literal colors.
make_overview_panel <- function(site_name) {
  d   <- site_tracks[[site_name]]
  ids <- unique(d$animal_id)
  ggplot() +
    roadclass_safe_layers(site_name) +
    geom_point(data = d,
      aes(x = UTME, y = UTMN, color = animal_id),
      size = 0.4, alpha = 0.45, stroke = 0) +
    scale_color_manual(values = point_shades(site_name, ids), guide = "none") +
    site_frame(site_name, border_color = site_colors[site_name]) +
    theme(panel.background = element_rect(fill = "white", color = NA))
}

p_lsv <- make_overview_panel("LSV")
p_ton <- make_overview_panel("TON")
p_siu <- make_overview_panel("SIU")

# ── Legend (road class only) ──────────────────────────────────────────────────

rc_lvls <- c("Large", "Medium", "Small")
rc_legend <- get_legend(
  ggplot(data.frame(x = 1:3, cls = factor(rc_lvls, levels = rc_lvls))) +
    geom_segment(aes(x = 0, xend = 1, y = cls, yend = cls, color = cls), linewidth = 1) +
    scale_color_manual(values = road_cat_colors_safe, name = "Road category") +
    theme_void() +
    theme(legend.title = element_text(size = 10, face = "bold"),
      legend.text = element_text(size = 9), legend.key.size = unit(0.45, "cm"))
)

fig_overview <- plot_grid(p_lsv, p_ton, p_siu, rc_legend,
  nrow = 1, rel_widths = c(1, 1, 1, 0.3))

fig_overview

ggsave(here("3_Figures/round2/supplemental/figS_track_overview.png"),
  fig_overview, width = 13.5, height = 4, dpi = 300, bg = "white")
