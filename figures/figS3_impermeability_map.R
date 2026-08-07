# Supplemental Figure — Road permeability map
# Ranked 2 km road segments (>=10 encounters) colored by BaBA impermeability
# index, drawn over the full road network in grey for context. Site frame
# (extent, scale bar, corner label, border) is shared with Figure 1 so the
# panels are directly comparable and the scale bars are consistent.
# AEN 06-02-26 (reworked onto shared base 07-10-26)

library(patchwork)
library(cowplot)
library(here)

source(here("2_Scripts/revisions/figures/_site_panel_base.R"))

rev_dir <- here("1_Data/revisions")

# Recompute the index from the stored per-segment counts so the encounter
# threshold is set at the figure level. Two variants are produced (mode arg):
#   "withinsite"  — full Xu index (fraction x unique_ind) rescaled PER SITE to 0-1.
#                   Colors show each site's relative worst roads. NOT comparable
#                   across panels (each site's worst segment is forced to 1).
#   "betweensite" — raw non-crossing fraction, bounded [0,1] with no rescaling and
#                   no unique_ind multiplier. The same color means the same thing
#                   in every panel, so panels ARE comparable across sites.
raw_index <- function(d) ((d$Bounce + d$Back_n_forth + d$Trace + d$Trapped) / d$total_enc) * d$unique_ind
raw_frac  <- function(d) (d$Bounce + d$Back_n_forth + d$Trace + d$Trapped) / d$total_enc
range01   <- function(x) (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))

compute_rank_list <- function(min_enc, mode) {
  lapply(c(LSV = "LSV", TON = "TON", SIU = "SIU"), function(s) {
    r <- readRDS(file.path(rev_dir, sprintf("Rank_%s_AEN.rds", s)))
    val <- if (mode == "withinsite") raw_index(r) else raw_frac(r)
    val[is.na(r$total_enc) | r$total_enc < min_enc] <- NA
    r$index <- if (mode == "withinsite") range01(val) else val
    r[!is.na(r$index), ]
  })
}

# Legend title differs by mode; the color mapping (magma, 0-1) is shared so the
# two variants render identically apart from what the values mean.
scale_name_for <- function(mode) {
  if (mode == "withinsite") "Impermeability index\n(within-site, relative)"
  else "Impermeability\nindex"
}
perm_scale_for <- function(mode) scale_color_viridis_c(
  option = "viridis", direction = -1, limits = c(0, 1),
  breaks = c(0, 0.5, 1), labels = c("0 (permeable)", "0.5", "1 (impermeable)"),
  name = scale_name_for(mode))

# ── Per-site panel windows fit to each site's ranked segments ─────────────────
# The three sites operate at different spatial scales (LSV segments are dispersed,
# TON and SIU compact), so each panel is fit to its own segments plus a margin
# (with a floor so a compact site is not over-zoomed). The scale bar is then a
# round distance sized to each panel, so bars render at a consistent visual length
# across panels while showing the correct (different) distance.

panel_bbox <- function(site) {
  # LSV has a few far-flung segments that would stretch the panel well beyond the
  # study-area map. Cap it to the Figure 1 LSV extent so it shows the same area.
  if (site == "LSV") return(site_bbox[["LSV"]])
  # TON/SIU: frame to where the bulk of the segments are, using a trimmed
  # coordinate range so a lone far segment does not stretch the window.
  xy   <- st_coordinates(rank_list[[site]])
  qx   <- quantile(xy[, "X"], c(0.02, 0.98), names = FALSE)
  qy   <- quantile(xy[, "Y"], c(0.02, 0.98), names = FALSE)
  cx   <- (qx[1] + qx[2]) / 2
  cy   <- (qy[1] + qy[2]) / 2
  half <- max(max(qx[2] - qx[1], qy[2] - qy[1]) / 2 * 1.20, 2500)
  st_bbox(c(xmin = cx - half, xmax = cx + half, ymin = cy - half, ymax = cy + half),
    crs = st_crs(32616))
}

# Round-number scale bar ~20% of panel width
nice_km <- function(width_m) {
  choices <- c(0.5, 1, 2, 5, 10, 20)
  choices[which.min(abs(choices - width_m / 1000 * 0.2))]
}

# ── Panels — grey context roads + colored index segments ──────────────────────

make_perm_panel <- function(site) {
  bbox    <- panel_bbox(site)
  clip    <- st_as_sfc(bbox)
  roads_c <- tryCatch(st_crop(site_roads_osm[[site]], clip), error = function(e) site_roads_osm[[site]])
  rank_c  <- tryCatch(st_crop(rank_list[[site]], clip), error = function(e) rank_list[[site]])
  xr <- bbox["xmax"] - bbox["xmin"]; yr <- bbox["ymax"] - bbox["ymin"]
  ggplot() +
    geom_sf(data = roads_c, color = "grey80", linewidth = 0.25) +
    geom_sf(data = rank_c, aes(color = index), linewidth = 0.9) +
    perm_scale +
    add_scalebar(bbox, bar_km = nice_km(xr),
      side = if (site == "LSV") "right" else "left") +
    annotate("label", x = bbox["xmin"] + xr * 0.03, y = bbox["ymax"] - yr * 0.03,
      label = site, hjust = 0, vjust = 1, size = 4.2, fontface = "bold",
      label.size = 0, fill = "white", alpha = 0.85) +
    coord_sf(crs = 32616, xlim = c(bbox["xmin"], bbox["xmax"]),
      ylim = c(bbox["ymin"], bbox["ymax"]), expand = FALSE) +
    theme_void() +
    theme(
      plot.background = element_rect(fill = "white", color = NA),
      panel.border = element_rect(color = site_colors[site], fill = NA, linewidth = 3.5),
      legend.position = "none",
      plot.margin = margin(3, 3, 3, 3)
    )
}

# ── Legend (label depends on mode; color mapping shared) ──────────────────────

legend_for <- function(mode) {
  legend_src <- ggplot(
    data.frame(x = 1, y = seq(0, 1, length.out = 100), index = seq(0, 1, length.out = 100))
  ) +
    geom_tile(aes(x = x, y = y, fill = index)) +
    scale_fill_viridis_c(
      option = "viridis", direction = -1, limits = c(0, 1),
      breaks = c(0, 0.5, 1), labels = c("0\n(permeable)", "0.5", "1\n(impermeable)"),
      name = scale_name_for(mode)) +
    theme_void() +
    theme(
      legend.position   = "right",
      legend.title      = element_text(size = 10, face = "bold", margin = margin(b = 12)),
      legend.text       = element_text(size = 9),
      legend.key.height = unit(1.5, "cm"),
      legend.key.width  = unit(0.4, "cm")
    )
  ggdraw(get_legend(legend_src))
}

# ── One figure, both scalings stacked (>=10 encounters) ───────────────────────
# Row A = within-site scaling, Row B = between-site scaling. make_perm_panel()
# reads the globals `rank_list` and `perm_scale`, so both are set before each row.
# Kept for reference in map_options/; the shipped supplement figure is built below.

build_row <- function(mode, min_enc) {
  perm_scale <<- perm_scale_for(mode)
  rank_list  <<- compute_rank_list(min_enc, mode)
  plot_grid(
    make_perm_panel("LSV"), make_perm_panel("TON"), make_perm_panel("SIU"), legend_for(mode),
    nrow = 1, rel_widths = c(1, 1, 1, 0.35))
}

build_options <- FALSE  # set TRUE to regenerate the two-variant comparison sheet
if (build_options) {
  for (min_enc in c(2, 10)) {
    fig_perm <- plot_grid(
      build_row("withinsite", min_enc), build_row("betweensite", min_enc), ncol = 1,
      labels = c("A  Within-site scaling", "B  Between-site scaling"),
      label_size = 12, label_fontface = "bold", hjust = 0, label_x = 0.01)
    fname <- sprintf("3_Figures/round2/map_options/figS_road_permeability_min%d.png", min_enc)
    ggsave(here(fname), fig_perm, width = 11, height = 8, dpi = 300, bg = "white")
    cat(sprintf("Saved: %s\n", basename(fname)))
  }
}

# ── Shipped supplement figure — between-site scaling only (>=10 encounters) ────
# Cross-site comparable raw non-crossing fraction (no per-site rescaling, no
# unique_ind multiplier), matching main-text Fig. 2D and Fig. 3B. Same color means
# the same thing in every panel: TON reads most impermeable, SIU most permeable.
fig_perm <- build_row("betweensite", min_enc = 10)
ggsave(here("3_Figures/round2/supplemental/figS_road_permeability.png"),
  fig_perm, width = 13.5, height = 4, dpi = 300, bg = "white")
cat("Saved: figS_road_permeability.png (between-site, supplemental/)\n")
