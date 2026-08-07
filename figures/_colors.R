# BaBA Project — Centralized Color Palettes
# Source into any figure script: source(here("2_Scripts/revisions/figures/_colors.R"))
# Run the preview section at the bottom to visualize palettes.
# AEN 05-25-26

library(ggplot2)
library(patchwork)

# ── Palettes ──────────────────────────────────────────────────────────────────

# Study sites — used in Fig. 1 inset map, Fig. 2, Fig. 3
site_colors <- c(
  "LSV" = "#1982c4",
  "TON" = "#8ac926",  
  "SIU" = "#6a4c93"
)

# Road categories (OSM L/M/S) — road-class supplement figure.
# R1-FigS1 asked for better category contrast and visible small roads. Large is
# black, medium a warm vermillion, small a cool blue — a colorblind-safe triad
# where medium and small no longer read as two similar warm reds. Borders in this
# figure are grey, so the blue does not clash with the LSV site color.
road_cat_colors <- c(Large = "#000000", Medium = "#e6550d", Small = "#0072B2")

# Logistic model significance — Fig. 4 forest plot
sig_colors <- c(
  "Significant"     = "#440154",
  "Non-significant" = "grey55"
)
sig_colors_tf <- c("TRUE" = "#440154", "FALSE" = "grey55")

# NLCD land cover — Fig. 1 raster background
nlcd_colors <- c(
  "11" = "#476BA0",
  "21" = "#DDC9C9",
  "22" = "#D89382",
  "41" = "#68AA63",
  "52" = "#CCBA7C",
  "71" = "#E2E2C1",
  "81" = "#EDE050",
  "90" = "#BAD8EA"
)
# Muted, near-equal-lightness earth palette for the track-overview base: keeps
# all land cover classes but desaturates them so shaded tracks read on top
# (cf. Xu et al. 2021, whose base was near-monochrome).
nlcd_colors_muted <- c(
  "11" = "#bcd7ea",  # open water   — light blue
  "21" = "#e7d9d2",  # developed    — light warm grey-pink
  "22" = "#d7a493",  # developed    — muted salmon (reads for the SIU town)
  "41" = "#a9c78f",  # forest       — sage green
  "52" = "#cdbf8a",  # scrub        — khaki
  "71" = "#ebe6c4",  # grassland    — pale cream
  "81" = "#e4d68d",  # crops        — muted wheat/gold (distinct from grassland, not bright yellow)
  "90" = "#bcd7ea"   # woody wetlands — grouped with open water (same blue)
)

nlcd_labels <- c(
  "11" = "Open water / wetlands",
  "21" = "Developed (open)",
  "22" = "Developed (low)",
  "41" = "Deciduous forest",
  "52" = "Shrub/scrub",
  "71" = "Grassland",
  "81" = "Hay/pasture",
  "90" = "Woody wetlands"
)

# Land cover predicted probability panels — Fig. 4B
# Pulled from NLCD class colors above (41 = forest, 22 = development, 81 = agriculture)
landcover_colors <- c(
  "Forest" = "#68AA63",
  "Development"  = "#D89382",
  "Agriculture"  = "#EDE050"  # FLAG: color not finalized — revisit
)

# BaBA encounter types (viridis) — Fig. 3A stacked bar
# Order: least to most altered behavior
encounter_colors <- setNames(
  viridisLite::viridis(6),
  c("Quick cross", "Average movement", "Bounce", "Back-and-forth", "Trace", "Trapped")
)

# Video behavior stacked bar — Fig. 5 bar chart
behavior_colors <- setNames(
  viridisLite::viridis(7),
  c("Forage", "Interaction", "Resting", "Scrape", "Standing", "Walking", "Running")
)

# ── Preview helpers ───────────────────────────────────────────────────────────

swatch <- function(pal, title) {
  df <- data.frame(
    name = factor(names(pal), levels = names(pal)),
    color = unname(pal)
  )
  ggplot(df, aes(x = name, y = 1, fill = name)) +
    geom_tile(color = "white", linewidth = 1.5) +
    geom_text(aes(label = color), y = 0.5, size = 2.8, color = "white",
      fontface = "bold", vjust = 0) +
    geom_text(aes(label = name), y = 1.5, size = 3, color = "black", vjust = 1) +
    scale_fill_manual(values = setNames(pal, names(pal))) +
    scale_y_continuous(limits = c(0, 2)) +
    labs(title = title) +
    theme_void() +
    theme(
      legend.position = "none",
      plot.title = element_text(size = 10, face = "bold", hjust = 0,
        margin = margin(b = 4)),
      plot.margin = margin(8, 8, 8, 8)
    )
}

swatch_gradient <- function(low, high, title) {
  df <- data.frame(x = seq(0, 1, length.out = 100), y = 1)
  ggplot(df, aes(x = x, y = y, fill = x)) +
    geom_tile() +
    scale_fill_gradient(low = low, high = high) +
    annotate("text", x = 0.02, y = 1, label = low, hjust = 0,
      size = 2.8, color = "grey30") +
    annotate("text", x = 0.98, y = 1, label = high, hjust = 1,
      size = 2.8, color = "white", fontface = "bold") +
    labs(title = title) +
    theme_void() +
    theme(
      legend.position = "none",
      plot.title = element_text(size = 10, face = "bold", hjust = 0,
        margin = margin(b = 4)),
      plot.margin = margin(8, 8, 8, 8)
    )
}

# ── Per-figure previews ───────────────────────────────────────────────────────

preview_sites     <- swatch(site_colors,    "Site colors — Fig. 1, Fig. 2, Fig. 3")
preview_road_cats <- swatch(road_cat_colors, "Road categories — Fig. S (road class maps)")
preview_sig       <- swatch(sig_colors,      "Significance — Fig. 4 (forest plot)")
nlcd_legend_classes <- c("11", "21", "22", "41", "81")
preview_nlcd <- swatch(
  setNames(nlcd_colors[nlcd_legend_classes], nlcd_labels[nlcd_legend_classes]),
  "NLCD land cover — Fig. 1 legend classes"
)
preview_encounters <- swatch(encounter_colors, "Encounter types (viridis) — Fig. 3A stacked bar")
preview_behaviors  <- swatch(behavior_colors,  "Behavior colors — Fig. 5 bar chart")

# View individual palette
preview_sites
preview_road_cats
preview_sig
preview_nlcd
preview_encounters
preview_behaviors

# ── Overview — all palettes in one figure ─────────────────────────────────────

preview_all <- (preview_sites | preview_road_cats | preview_sig) /
  (preview_nlcd | preview_encounters | preview_behaviors) +
  plot_annotation(
    title = "BaBA manuscript — color scheme overview",
    theme = theme(plot.title = element_text(size = 12, face = "bold"))
  )

preview_all

