# BaBA Study site figure (Fig. 1) — Option 2: land cover + deer tracks
# Three site panels (Shelbyville, Touch of Nature, SIUC) + Illinois overview.
# Land cover, grey roads by width, and a 5-individual track subset per site.
# Site panels are built from site_panel_base.R so extent, scale bars, and
# labels stay locked to the track-overview and road-category figures.
# AEN 05-19-26 (refactored onto shared base 07-08-26)

library(patchwork)
library(cowplot)
library(tigris)
library(here)

source(here("2_Scripts/revisions/figures/_site_panel_base.R"))

options(tigris_use_cache = TRUE)

fig_dir  <- here("3_Figures/round2/main")
supp_dir <- here("3_Figures/round2/supplemental")

# ── Deer tracks for the main figure (Option 2 = land cover + tracks) ──────────
# Same 5-individual, one-color-each selection as the Fig 1 option set.
source(here("2_Scripts/revisions/figures/figS1_track_overview.R"))  # provides site_tracks
oi <- c("#E69F00", "#0072B2", "#D55E00", "#CC79A7", "#56B4E9")
subset_tracks <- function(site, n = 5) {
  df <- site_tracks[[site]]
  ids <- head(unique(df$animal_id), n)
  df %>% filter(animal_id %in% ids) %>% mutate(col = oi[match(animal_id, ids)])
}
tracks_layer <- function(site) list(
  geom_path(data = subset_tracks(site), aes(x = UTME, y = UTMN, group = animal_id, color = col),
    linewidth = 0.3, alpha = 0.75),
  scale_color_identity())

# ── Illinois state / neighbors for the overview inset ────────────────────────

us_states <- states(cb = TRUE, year = 2022, progress_bar = FALSE) %>%
  filter(!STUSPS %in% c("AK", "HI", "PR", "VI", "GU", "MP", "AS")) %>%
  st_transform(32616)
il_state <- us_states %>% filter(STUSPS == "IL")

# ── Site panels — land cover + grey roads + shared frame ─────────────────────

p_lsv <- ggplot() + muted_landcover_layers("LSV") + site_frame("LSV", border_color = site_colors["LSV"])
p_ton <- ggplot() + muted_landcover_layers("TON") + site_frame("TON", border_color = site_colors["TON"])
p_siu <- ggplot() + muted_landcover_layers("SIU") + site_frame("SIU", border_color = site_colors["SIU"])

# ── Illinois overview ─────────────────────────────────────────────────────────

site_centroids <- captures_sf %>%
  group_by(Study_area) %>%
  summarise(geometry = st_centroid(st_union(geometry)), .groups = "drop") %>%
  mutate(
    label = recode(Study_area,
      "Touch of Nature" = "TON", "SIUC" = "SIU", "Shelbyville" = "LSV"),
    label_x = st_coordinates(geometry)[, 1] + case_when(
      Study_area == "Shelbyville"     ~  30000,
      Study_area == "Touch of Nature" ~ -30000,
      Study_area == "SIUC"            ~  30000),
    label_y = st_coordinates(geometry)[, 2] + case_when(
      Study_area == "Shelbyville"     ~      0,
      Study_area == "Touch of Nature" ~ -20000,
      Study_area == "SIUC"            ~  20000),
    label_hjust = case_when(
      Study_area == "Touch of Nature" ~ 1,
      TRUE                            ~ 0
    )
  )

# Regional overview: equal x/y extents so the inset renders as a square
il_bb <- st_bbox(il_state)
y_pad  <- 30000
x_pad  <- ((il_bb["ymax"] - il_bb["ymin"] + 2 * y_pad) - (il_bb["xmax"] - il_bb["xmin"])) / 2
region_xlim <- c(il_bb["xmin"] - x_pad, il_bb["xmax"] + x_pad)
region_ylim <- c(il_bb["ymin"] - y_pad, il_bb["ymax"] + y_pad)

neighbor_labels <- us_states %>%
  filter(STUSPS %in% c("IN", "MO", "KY", "IA", "WI", "MI")) %>%
  st_centroid()

p_il <- ggplot() +
  geom_sf(data = us_states, fill = "grey93", color = "grey70", linewidth = 0.2) +
  geom_sf(data = il_state, fill = "grey78", color = "grey20", linewidth = 0.6) +
  geom_sf_text(data = neighbor_labels, aes(label = STUSPS),
    size = 2.4, color = "grey40") +
  annotate("text", x = (il_bb["xmin"] + il_bb["xmax"]) / 2 + 60000,
    y = (il_bb["ymin"] + il_bb["ymax"]) / 2 + 100000,
    label = "IL", size = 2.8, fontface = "bold", color = "grey20") +
  geom_sf(data = site_centroids, aes(color = Study_area), size = 2.5, shape = 16) +
  geom_label(data = site_centroids, aes(x = label_x, y = label_y, label = label, hjust = label_hjust),
    size = 3.2, fontface = "bold", color = "black",
    fill = "white", alpha = 0.8, label.size = 0, label.padding = unit(0.1, "lines")) +
  scale_color_manual(values = site_colors_area, guide = "none") +
  coord_sf(crs = 32616, xlim = region_xlim, ylim = region_ylim, expand = FALSE, clip = "on") +
  labs(title = NULL) +
  theme_void() +
  theme(plot.background = element_rect(fill = "white", color = NA),
        panel.border = element_rect(color = "grey30", fill = NA, linewidth = 3.5),
        plot.margin = margin(3, 3, 3, 3), legend.position = "none")

# ── Legend panel — combined land cover + road ───────────────────────────────

legend_src <- ggplot() +
  geom_tile(
    data = data.frame(x = seq_along(legend_classes), y = 1, class = legend_classes),
    aes(x = x, y = y, fill = class)
  ) +
  geom_segment(
    data = data.frame(x = 1, xend = 2, y = 1, yend = 1,
      lab = factor("Roads", levels = "Roads")),
    aes(x = x, xend = xend, y = y, yend = yend, color = lab),
    linewidth = 0.6
  ) +
  scale_fill_manual(values = nlcd_colors_muted[legend_classes],
    labels = nlcd_labels[legend_classes],
    name = "Land cover") +
  scale_color_manual(values = c("Roads" = "grey35"), name = NULL) +
  guides(fill = guide_legend(order = 1), color = guide_legend(order = 2)) +
  theme_void() +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 9),
    legend.key.size = unit(0.4, "cm"),
    legend.box = "vertical",
    legend.spacing.y = unit(0.2, "cm"),
    legend.box.background = element_rect(color = "grey30", fill = NA, linewidth = 1),
    legend.box.margin = margin(8, 8, 8, 8)
  )

# Legend as a bordered box (border hugs the legend via legend.box.background).
p_legend <- ggdraw(get_legend(legend_src))

# ── Assemble ──────────────────────────────────────────────────────────────────

# 2x2 grid of the three site panels + a smaller IL inset (scaled within its
# cell), with the bordered legend box as a column on the right. Equal gutters.
grid_2x2 <- plot_grid(p_lsv, p_ton, p_siu,
  plot_grid(p_il, scale = 0.8), ncol = 2)
fig1 <- plot_grid(grid_2x2, p_legend, nrow = 1, rel_widths = c(1, 0.3))

fig1

ggsave(file.path(fig_dir, "fig_study_site.png"),
  fig1, width = 9.5, height = 8, dpi = 300, bg = "white")
cat("Saved: fig_study_site.png\n")

# ── Supplemental Figure — roads by OSM category (L/M/S), white background ─────
# Same site frame as Fig. 1; roads colored by size on white (no land cover).

road_legend_src <- ggplot(
  data = data.frame(x = 1:3, y = 1, osm_class = factor(c("Large", "Medium", "Small"),
    levels = c("Large", "Medium", "Small")))
) +
  geom_segment(aes(x = x - 0.4, xend = x + 0.4, y = y, yend = y, color = osm_class),
    linewidth = 2) +
  scale_color_manual(values = osm_colors, name = "Road category") +
  theme_void() +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 9),
    legend.key.size = unit(0.5, "cm")
  )

sp_legend <- ggdraw(get_legend(road_legend_src))

# Neutral (grey) borders here — road category already uses a color encoding,
# so site-colored borders would add a confusing second, similar-hued channel.
rc_lsv <- ggplot() + osm_road_layers("LSV") + site_frame("LSV", border_color = "grey30")
rc_ton <- ggplot() + osm_road_layers("TON") + site_frame("TON", border_color = "grey30")
rc_siu <- ggplot() + osm_road_layers("SIU") + site_frame("SIU", border_color = "grey30")

figS1_road_class <- plot_grid(rc_lsv, rc_ton, rc_siu, sp_legend,
  nrow = 1, rel_widths = c(1, 1, 1, 0.35), align = "h", axis = "tb")

figS1_road_class

ggsave(file.path(supp_dir, "figS1_road_class.png"),
  figS1_road_class, width = 10, height = 4, dpi = 300, bg = "white")
cat("Saved: figS1_road_class.png\n")
