# Shared foundation for all figures built on the study-site panels
# (Fig. 1 study-area map, deer-track overview, road-category map).
# Holds the pieces that MUST match across those figures — site extents,
# scale bars, corner labels, panel theme — plus reusable layer builders so
# each figure only supplies its own data layer on top of a common frame.
# Source this from a figure script; do not run standalone.
# AEN 07-08-26

library(tidyverse)
library(sf)
library(terra)
library(here)

source(here("2_Scripts/revisions/figures/_colors.R"))

.base_data_dir    <- here("1_Data/leyna_data")
.base_spatial_dir <- file.path(.base_data_dir, "spatial")

# ── Data ────────────────────────────────────────────────────────────────────

meta        <- read.csv(file.path(.base_data_dir, "deer_meta.csv"))
roads_N     <- st_read(file.path(.base_spatial_dir, "Trans_Road_Nroadsutm.shp"), quiet = TRUE)
roads_S     <- st_read(file.path(.base_spatial_dir, "Trans_Road_Sroadsutm.shp"), quiet = TRUE)
nlcd        <- rast(file.path(.base_spatial_dir, "nlcd_2023_sm/NLCD_2023_sm.tif"))

captures_sf <- meta %>%
  filter(!is.na(Latitude), !is.na(Longitude)) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
  st_transform(32616)

# ── Road classification ──────────────────────────────────────────────────────

# Grey roads for the land-cover base (Fig. 1, track overview)
classify_roads <- function(roads) {
  roads %>% mutate(road_class = case_when(
    mtfcc_code == "S1100" ~ "Primary",
    mtfcc_code == "S1200" ~ "Secondary",
    mtfcc_code == "S1400" ~ "Local",
    TRUE ~ "Other"
  ))
}
roads_N <- classify_roads(roads_N)
roads_S <- classify_roads(roads_S)
road_colors <- c(Primary = "grey35", Secondary = "grey35", Local = "grey35")
road_lwd    <- c(Primary = 1, Secondary = 0.5, Local = 0.2)

# Roads colored by OSM category (road-size figure). L/M/S mapping approximates
# the OSM categories used in the logistic regression.
classify_osm <- function(roads) {
  roads %>% mutate(osm_class = case_when(
    mtfcc_code %in% c("S1100", "S1200", "S1630") ~ "Large",
    mtfcc_code == "S1400" ~ "Medium",
    mtfcc_code %in% c("S1500", "S1640", "S1740") ~ "Small",
    TRUE ~ NA_character_
  )) %>%
    filter(!is.na(osm_class)) %>%
    mutate(osm_class = factor(osm_class, levels = c("Large", "Medium", "Small")))
}
roads_N_osm <- classify_osm(roads_N)
roads_S_osm <- classify_osm(roads_S)
osm_colors <- road_cat_colors
osm_lwd    <- c(Large = 0.5, Medium = 0.4, Small = 0.4)    # road-size figure; large only slightly heavier, medium = small (categories read by color)
# Separate road-class palette for the land-cover overlay (Option 2 main): the
# viridis road colors collide with land cover (teal roads vanish into forest,
# yellow into pasture), so use a warm ramp not present in the land cover.
osm_colors_overlay <- c(Large = "#000000", Medium = "#d1495b", Small = "#edae49")
# Thinner grey widths for roads-as-context on the land-cover base. Same road
# SET as the road-size figure (so both draw identical networks), but the bulk
# local (Medium) roads stay thin to preserve Fig. 1's look.
osm_lwd_base <- c(Large = 0.5, Medium = 0.2, Small = 0.2)

# ── NLCD ──────────────────────────────────────────────────────────────────────

# nlcd_colors / nlcd_labels sourced from colors.R
nlcd_labels["41"] <- "Dec. forest"
legend_classes <- nlcd_legend_classes

get_nlcd_df <- function(bbox) {
  e <- ext(bbox["xmin"], bbox["xmax"], bbox["ymin"], bbox["ymax"])
  cr <- crop(nlcd, e)
  as.data.frame(cr, xy = TRUE) %>%
    rename(class = 3) %>%
    filter(!is.na(class), class != 128) %>%
    mutate(class = as.character(class))
}

# ── Site extents (identical across all site-panel figures) ───────────────────

get_bbox <- function(area, buf = 5000) {
  pts <- captures_sf %>% filter(Study_area == area)
  bb <- st_bbox(st_buffer(st_union(pts$geometry), buf))
  cx <- (bb["xmin"] + bb["xmax"]) / 2
  cy <- (bb["ymin"] + bb["ymax"]) / 2
  half <- max(bb["xmax"] - bb["xmin"], bb["ymax"] - bb["ymin"]) / 2
  st_bbox(c(xmin = unname(cx - half), xmax = unname(cx + half),
    ymin = unname(cy - half), ymax = unname(cy + half)),
    crs = st_crs(32616))
}

bbox_shv_raw <- get_bbox("Shelbyville", buf = 2500)
roads_bb <- st_bbox(roads_N)
bbox_shv <- st_bbox(c(
  xmin = max(bbox_shv_raw["xmin"], roads_bb["xmin"]),
  xmax = min(bbox_shv_raw["xmax"], roads_bb["xmax"]),
  ymin = max(bbox_shv_raw["ymin"], roads_bb["ymin"]),
  ymax = min(bbox_shv_raw["ymax"], roads_bb["ymax"])),
  crs = st_crs(32616))
bbox_ton <- get_bbox("Touch of Nature", buf = 5000)
bbox_siu <- get_bbox("SIUC", buf = 5000)

# ── Site lookups (keyed by abbreviation) ─────────────────────────────────────

site_bbox      <- list(LSV = bbox_shv, TON = bbox_ton, SIU = bbox_siu)
site_roads     <- list(LSV = roads_N,     TON = roads_S,     SIU = roads_S)
site_roads_osm <- list(LSV = roads_N_osm, TON = roads_S_osm, SIU = roads_S_osm)
site_bar_km    <- c(LSV = 2, TON = 2, SIU = 2)           # uniform 2 km so bar
                                                         # length is comparable
                                                         # across panels
site_area      <- c(LSV = "Shelbyville", TON = "Touch of Nature", SIU = "SIUC")

# site_colors (LSV/TON/SIU) comes from colors.R; also key by full area name
# for the Illinois overview, which labels points by Study_area.
site_colors_area <- setNames(unname(site_colors[c("LSV", "TON", "SIU")]),
  c("Shelbyville", "Touch of Nature", "SIUC"))

# ── Scale bar (UTM metres) ────────────────────────────────────────────────────

add_scalebar <- function(bbox, bar_km = 5, side = "left") {
  bar_m <- bar_km * 1000
  xrange <- bbox["xmax"] - bbox["xmin"]
  yrange <- bbox["ymax"] - bbox["ymin"]
  if (side == "right") {
    x0 <- bbox["xmax"] - xrange * 0.08 - bar_m
    x1 <- bbox["xmax"] - xrange * 0.08
  } else {
    x0 <- bbox["xmin"] + xrange * 0.06
    x1 <- bbox["xmin"] + xrange * 0.06 + bar_m
  }
  y0 <- bbox["ymin"] + yrange * 0.06
  list(
    annotate("rect",
      xmin = x0 - xrange * 0.02, xmax = x1 + xrange * 0.02,
      ymin = y0 - yrange * 0.03, ymax = y0 + yrange * 0.075,
      fill = "white", alpha = 0.7, color = NA),
    annotate("segment", x = x0, xend = x1, y = y0, yend = y0,
      color = "black", linewidth = 0.9),
    annotate("segment", x = x0, xend = x0, y = y0 - yrange * 0.008,
      yend = y0 + yrange * 0.008, color = "black", linewidth = 0.9),
    annotate("segment", x = x1, xend = x1, y = y0 - yrange * 0.008,
      yend = y0 + yrange * 0.008, color = "black", linewidth = 0.9),
    annotate("text", x = (x0 + x1) / 2, y = y0 + yrange * 0.05,
      label = paste0(bar_km, " km"), size = 2.8, hjust = 0.5, fontface = "bold")
  )
}

# ── Panel theme ───────────────────────────────────────────────────────────────

site_theme <- theme_void() +
  theme(
    plot.background = element_rect(fill = "white", color = "grey30", linewidth = 1),
    plot.margin = margin(5, 5, 5, 5),
    plot.title = element_text(size = 9, hjust = 0.5, face = "bold",
      margin = margin(b = 3)),
    legend.position = "none"
  )

# ── Reusable panel components ─────────────────────────────────────────────────
# Each figure builds a panel as:
#   ggplot() + <base layers> + <its data layer> + site_frame(site, border)
# so extent, scale bar, corner label, and theme stay identical everywhere.

# Land-cover + grey-road base (Fig. 1, track overview). Draws the same OSM
# road SET as the road-size figure (Large/Medium/Small) so the two figures
# show identical networks — just in grey and thinner here.
landcover_layers <- function(site, palette = nlcd_colors, alpha = 0.75) {
  bbox <- site_bbox[[site]]
  nlcd_df <- get_nlcd_df(bbox)
  roads_c <- tryCatch(st_crop(site_roads_osm[[site]], st_as_sfc(bbox)),
    error = function(e) site_roads_osm[[site]])
  list(
    geom_raster(data = nlcd_df, aes(x = x, y = y, fill = class), alpha = alpha),
    scale_fill_manual(values = palette, na.value = "white", guide = "none"),
    geom_sf(data = roads_c %>% filter(osm_class == "Small"),
      color = "grey35", linewidth = osm_lwd_base["Small"]),
    geom_sf(data = roads_c %>% filter(osm_class == "Medium"),
      color = "grey35", linewidth = osm_lwd_base["Medium"]),
    geom_sf(data = roads_c %>% filter(osm_class == "Large"),
      color = "grey35", linewidth = osm_lwd_base["Large"])
  )
}

# Muted base (all land cover classes desaturated) for the track-overview figure.
muted_landcover_layers <- function(site) landcover_layers(site, palette = nlcd_colors_muted, alpha = 1)

# Roads colored by OSM size category on a white background (road-size figure)
osm_road_layers <- function(site) {
  bbox <- site_bbox[[site]]
  roads_c <- tryCatch(st_crop(site_roads_osm[[site]], st_as_sfc(bbox)),
    error = function(e) site_roads_osm[[site]])
  list(
    geom_sf(data = roads_c %>% filter(osm_class == "Small"),
      color = osm_colors["Small"], linewidth = osm_lwd["Small"]),
    geom_sf(data = roads_c %>% filter(osm_class == "Medium"),
      color = osm_colors["Medium"], linewidth = osm_lwd["Medium"]),
    geom_sf(data = roads_c %>% filter(osm_class == "Large"),
      color = osm_colors["Large"], linewidth = osm_lwd["Large"])
  )
}

# Shared frame added LAST: scale bar, corner label, extent, theme, border.
# border_color lets each figure choose its border (site color vs neutral)
# while keeping identical weight and placement.
site_frame <- function(site, border_color = "grey30", border_lwd = 3.5) {
  bbox <- site_bbox[[site]]
  xrange <- bbox["xmax"] - bbox["xmin"]
  yrange <- bbox["ymax"] - bbox["ymin"]
  list(
    add_scalebar(bbox, bar_km = site_bar_km[[site]], side = "left"),
    annotate("label",
      x = bbox["xmin"] + xrange * 0.03,
      y = bbox["ymax"] - yrange * 0.03,
      label = site, hjust = 0, vjust = 1, size = 4.2,
      fontface = "bold", label.size = 0, fill = "white", alpha = 0.85),
    coord_sf(crs = 32616,
      xlim = c(bbox["xmin"], bbox["xmax"]),
      ylim = c(bbox["ymin"], bbox["ymax"]),
      expand = FALSE),
    labs(title = NULL),
    site_theme,
    theme(
      plot.background = element_rect(fill = "white", color = NA),
      panel.border = element_rect(color = border_color, fill = NA, linewidth = border_lwd),
      plot.margin = margin(3, 3, 3, 3)
    )
  )
}

# Land cover raster + colored (by size class) roads — for the Option 2
# main-figure variant. Unlike landcover_layers (grey roads), roads here carry
# the L/M/S color encoding on top of land cover.
lc_plus_roadclass_layers <- function(site) {
  bbox <- site_bbox[[site]]
  nlcd_df <- get_nlcd_df(bbox)
  roads_c <- tryCatch(st_crop(site_roads_osm[[site]], st_as_sfc(bbox)),
    error = function(e) site_roads_osm[[site]])
  list(
    geom_raster(data = nlcd_df, aes(x = x, y = y, fill = class), alpha = 0.75),
    scale_fill_manual(values = nlcd_colors, na.value = "white", guide = "none"),
    geom_sf(data = roads_c %>% filter(osm_class == "Small"),
      color = osm_colors_overlay["Small"], linewidth = osm_lwd["Small"]),
    geom_sf(data = roads_c %>% filter(osm_class == "Medium"),
      color = osm_colors_overlay["Medium"], linewidth = osm_lwd["Medium"]),
    geom_sf(data = roads_c %>% filter(osm_class == "Large"),
      color = osm_colors_overlay["Large"], linewidth = osm_lwd["Large"])
  )
}

# Safe road-class palette for the combined track + roads supplement figure:
# warm/neutral hues distinct from the blue/green/purple site (track) colors and
# from the muted land cover. Colorblind-aware (Okabe-Ito).
road_cat_colors_safe <- c(Large = "#000000", Medium = "#E69F00", Small = "#CC3399")

# Roads colored by size class (safe palette) on a plain white base, no land
# cover. Base for the supplement tracks + road-class figure.
roadclass_safe_layers <- function(site) {
  bbox <- site_bbox[[site]]
  roads_c <- tryCatch(st_crop(site_roads_osm[[site]], st_as_sfc(bbox)),
    error = function(e) site_roads_osm[[site]])
  lwd <- c(Large = 0.5, Medium = 0.35, Small = 0.35)   # small = medium; large only slightly heavier (classes read by color)
  list(
    geom_sf(data = roads_c %>% filter(osm_class == "Small"),
      color = road_cat_colors_safe["Small"], linewidth = lwd["Small"]),
    geom_sf(data = roads_c %>% filter(osm_class == "Medium"),
      color = road_cat_colors_safe["Medium"], linewidth = lwd["Medium"]),
    geom_sf(data = roads_c %>% filter(osm_class == "Large"),
      color = road_cat_colors_safe["Large"], linewidth = lwd["Large"])
  )
}

# Muted land cover + roads colored by size class (safe palette), no grey roads.
# Base for the combined supplement figure (land cover + road class + tracks).
muted_lc_roadclass_layers <- function(site) {
  bbox <- site_bbox[[site]]
  nlcd_df <- get_nlcd_df(bbox)
  roads_c <- tryCatch(st_crop(site_roads_osm[[site]], st_as_sfc(bbox)),
    error = function(e) site_roads_osm[[site]])
  # Thin the dense medium/small network so roads read as context, not the focus;
  # keep large arterials prominent.
  lwd <- c(Large = 0.5, Medium = 0.22, Small = 0.22)
  list(
    geom_raster(data = nlcd_df, aes(x = x, y = y, fill = class)),
    scale_fill_manual(values = nlcd_colors_muted, na.value = "white", guide = "none"),
    geom_sf(data = roads_c %>% filter(osm_class == "Small"),
      color = road_cat_colors_safe["Small"], linewidth = lwd["Small"]),
    geom_sf(data = roads_c %>% filter(osm_class == "Medium"),
      color = road_cat_colors_safe["Medium"], linewidth = lwd["Medium"]),
    geom_sf(data = roads_c %>% filter(osm_class == "Large"),
      color = road_cat_colors_safe["Large"], linewidth = lwd["Large"])
  )
}
