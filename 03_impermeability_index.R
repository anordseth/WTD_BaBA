# BaRanking — road impermeability index per 2km segment
# Adapted from Leyna Stemle's BaRanking_072325.R (Xu et al. 2021)
# SIU and TON use South roads; LSV uses North roads (same as script 02)

library(tidyverse)
library(sf)
library(lwgeom)
library(purrr)
library(BaBA)
library(here)

data_dir <- here("1_Data/leyna_data")
out_dir <- here("1_Data/revisions")

make_split_segments <- function(shp, segment_id_offset = 0) {
  roads_linestring <- shp %>%
    mutate(original_id = ObjectID_1) %>%
    st_cast("LINESTRING", group_or_split = TRUE)
  roads_segmentized <- st_segmentize(roads_linestring, dfMaxLength = 2000)
  split_line <- function(geometry) {
    coords <- st_coordinates(geometry)
    if (nrow(coords) < 2) return(NULL)
    segments <- map2(1:(nrow(coords) - 1), 2:nrow(coords), ~{
      st_linestring(rbind(coords[.x, 1:2], coords[.y, 1:2]))
    })
    st_sfc(segments, crs = st_crs(geometry))
  }
  split_segments_list <- roads_segmentized %>%
    mutate(road_index = row_number()) %>%
    rowwise() %>%
    mutate(geometry_list = list(split_line(geometry))) %>%
    ungroup() %>%
    filter(!sapply(geometry_list, is.null)) %>%
    dplyr::select(road_index, original_id, geometry_list) %>%
    tidyr::unnest(geometry_list) %>%
    mutate(geometry = geometry_list) %>%
    st_as_sf()
  split_segments_list %>% mutate(segment_id = row_number() + segment_id_offset)
}

# --- South roads (SIU, TON) ---
spatial_dir <- file.path(data_dir, "spatial")
shp_roadSouth <- st_transform(st_read(file.path(spatial_dir, "Trans_Road_Sroadsutm.shp")), crs = 32616)
split_segments_south <- make_split_segments(shp_roadSouth)
saveRDS(split_segments_south, file.path(out_dir, "south_roads_split_AEN.rds"))

# --- North roads (LSV) ---
unzip(file.path(spatial_dir, "Trans_Road_Nroadsutm.zip"), exdir = spatial_dir, overwrite = FALSE)
shp_roadNorth <- st_transform(st_read(file.path(spatial_dir, "Trans_Road_Nroadsutm.shp")), crs = 32616)
split_segments_north <- make_split_segments(shp_roadNorth)
saveRDS(split_segments_north, file.path(out_dir, "north_roads_split_AEN.rds"))

# --- Load BaBA classification results ---
load(file.path(out_dir, "BaBA_SIU_AEN.Rdata"))
load(file.path(out_dir, "BaBA_TON_AEN.Rdata"))
load(file.path(out_dir, "BaBA_LSV_AEN.Rdata"))

d <- 50

# original Xu et al. 2021 equation
index_fun <- expression(((Bounce + Back_n_forth + Trace + Trapped) / total_enc) * unique_ind)

# --- Run BaRanking per site ---
Rank_SIU <- BaRanking(BaBA_SIU$classification, split_segments_south, d,
  Barrier_ID = "segment_id", min_total_enc = 10, index_fun = index_fun, show_plot = FALSE)

Rank_TON <- BaRanking(BaBA_TON$classification, split_segments_south, d,
  Barrier_ID = "segment_id", min_total_enc = 10, index_fun = index_fun, show_plot = FALSE)

Rank_LSV <- BaRanking(BaBA_LSV$classification, split_segments_north, d,
  Barrier_ID = "segment_id", min_total_enc = 10, index_fun = index_fun, show_plot = FALSE)

saveRDS(Rank_SIU, file.path(out_dir, "Rank_SIU_AEN.rds"))
saveRDS(Rank_TON, file.path(out_dir, "Rank_TON_AEN.rds"))
saveRDS(Rank_LSV, file.path(out_dir, "Rank_LSV_AEN.rds"))

# --- Combine and save for Fig 3 ---
rank_SIU_df <- st_drop_geometry(na.omit(Rank_SIU)); rank_SIU_df$site <- "SIU"
rank_TON_df <- st_drop_geometry(na.omit(Rank_TON)); rank_TON_df$site <- "TON"
rank_LSV_df <- st_drop_geometry(na.omit(Rank_LSV)); rank_LSV_df$site <- "LSV"

allrank <- bind_rows(rank_SIU_df, rank_TON_df, rank_LSV_df)
write.csv(allrank, file.path(out_dir, "allrank_AEN.csv"), row.names = FALSE)
# Fig 3 is built in 3_Figures/main/main_figures_revisions.R
