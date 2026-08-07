# Fig. 2: Road density, encounter rate, encounter types, and permeability — 2x2
# AEN 06-01-26

library(tidyverse)
library(sf)
library(patchwork)
library(here)

source(here("2_Scripts/revisions/figures/_colors.R"))

# ── Road density ──────────────────────────────────────────────────────────────

logistic_data <- read.csv(
  here("1_Data/leyna_data/scaled_centered_transform_logisticfixed_roadnlcd.csv")
)

road_density <- logistic_data %>%
  group_by(AnimalID, Study_area) %>%
  summarise(road_density = first(rdkm_sqkm), .groups = "drop") %>%
  mutate(
    site = case_when(
      Study_area == "SIUC"            ~ "SIU",
      Study_area == "Shelbyville"     ~ "LSV",
      Study_area == "Touch of Nature" ~ "TON"
    ),
    group = site
  )

# ── Encounter rate ────────────────────────────────────────────────────────────

# Computed in 2_Scripts/revisions/04_deer_summaries.R — rerun that script if the
# BaBA output or the individual summary changes.
enc_rate <- read.csv(here("1_Data/revisions/encounter_rates_AEN.csv")) %>%
  mutate(group = site)

# ── Encounter type proportions ────────────────────────────────────────────────

freq <- read.csv(here("1_Data/revisions/freq_summary_AEN.csv")) %>%
  filter(eventTYPE != "unknown") %>%
  mutate(
    site = case_when(
      Study_area == "SIUC"            ~ "SIU",
      Study_area == "Shelbyville"     ~ "LSV",
      Study_area == "Touch of Nature" ~ "TON"
    ),
    site = factor(site, levels = c("LSV", "TON", "SIU")),
    eventTYPE = factor(eventTYPE,
      levels  = c("Quick_Cross", "Average_Movement", "Bounce",
                  "Back_n_forth", "Trace", "Trapped"),
      labels  = c("Quick cross", "Average movement", "Bounce",
                  "Back-and-forth", "Trace", "Trapped")
    )
  )

# ── Permeability: raw non-crossing fraction (cross-site comparable) ───────────
# Raw fraction of encounters that were non-crossing behaviors per 2 km segment,
# from the per-site Rank objects. NOT the per-site rescaled `index` in
# allrank_AEN.csv, which forced each site's worst segment to 1.0 and is not
# comparable across sites (it reversed the SIU permeability conclusion). Bounded
# [0,1], no unique_ind multiplier. See fig_perm_density_crosssite.R.
min_enc  <- 10
raw_frac <- function(d) (d$Bounce + d$Back_n_forth + d$Trace + d$Trapped) / d$total_enc
allrank <- bind_rows(lapply(c("LSV", "TON", "SIU"), function(s) {
  r <- st_drop_geometry(readRDS(here(sprintf("1_Data/revisions/Rank_%s_AEN.rds", s))))
  r <- r[!is.na(r$total_enc) & r$total_enc >= min_enc, ]
  data.frame(frac = raw_frac(r), site = s)
})) %>%
  mutate(site = factor(site, levels = c("LSV", "TON", "SIU")))

# ── Shared settings ───────────────────────────────────────────────────────────

group_order  <- c("LSV", "TON", "SIU")
group_colors <- site_colors
group_labels <- c(
  "LSV" = "LSV\n(n=47)",
  "TON" = "TON\n(n=68)",
  "SIU" = "SIU\n(n=10)"
)

road_density <- road_density %>% mutate(group = factor(group, levels = group_order))
enc_rate     <- enc_rate     %>% mutate(group = factor(group, levels = group_order))

box_theme <- theme_classic() +
  theme(
    axis.title.x    = element_blank(),
    axis.text.x     = element_text(size = 10),
    axis.title.y    = element_text(size = 10),
    axis.text.y     = element_text(size = 9),
    legend.position = "none"
  )

# ── Panel A: home range road density ─────────────────────────────────────────

p_density <- ggplot(road_density, aes(x = group, y = road_density, fill = group)) +
  geom_boxplot(outlier.shape = NA, width = 0.55, alpha = 0.85) +
  geom_jitter(width = 0.15, size = 1.2, alpha = 0.45, color = "grey25") +
  scale_fill_manual(values = group_colors) +
  scale_x_discrete(labels = group_labels) +
  labs(y = expression("Road density (km/km"^2*")")) +
  box_theme

# ── Panel B: encounter rate ───────────────────────────────────────────────────

p_enc <- ggplot(enc_rate, aes(x = group, y = enc_per_day, fill = group)) +
  geom_boxplot(outlier.shape = NA, width = 0.55, alpha = 0.85) +
  geom_jitter(width = 0.15, size = 1.2, alpha = 0.45, color = "grey25") +
  scale_fill_manual(values = group_colors) +
  scale_x_discrete(labels = group_labels) +
  labs(y = "Road encounters per day") +
  box_theme

# ── Panel C: encounter type proportions ───────────────────────────────────────

p_freq <- ggplot(freq, aes(x = site, y = prop, fill = eventTYPE)) +
  geom_col(width = 0.65) +
  scale_fill_manual(values = encounter_colors, name = "Encounter type",
                    labels = c("Quick cross", "Avg. movement", "Bounce",
                               "Back-and-forth", "Trace", "Trapped")) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     expand = expansion(mult = c(0, 0.02))) +
  labs(x = NULL, y = "Proportion of encounters") +
  theme_classic() +
  theme(
    axis.text.x  = element_text(size = 10),
    axis.title.y = element_text(size = 10),
    legend.title = element_text(size = 9),
    legend.text  = element_text(size = 8),
    legend.key.size = unit(0.4, "cm"),
    legend.position = "right",
    legend.margin = margin(l = -8)
  )

# ── Panel D: non-crossing fraction density (cross-site comparable) ─────────────

p_perm <- ggplot(allrank, aes(x = frac, fill = site, color = site)) +
  geom_density(alpha = 0.45, linewidth = 0.7) +
  scale_fill_manual(values  = site_colors, name = "Site") +
  scale_color_manual(values = site_colors, name = "Site") +
  scale_x_continuous(limits = c(0, 1),
                     breaks = c(0, 0.25, 0.5, 0.75, 1),
                     labels = c("0\n(permeable)", "0.25", "0.5", "0.75",
                                "1\n(impermeable)"),
                     expand = expansion(mult = c(0.02, 0.12))) +
  labs(x = "Road impermeability index", y = "Density") +
  theme_classic() +
  theme(
    axis.text.x  = element_text(size = 8),
    axis.title   = element_text(size = 10),
    legend.title = element_text(size = 9),
    legend.text  = element_text(size = 8),
    legend.key.size = unit(0.4, "cm"),
    legend.position = c(0.82, 0.75),
    legend.justification = c(0.5, 0.5)
  )

# ── Combine 2x2 ───────────────────────────────────────────────────────────────

fig_out <- (p_density + p_enc) / (p_freq + p_perm) +
  plot_annotation(tag_levels = "A") &
  theme(plot.tag = element_text(size = 12, face = "bold"))

fig_out

ggsave(
  here("3_Figures/round2/main/fig2_combined.png"),
  fig_out,
  width = 8, height = 7, dpi = 300
)
