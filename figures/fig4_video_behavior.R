# Figure 4 — Video behavior bar chart
# Behavior frequencies by BaBA encounter category + non-road baseline.
# AEN 05-25-26

library(tidyverse)
library(ggsignif)
library(here)

source(here("2_Scripts/revisions/figures/_colors.R"))

data_dir <- here("1_Data/leyna_data")

# ── Load data ─────────────────────────────────────────────────────────────────

cam <- read.csv(file.path(data_dir, "deer.cross.behav.agg_cam_forfig.csv"))

# Average movement dropped per R2-S5 (focus on quick cross and bounce; it was
# ~5% of encounters and did not differ from the non-road baseline).
cam <- cam %>%
  filter(type %in% c("Bounce", "Quick_cross", "z_No_road"))

# ── Pairwise chi-square tests ─────────────────────────────────────────────────
# Behavior-distribution differences between encounter categories, on raw counts.
# Stars are derived from these p-values so the brackets stay in sync with data.

behav_tab <- xtabs(freq ~ behav + type, data = cam)

chisq_pair <- function(a, b) {
  sub <- behav_tab[, c(a, b)]
  sub <- sub[rowSums(sub) > 0, ]
  suppressWarnings(chisq.test(sub)$p.value)
}

p_stars <- function(p) {
  ifelse(p < 0.001, "***", ifelse(p < 0.01, "**", ifelse(p < 0.05, "*", "ns")))
}

sig_p <- c(
  bounce_quick = chisq_pair("Bounce", "Quick_cross"),
  quick_noroad = chisq_pair("Quick_cross", "z_No_road"),
  bounce_noroad = chisq_pair("Bounce", "z_No_road")
)

# ── Plot data ─────────────────────────────────────────────────────────────────

cam <- cam %>%
  group_by(type) %>%
  mutate(rel_freq = freq / sum(freq)) %>%
  ungroup() %>%
  mutate(
    type = recode(type,
      "Quick_cross" = "Quick cross",
      "z_No_road"   = "Non-road"
    ),
    type = factor(type, levels = c("Bounce", "Quick cross", "Non-road"))
  ) %>%
  complete(type, behav, fill = list(freq = 0, rel_freq = 0))

# ── Grouped bar chart ─────────────────────────────────────────────────────────
# Per R1-Fig4: dodged (grouped) bars instead of a single stacked bar per
# encounter category, so behavior proportions are directly comparable. x =
# encounter category, one bar per video behavior within each.

fig4_bar <- ggplot(cam, aes(x = type, y = rel_freq, fill = behav)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.75) +
  scale_fill_manual("Video behavior", values = behavior_colors,
    labels = function(x) ifelse(x == "Forage", "Foraging", x)) +
  scale_y_continuous(
    labels = scales::label_percent(accuracy = 1),
    limits = c(0, 0.55),
    expand = expansion(mult = c(0, 0.02))
  ) +
  # Significance brackets = pairwise chi-square tests of the overall behavior
  # distribution between encounter categories. Umbrella over each category
  # cluster (kept as a quick-glance cue; also stated in the caption).
  geom_signif(
    annotations = p_stars(sig_p[c("bounce_quick", "quick_noroad", "bounce_noroad")]),
    xmin = c(1, 2, 1),
    xmax = c(2, 3, 3),
    y_position = c(0.45, 0.45, 0.51),
    tip_length = 0.01,
    textsize = 4,
    vjust = 0.4
  ) +
  labs(x = "Encounter category", y = "Relative frequency") +
  theme_classic() +
  theme(
    axis.text    = element_text(size = 9, color = "black"),
    axis.text.x  = element_text(size = 9, color = "black", face = "italic"),
    axis.title   = element_text(size = 10),
    axis.ticks   = element_line(color = "black"),
    legend.title = element_text(size = 9),
    legend.text  = element_text(size = 8)
  )

fig4_bar

ggsave(here("3_Figures/round2/main/fig4_behavior_bar.png"), fig4_bar,
  width = 7.5, height = 4.5, dpi = 300)
