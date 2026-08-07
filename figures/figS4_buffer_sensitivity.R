# Supplemental Figure — Buffer-size sensitivity of encounter detection
# Total BaBA encounters detected across road buffer distances (20-80 m) at each
# site, expressed as a proportion of each site's maximum so the peak/plateau
# shape is comparable across sites of very different encounter volumes.
# Supports the 50 m buffer choice (R2-L204-206): detection rises to ~50 m then
# levels off or declines; smaller buffers under-detect.
# AEN 07-17-26

library(tidyverse)
library(here)

source(here("2_Scripts/revisions/figures/_colors.R"))

res <- read.csv(here("1_Data/revisions/buffer_sensitivity_results.csv")) %>%
  mutate(site = factor(site, levels = c("LSV", "TON", "SIU"))) %>%
  group_by(site) %>%
  mutate(prop_max = total_enc / max(total_enc)) %>%
  ungroup()

p_buffer <- ggplot(res, aes(x = buffer, y = prop_max, color = site)) +
  geom_vline(xintercept = 50, linetype = "dashed", color = "grey60", linewidth = 0.4) +
  geom_line(linewidth = 0.7) +
  geom_point(size = 1.8) +
  scale_color_manual(values = site_colors, name = "Site") +
  scale_x_continuous(breaks = seq(20, 80, 10)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(x = "Road buffer distance (m)",
       y = "Encounters detected (% of site maximum)") +
  theme_classic() +
  theme(
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 9),
    legend.title = element_text(size = 9, face = "bold"),
    legend.text = element_text(size = 9),
    legend.position = c(0.88, 0.25)
  )

p_buffer

ggsave(here("3_Figures/round2/supplemental/figS_buffer_sensitivity.png"),
  p_buffer, width = 5.5, height = 4, dpi = 300, bg = "white")
cat("Saved: figS_buffer_sensitivity.png\n")
