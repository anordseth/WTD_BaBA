# Video-recorded behavior during road encounters: chi-square tests
# Adapted from John Vinson / Leyna Stemle's original chi-square script.
# AEN 08-11-26: cleaned up for GitHub — reads video_behavior_counts.csv
#   directly, dropped the plotting code (Fig. 4 is built elsewhere), and
#   kept only the three comparisons reported in the manuscript.

library(here)

data_dir <- here("7_Dryad_Upload/data")

# Load data# One row per (encounter type, behavior): counts of scored video segments.
# type: Quick_cross, Bounce, Average_movement, z_No_road (non-road baseline)

d <- read.csv(file.path(data_dir, "video_behavior_counts.csv"))

qc <- subset(d, type == "Quick_cross")
bounce <- subset(d, type == "Bounce")
noroad <- subset(d, type == "z_No_road")

# Align behavior order across groups so freq vectors line up positionally
bounce <- bounce[match(qc$behav, bounce$behav), ]
noroad <- noroad[match(qc$behav, noroad$behav), ]

qc$rel.freq.behav <- qc$freq / sum(qc$freq)
bounce$rel.freq.behav <- bounce$freq / sum(bounce$freq)
noroad$rel.freq.behav <- noroad$freq / sum(noroad$freq)

# Chi-square tests# Each test asks whether one group's behavior counts follow the behavior
# proportions observed in a comparison group. Matches the three comparisons
# reported in the manuscript (Results: "Behaviors During Road Encounters").

qc_bounce <- chisq.test(bounce$freq, p = qc$rel.freq.behav)
qc_noroad <- chisq.test(qc$freq, p = noroad$rel.freq.behav)
bounce_noroad <- chisq.test(bounce$freq, p = noroad$rel.freq.behav)

results <- data.frame(
  comparison = c("quick cross vs. bounce", "quick cross vs. non-road baseline", "bounce vs. non-road baseline"),
  chi_sq = c(qc_bounce$statistic, qc_noroad$statistic, bounce_noroad$statistic),
  df = c(qc_bounce$parameter, qc_noroad$parameter, bounce_noroad$parameter),
  p = c(qc_bounce$p.value, qc_noroad$p.value, bounce_noroad$p.value)
)
results

out_dir <- here("1_Data/revisions")
write.csv(results, file.path(out_dir, "video_behavior_chisq_results.csv"), row.names = FALSE)
