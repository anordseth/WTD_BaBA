# Seasonal range of twilight window durations
# R1-L185: SI references seasonal variation in twilight window duration
# Calculates min/max twilight window duration across study period
# Study period: Jan 2023 - Mar 2025
# Location: lat 37.7, lon -89.2 (southern Illinois)

library(suncalc)
library(tidyverse)
library(here)

out_dir <- here("1_Data/revisions")

dates <- seq(as.Date("2023-01-01"), as.Date("2025-03-31"), by = "day")

sun <- getSunlightTimes(
  date = dates,
  lat = 37.7,
  lon = -89.2,
  tz = "America/Chicago"
)

# Morning twilight: nightEnd (astronomical dawn) to sunrise
# Evening twilight: sunset to night (astronomical dusk)
sun <- sun %>%
  mutate(
    dawn_duration_min = as.numeric(difftime(sunrise, nightEnd, units = "mins")),
    dusk_duration_min = as.numeric(difftime(night, sunset, units = "mins")),
    min_twilight_min = pmin(dawn_duration_min, dusk_duration_min),
    month = format(date, "%Y-%m")
  )

# Summary
cat("--- Morning twilight (astronomical dawn to sunrise) ---\n")
cat("Min:", round(min(sun$dawn_duration_min), 1), "min\n")
cat("Max:", round(max(sun$dawn_duration_min), 1), "min\n")

cat("\n--- Evening twilight (sunset to astronomical dusk) ---\n")
cat("Min:", round(min(sun$dusk_duration_min), 1), "min\n")
cat("Max:", round(max(sun$dusk_duration_min), 1), "min\n")

# Monthly means for SI table
monthly_summary <- sun %>%
  group_by(month) %>%
  summarise(
    mean_dawn_min = round(mean(dawn_duration_min), 1),
    mean_dusk_min = round(mean(dusk_duration_min), 1)
  )

write.csv(monthly_summary, file.path(out_dir, "twilight_windows_AEN.csv"), row.names = FALSE)

# ── Figure: actual twilight times by date ─────────────────────────────────────
# Convert times to decimal hours for plotting
to_decimal_hour <- function(x) {
  as.numeric(format(x, "%H")) + as.numeric(format(x, "%M")) / 60
}

sun_plot <- sun %>%
  mutate(
    nightEnd_h  = to_decimal_hour(nightEnd),
    sunrise_h   = to_decimal_hour(sunrise),
    sunset_h    = to_decimal_hour(sunset),
    night_h     = to_decimal_hour(night)
  ) %>%
  pivot_longer(cols = c(nightEnd_h, sunrise_h, sunset_h, night_h),
               names_to = "event", values_to = "hour") %>%
  mutate(event = factor(event,
    levels = c("nightEnd_h", "sunrise_h", "sunset_h", "night_h"),
    labels = c("Astronomical dawn", "Sunrise", "Sunset", "Astronomical dusk")))

ggplot(sun_plot, aes(x = date, y = hour, color = event)) +
  geom_line() +
  scale_color_manual(values = c("#0072B2", "#E69F00", "#E69F00", "#0072B2"),
                     guide = guide_legend(title = NULL)) +
  scale_y_continuous(breaks = seq(4, 22, by = 2),
                     labels = function(x) sprintf("%02d:00", x)) +
  labs(x = NULL, y = "Time of day") +
  theme_classic()

ggsave(file.path(out_dir, "twilight_times_AEN.png"), width = 8, height = 4)
