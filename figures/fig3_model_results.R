# Figure 3 — Logistic model results: quick cross vs. bounce
# (A) Odds ratio forest plot
# (B) Continuous land cover predicted probabilities
# (C) Season predicted probabilities
# (D) Diel period predicted probabilities
# Addresses: R1-Fig3B (CI bands, panel sizing), R2-Fig3
# Updated 07-06-26: switched to linear-only model (AIC-selected); removed ln() terms;
#   widened CI ribbon; expanded panel B height.
# AEN updated 06-03-26

library(tidyverse)
library(glmmTMB)
library(ggeffects)
library(patchwork)
library(cowplot)
library(here)

source(here("2_Scripts/revisions/figures/_colors.R"))

data_dir <- here("1_Data/leyna_data")

# ── Load and merge data ────────────────────────────────────────────────────────

df5_old <- read.csv(file.path(data_dir,
  "scaled_centered_transform_logisticfixed_roadnlcd.csv"))

df5_new <- read.csv(file.path(data_dir,
  "df5_ new road cat osm newcat_deposit_condense.csv")) %>%
  select(burstID, road_category_new = road_category)

# Reference levels (alphabetical defaults): season = fawn, time.cat = daytime,
# road_category = L (large). Matches 06_logistic_model.R output.
df5 <- df5_old %>%
  left_join(df5_new, by = "burstID") %>%
  mutate(road_category = road_category_new) %>%
  select(-road_category_new) %>%
  mutate(
    type = factor(type),
    road_category = factor(road_category),
    season = factor(season),
    time.cat = factor(time.cat)
  )

stopifnot(all(!is.na(df5$road_category)))

# ── Fit linear-only model (AIC-selected over log-transformed alternative) ─────

model_top <- glmmTMB(
  type ~ std_forest + std_agri + std_dev +
    (1 | site) + (1 | AnimalID) +
    season + time.cat + std_rdden + road_category,
  data = df5,
  family = binomial(link = "logit")
)

# ── Extract odds ratios ───────────────────────────────────────────────────────

coef_mat <- summary(model_top)$coefficients$cond
coefs <- as.data.frame(coef_mat) %>%
  rownames_to_column("term") %>%
  filter(term != "(Intercept)") %>%
  rename(estimate = Estimate, se = `Std. Error`, z = `z value`, p = `Pr(>|z|)`) %>%
  mutate(
    or = exp(estimate),
    ci_low = exp(estimate - 1.96 * se),
    ci_high = exp(estimate + 1.96 * se),
    sig = p < 0.05,
    group = case_when(
      str_detect(term, "season") ~ "Season",
      str_detect(term, "time\\.cat") ~ "Diel",
      str_detect(term, "rdden|road_category") ~ "Road",
      TRUE ~ "Land cover"
    ),
    label = case_when(
      term == "std_forest" ~ "Forest",
      term == "std_agri" ~ "Agriculture",
      term == "std_dev" ~ "Development",
      term == "std_rdden" ~ "Road density",
      str_detect(term, "season") ~ recode(str_remove(term, "season"),
        postfawn = "Post-fawning", prefawn = "Pre-fawning", rut = "Rut"),
      str_detect(term, "time\\.cat") ~ recode(str_remove(term, "time\\.cat"),
        twilight = "Twilight", nighttime = "Night"),
      str_detect(term, "road_category") ~ recode(str_remove(term, "road_category"),
        M = "Medium road", S = "Small road"),
      TRUE ~ term
    )
  )

all_coefs <- coefs %>%
  mutate(
    group = factor(group, levels = c("Land cover", "Season", "Diel", "Road")),
    label = factor(label, levels = c(
      "Forest", "Development", "Agriculture",
      "Post-fawning", "Pre-fawning", "Rut",
      "Twilight", "Night",
      "Medium road", "Small road",
      "Road density"
    ))
  )

# ── Panel A: Odds ratio forest plot ──────────────────────────────────────────

fig3a <- ggplot(all_coefs, aes(x = or, y = label, color = factor(sig))) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "grey40", linewidth = 0.4) +
  geom_pointrange(
    aes(xmin = ci_low, xmax = ci_high),
    size = 0.5, linewidth = 0.6, shape = 16,
    na.rm = TRUE
  ) +
  scale_color_manual(values = sig_colors_tf) +
  scale_x_log10() +
  coord_cartesian(clip = "off") +
  facet_grid(group ~ ., scales = "free_y", space = "free_y", switch = "y") +
  labs(x = "Odds ratio", y = NULL) +
  theme_classic() +
  theme(
    axis.text = element_text(size = 9, color = "black"),
    axis.title.x = element_text(size = 11, margin = margin(t = 6)),
    axis.ticks = element_line(color = "black"),
    legend.position = "none",
    strip.placement = "outside",
    strip.background = element_blank(),
    strip.text.y.left = element_text(size = 9, angle = 90, hjust = 0.5, face = "bold"),
    plot.margin = margin(5, 5, 5, 12)
  )

# ── Panel B: Continuous land cover predicted probabilities ────────────────────
# Linear terms varied across 2nd-98th percentile of each variable.
# All other predictors held at mean (scaled = 0) or reference level.
# CIs are 95% Wald intervals on the log-odds scale, back-transformed to probability.

sc <- function(x) { x <- x[is.finite(x)]; list(mu = mean(x), sd = sd(x)) }
sp <- list(
  forest = sc(df5_old$X41),
  agri = sc(df5_old$X81),
  dev = sc(df5_old$X22),
  rdden = sc(df5_old$rdkm_sqkm)
)

base_nd <- df5[1, ] %>%
  mutate(
    across(c(std_forest, std_agri, std_dev, std_rdden), ~ 0),
    season = factor("fawn", levels(df5$season)),
    time.cat = factor("daytime", levels(df5$time.cat)),
    road_category = factor("L", levels(df5$road_category))
  )

make_cont <- function(raw_seq, raw_key, std_col, label) {
  n <- length(raw_seq)
  nd <- base_nd[rep(1, n), ]
  nd[[std_col]] <- (raw_seq - sp[[raw_key]]$mu) / sp[[raw_key]]$sd
  pr <- predict(model_top, newdata = nd, type = "link", se.fit = TRUE, re.form = NA)
  tibble(
    x = raw_seq,
    fit = plogis(pr$fit),
    lwr = plogis(pr$fit - 1.96 * pr$se.fit),
    upr = plogis(pr$fit + 1.96 * pr$se.fit),
    panel = label
  )
}

figB_data <- bind_rows(
  make_cont(
    seq(quantile(df5_old$X41, .02), quantile(df5_old$X41, .98), length.out = 100),
    "forest", "std_forest", "Forest"
  ),
  make_cont(
    seq(quantile(df5_old$X22, .02, na.rm = TRUE),
        quantile(df5_old$X22, .98, na.rm = TRUE), length.out = 100),
    "dev", "std_dev", "Development"
  ),
  make_cont(
    seq(quantile(df5_old$X81, .02, na.rm = TRUE),
        quantile(df5_old$X81, .98, na.rm = TRUE), length.out = 100),
    "agri", "std_agri", "Agriculture"
  )
) %>%
  mutate(panel = factor(panel, levels = c("Forest", "Development", "Agriculture")))

ylim_b <- c(0, 0.70)

fig3b <- ggplot(figB_data, aes(x = x, y = fit, color = panel, fill = panel)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.35, color = NA) +
  geom_line(linewidth = 0.8) +
  scale_color_manual(values = landcover_colors) +
  scale_fill_manual(values = landcover_colors) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.05)),
                     limits = c(0, NA)) +
  facet_wrap(~ panel, scales = "free_x", ncol = 3) +
  scale_y_continuous(
    labels = scales::label_percent(accuracy = 1),
    limits = ylim_b
  ) +
  labs(x = "Proportion cover", y = "Predicted probability") +
  theme_classic() +
  theme(
    axis.text = element_text(size = 9, color = "black"),
    axis.title = element_text(size = 10),
    axis.ticks = element_line(color = "black"),
    legend.position = "none",
    strip.background = element_blank(),
    strip.text = element_text(size = 9, face = "bold"),
    panel.spacing.x = unit(1, "lines")
  )

# ── Panel C: Categorical predicted probabilities (season + diel) ─────────────
# ggpredict holds all other terms at their mean/reference level. Season and
# diel share one panel, faceted with strip labels above (like panel B).

season_pred <- ggpredict(model_top, "season") %>%
  as_tibble() %>%
  mutate(x = recode(as.character(x),
    fawn = "Fawning", postfawn = "Post-fawning",
    prefawn = "Pre-fawning", rut = "Rut"
  ))

diel_pred <- ggpredict(model_top, "time.cat") %>%
  as_tibble() %>%
  mutate(x = recode(as.character(x),
    daytime = "Day", nighttime = "Night", twilight = "Twilight"
  ))

cat_theme <- theme_classic() +
  theme(
    axis.text = element_text(size = 9, color = "black"),
    axis.title = element_text(size = 10),
    axis.ticks = element_line(color = "black"),
    legend.position = "none",
    plot.margin = margin(t = 20, r = 8, b = 4, l = 22)
  )

ylim_cd <- c(0, max(season_pred$conf.high, diel_pred$conf.high) * 1.05)

cd_data <- bind_rows(
  season_pred %>% transmute(x = as.character(x), predicted, conf.low, conf.high, facet = "Season"),
  diel_pred   %>% transmute(x = as.character(x), predicted, conf.low, conf.high, facet = "Diel period")
) %>%
  mutate(
    facet = factor(facet, levels = c("Season", "Diel period")),
    x = factor(x, levels = c("Fawning", "Pre-fawning", "Post-fawning", "Rut",
                             "Day", "Twilight", "Night"))
  )

fig3c <- ggplot(cd_data, aes(x = x, y = predicted)) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high),
    size = 0.5, linewidth = 0.6, color = "grey30") +
  facet_wrap(~ facet, scales = "free_x", ncol = 2) +
  scale_y_continuous(
    labels = scales::label_percent(accuracy = 1),
    limits = ylim_cd
  ) +
  scale_x_discrete(expand = expansion(add = 0.4)) +
  labs(x = NULL, y = "Predicted probability") +
  cat_theme +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.background = element_blank(),
    strip.text = element_text(size = 9, face = "bold", margin = margin(b = 6)),
    panel.spacing.x = unit(2.5, "lines")
  )

# ── Combine ───────────────────────────────────────────────────────────────────
# cowplot stack (no axis alignment — patchwork alignment inserted gaps between
# the y-axis and panels). A's left margin trimmed above to better match B's
# panel width. Season and diel now share one faceted panel C.

fig3_combined <- plot_grid(
  fig3a, fig3b, fig3c,
  ncol = 1,
  rel_heights = c(2, 1.5, 1.5),
  labels = c("A", "B", "C"),
  label_size = 12
)

fig3_combined

ggsave(
  here("3_Figures/round2/main/fig3_combined_round2.png"),
  fig3_combined,
  width = 8, height = 9.5, dpi = 300
)
