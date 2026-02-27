# ============================================================
# Multiple experiment only: one driver vs more than one driver
# Split by targeted stressor level (T=24, pH=7.7, Li=7)
## NOTE ##
# Requires all_data and multi_raw from consistency_analysis.R
# ============================================================

library(ggplot2)
library(dplyr)
library(cowplot)

# ============================================================
# 1. Build subsets from multi_raw, tag driver complexity
# ============================================================

REF_T  <- 20
REF_pH <- 8.1
REF_Li <- 0.18

multi_tagged <- multi_raw %>%
  mutate(
    n_stressors_out   = (T_target != REF_T) + (pH_target != REF_pH) + (Li_target != REF_Li),
    driver_complexity = ifelse(n_stressors_out == 1, "One driver", "More drivers")
  )

# --- Temperature: all multi treatments with T=24 ---
temp_multi_24 <- multi_tagged %>%
  filter(T_target == 24) %>%
  mutate(driver_complexity = factor(driver_complexity,
                                    levels = c("One driver", "More drivers")))

# --- pH: all multi treatments with pH=7.7 ---
pH_multi_77 <- multi_tagged %>%
  filter(pH_target == 7.7) %>%
  mutate(driver_complexity = factor(driver_complexity,
                                    levels = c("One driver", "More drivers")))

# --- Lithium: all multi treatments with Li=7 ---
Li_multi_7 <- multi_tagged %>%
  filter(Li_target == 7) %>%
  mutate(driver_complexity = factor(driver_complexity,
                                    levels = c("One driver", "More drivers")))

# Quick check
cat("temp_multi_24:\n"); print(table(temp_multi_24$Treatment, temp_multi_24$driver_complexity))
cat("\npH_multi_77:\n");  print(table(pH_multi_77$Treatment,  pH_multi_77$driver_complexity))
cat("\nLi_multi_7:\n");   print(table(Li_multi_7$Treatment,   Li_multi_7$driver_complexity))

# ============================================================
# 2. Shared settings
# ============================================================

y_label <- expression(paste("Growth rate ("*mu*"m"%.%"day"^-1*")"))

driver_colors <- c("One driver"   = "#F0E442",
                   "More drivers" = "#CC79A7")

shared_theme <- theme_bw() +
  theme(
    axis.text    = element_text(size = 14),
    axis.title   = element_text(size = 16),
    legend.text  = element_text(size = 14),
    legend.title = element_text(size = 16),
    strip.text   = element_text(size = 15),
    plot.title   = element_text(size = 16, face = "bold")
  )

# ============================================================
# 3. Plots
# ============================================================

ptemp_multi <- ggplot(temp_multi_24,
                      aes(x = driver_complexity, y = GR, fill = driver_complexity)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha = 0.6, size = 2) +
  scale_fill_manual(name = "Drivers", values = driver_colors) +
  labs(x = "Drivers out of reference", y = y_label) +
  shared_theme +
  ggtitle("Temperature targeted: 24\u00b0C")+
  theme(legend.position = "none")+
  labs(x=NULL)+
  ylim(50, 85)

ppH_multi <- ggplot(pH_multi_77,
                    aes(x = driver_complexity, y = GR, fill = driver_complexity)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha = 0.6, size = 2) +
  scale_fill_manual(name = "Drivers", values = driver_colors) +
  labs(x = "Drivers out of reference", y = y_label) +
  shared_theme +
  ggtitle("pH targeted: 7.7")+
  theme(legend.position = "none")+
  labs(x=NULL)+
  ylim(50, 85)

pLi_multi <- ggplot(Li_multi_7,
                    aes(x = driver_complexity, y = GR, fill = driver_complexity)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha = 0.6, size = 2) +
  scale_fill_manual(name = "Drivers", values = driver_colors) +
  labs(x = "Drivers out of reference condition", y = y_label) +
  shared_theme +
  ggtitle("Lithium targeted: 7 ppm")+
  theme(legend.position = "none")+
  ylim(50, 85)


# ============================================================
# 4. Combined plot
# ============================================================

png("plot_multi_driver_complexity.png", width = 7, height = 13, units = "in", res = 300)
plot_grid(ptemp_multi, ppH_multi, pLi_multi, ncol = 1, align = "v", axis = "lr")
dev.off()

cat("Plot saved to plot_multi_driver_complexity.png\n")




# ============================================================
# ADDITION: same plots but with single driver treatment added
# Paste this at the end of multi_driver_plots.R
# ============================================================

# ============================================================
# 5. Load single treatments T4, P3, L3
# ============================================================

single_raw <- read_excel("full_data_single_pHinsitu.xlsx") %>%
  rename(
    T_target  = T_targed,
    pH_target = pH_targed,
    Li_target = Li_targed
  )

single_T4 <- single_raw %>%
  filter(Treatment == "T4") %>%
  select(Treatment, GR) %>%
  mutate(driver_complexity = "Single")

single_P3 <- single_raw %>%
  filter(Treatment == "P4") %>%
  select(Treatment, GR) %>%
  mutate(driver_complexity = "Single")

single_L3 <- single_raw %>%
  filter(Treatment == "L3") %>%
  select(Treatment, GR) %>%
  mutate(driver_complexity = "Single")

# ============================================================
# 6. Combine with multi subsets already in memory
# ============================================================

temp_combined <- bind_rows(
  temp_multi_24 %>% select(Treatment, GR, driver_complexity),
  single_T4
) %>%
  mutate(driver_complexity = factor(driver_complexity,
                                    levels = c("Single", "One driver", "More drivers")))

pH_combined <- bind_rows(
  pH_multi_77 %>% select(Treatment, GR, driver_complexity),
  single_P3
) %>%
  mutate(driver_complexity = factor(driver_complexity,
                                    levels = c("Single", "One driver", "More drivers")))

Li_combined <- bind_rows(
  Li_multi_7 %>% select(Treatment, GR, driver_complexity),
  single_L3
) %>%
  mutate(driver_complexity = factor(driver_complexity,
                                    levels = c("Single", "One driver", "More drivers")))

# ============================================================
# 7. Updated color scale (3 groups now)
# ============================================================

driver_colors_3 <- c(
  "Single"       = "#56B4E9",
  "One driver"   = "#CC79A7",
  "More drivers" = "#F0E442"
)

# ============================================================
# 8. Plots
# ============================================================

ptemp_combined <- ggplot(temp_combined,
                         aes(x = driver_complexity, y = GR, fill = driver_complexity)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha = 0.6, size = 2) +
  scale_fill_manual(name = "Drivers", values = driver_colors_3) +
  labs(x = "Drivers", y = y_label) +
  shared_theme +
  ggtitle("Temperature targeted: 24\u00b0C")

ppH_combined <- ggplot(pH_combined,
                       aes(x = driver_complexity, y = GR, fill = driver_complexity)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha = 0.6, size = 2) +
  scale_fill_manual(name = "Drivers", values = driver_colors_3) +
  labs(x = "Drivers", y = y_label) +
  shared_theme +
  ggtitle("pH targeted: 7.7")

pLi_combined <- ggplot(Li_combined,
                       aes(x = driver_complexity, y = GR, fill = driver_complexity)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha = 0.6, size = 2) +
  scale_fill_manual(name = "Drivers", values = driver_colors_3) +
  labs(x = "Drivers", y = y_label) +
  shared_theme +
  ggtitle("Lithium targeted: 7 ppm")

# ============================================================
# 9. Combined plot
# ============================================================

png("plot_single_vs_one_vs_more.png", width = 7, height = 13, units = "in", res = 300)
plot_grid(ptemp_combined, ppH_combined, pLi_combined, ncol = 1, align = "v", axis = "lr")
dev.off()

cat("Plot saved to plot_single_vs_one_vs_more.png\n")






