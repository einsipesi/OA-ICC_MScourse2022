# ============================================================
# Consistency Analysis: Single vs Multiple Experiment
# ============================================================

library(readxl)
library(ggplot2)
library(ggpattern)
library(dplyr)
library(car)

# ============================================================
# 1. Load data
# ============================================================

single_raw <- read_excel("full_data_single_pHinsitu.xlsx") %>%
  rename(
    T_target  = T_targed,
    pH_target = pH_targed,
    Li_target = Li_targed
  ) %>%
  mutate(experiment = "single")


multi_raw <- read_excel("full_multiple.xlsx") %>%
  rename(
    T_target  = T_targed,
    pH_target = pH_targed,
    Li_target = Li_targed
  ) %>%
  mutate(experiment = "multi")

# ============================================================
# 2. Assign groups
# ============================================================

# Reference levels
REF_T  <- 20
REF_pH <- 8.1
REF_Li <- 0.18

# Single: label each treatment ## Here we choose target 7.45 because that treatment's
# real values ended up being 7.7, so they are most comparable to the multi treatment (7.67)
single_labelled <- single_raw %>%
  mutate(group = case_when(
    # Reference: all three at reference levels (T3, P1, L1 all share same targets)
    T_target == REF_T & pH_target == REF_pH & Li_target == REF_Li ~ "Reference",
    # Temperature: only T differs
    T_target == 24 & pH_target == REF_pH & Li_target == REF_Li ~ "Temperature",
    # pH: only pH differs
    T_target == REF_T & pH_target %in% c(7.45)  & Li_target == REF_Li ~ "pH",
    # Lithium: only Li differs
    T_target == REF_T & pH_target == REF_pH & Li_target == 7 ~ "Lithium",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(group))

# Multiple: only treatments where one stressor is out of reference (or none)
multi_labelled <- multi_raw %>%
  mutate(group = case_when(
    T_target == REF_T  & pH_target == REF_pH  & Li_target == REF_Li  ~ "Reference",
    T_target != REF_T  & pH_target == REF_pH  & Li_target == REF_Li  ~ "Temperature",
    T_target == REF_T  & pH_target != REF_pH  & Li_target == REF_Li  ~ "pH",
    T_target == REF_T  & pH_target == REF_pH  & Li_target != REF_Li  ~ "Lithium",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(group))

# ============================================================
# 3. Combine into one dataset
# ============================================================

all_data <- bind_rows(single_labelled, multi_labelled) %>%
  select(Treatment, GR, T_target, pH_target, Li_target, experiment, group) %>%
  mutate(
    experiment = factor(experiment, levels = c("single", "multi")),
    group      = factor(group, levels = c("Reference", "Temperature", "pH", "Lithium")),
    group_exp  = factor(
      paste(group, experiment, sep = "_"),
      levels = c(
        "Reference_single",   "Reference_multi",
        "Temperature_single", "Temperature_multi",
        "pH_single",          "pH_multi",
        "Lithium_single",     "Lithium_multi"
      )
    )
  )

# Quick check
cat("Observations per group and experiment:\n")
print(table(all_data$group, all_data$experiment))

# ============================================================
# 4. Plot
# ============================================================

fill_map <- c(
  "Reference_single"   = "white",
  "Reference_multi"    = "white",
  "Temperature_single" = "#E69F00",
  "Temperature_multi"  = "white",
  "pH_single"          = "#56B4E9",
  "pH_multi"           = "white",
  "Lithium_single"     = "#009E73",
  "Lithium_multi"      = "white"
)

y_label <- expression(paste("Growth rate ("*mu*"m"%.%"day"^-1*")"))

shared_theme <- theme_minimal(base_size = 14) +
  theme(
    axis.text        = element_text(size = 12),
    axis.title.y     = element_text(size = 14),
    legend.text      = element_text(size = 12),
    legend.title     = element_text(size = 14),
    legend.position  = "right"
  )

p <- ggplot(all_data, aes(x = group, y = GR,
                          fill = group_exp, pattern = experiment)) +
  geom_boxplot_pattern(
    position        = position_dodge(width = 0.8),
    width           = 0.6,
    pattern_fill    = "gray40",
    pattern_colour  = "gray40",
    pattern_density = 0.3,
    pattern_spacing = 0.03,
    colour          = "black"
  ) +
  geom_jitter(alpha = 0.3, size = 1.5, position = position_jitterdodge())+
  scale_fill_manual(values = fill_map, guide = "none") +
  scale_pattern_manual(
    values = c("single" = "none", "multi" = "stripe"),
    name   = "Experiment"
  ) +
  scale_x_discrete(labels = c(
    "Reference"   = "Reference\n(T=20, pH=8.1, Li=0.18)",
    "Temperature" = "Temperature\n(T=24)",
    "pH"          = "pH\n(pH=7.7)",
    "Lithium"     = "Lithium\n(Li=7 ppm)"
  )) +
  labs(x = NULL, y = y_label) +
  shared_theme

print(p)

ggsave("consistency_plot.png", plot = p, width = 9, height = 6, dpi = 300, bg = "white")
cat("Plot saved to consistency_plot.png\n")

# ============================================================
# 5. Statistical comparisons
# ============================================================

groups <- c("Reference", "Temperature", "pH", "Lithium")

for (g in groups) {
  
  cat("\n", strrep("=", 50), "\n")
  cat("GROUP:", g, "\n")
  cat(strrep("=", 50), "\n")
  
  df <- all_data %>% filter(group == g)
  
  cat("n single:", sum(df$experiment == "single"),
      "| n multi:", sum(df$experiment == "multi"), "\n\n")
  
  # --- Shapiro-Wilk ---
  cat("--- Normality (Shapiro-Wilk) ---\n")
  sw_results <- list()
  for (exp in c("single", "multi")) {
    vals <- df %>% filter(experiment == exp) %>% pull(GR)
    sw   <- shapiro.test(vals)
    sw_results[[exp]] <- sw$p.value
    cat(exp, ": W =", round(sw$statistic, 4),
        ", p =", round(sw$p.value, 4),
        ifelse(sw$p.value > 0.05, " => Normal\n", " => NOT normal\n"))
  }
  
  # --- Levene's test ---
  cat("\n--- Homogeneity of variance (Levene's test) ---\n")
  lev   <- leveneTest(GR ~ experiment, data = df)
  lev_p <- lev$`Pr(>F)`[1]
  cat("p =", round(lev_p, 4),
      ifelse(lev_p > 0.05, " => Homogeneous variance\n", " => Variance NOT homogeneous\n"))
  
  # --- Choose and run test ---
  both_normal <- all(unlist(sw_results) > 0.05)
  homogeneous <- lev_p > 0.05
  
  cat("\n--- Comparison test ---\n")
  if (both_normal & homogeneous) {
    cat("Both normal, homogeneous variance => Student's t-test\n")
    result <- t.test(GR ~ experiment, data = df, var.equal = TRUE)
  } else if (both_normal & !homogeneous) {
    cat("Both normal, heterogeneous variance => Welch's t-test\n")
    result <- t.test(GR ~ experiment, data = df, var.equal = FALSE)
  } else {
    cat("Normality violated => Wilcoxon rank-sum test\n")
    result <- wilcox.test(GR ~ experiment, data = df)
  }
  
  print(result)
}

