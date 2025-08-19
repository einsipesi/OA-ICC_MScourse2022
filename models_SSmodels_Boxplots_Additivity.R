# This code is from Natalija's 'Models2.html'. Annotations are therein. This is just to make figures. 



# Set global options
knitr::opts_chunk$set(echo = TRUE, warning = FALSE, message = FALSE)

# Load required libraries
library(readxl)   # For reading Excel files
library(ggplot2)  # For creating visualizations
library(mgcv)     # For Generalized Additive Models (GAM)
library(plyr)     # For data manipulation
library(dplyr)    # For modern data manipulation
library(purrr)    # For functional programming
library(car)      # For Levene's test
library(cowplot)


# Import the dataset from the specified file path
# Replace the file path with a relative path if needed for better portability  
#full_data_single <- read_excel("~/Library/CloudStorage/OneDrive-PublicAdministration/Documents/surt/BOAT/monaco_multiple_drivers/scripts_data/natalija_modelling/SS_with_relevant_MSdata.xlsx")
full_data_single <- read_excel("full_data_single_pHinsitu.xlsx")


# Rename the columns of the dataset for clarity and consistency
# - Treatment: Experimental treatment group
# - T_targeted: Target temperature for the experiment
# - pH_targeted: Target pH level for the experiment
# - Li_targeted: Target lithium concentration for the experiment
# - T_measured: Measured temperature during the experiment
# - pH_measured: Measured pH level during the experiment
# - Li_measured: Measured lithium concentration during the experiment
# - BL: Body length of the larvae
# - GR: Growth rate of the larvae
names(full_data_single) <- c("Treatment", "T_targeted", "pH_targeted", "Li_targeted", 
                             "T_measured", "pH_measured", "Li_measured", "BL", "GR")



# Subset data for temperature experiments
# Select rows 1 to 57 and 191 to 229, and relevant columns:
# - Column 1: Treatment
# - Column 2: Targeted temperature (T_targeted)
# - Column 5: Measured temperature (temp)
# - Column 9: Growth rate (rate)
temp_only <- full_data_single[c(134:190), c(1,2,5,9)]
names(temp_only) <- c("Treatment", "T_targeted", "temp", "rate")  # Rename columns for clarity
# Remove rows with any NA values in temp_only
temp_only <- na.omit(temp_only)

# Create the 'Experiment' column
#temp_only$Experiment <- ifelse(temp_only$Treatment %in% c("T1", "T2", "T3", "T4", "T5"), "Single", "Multiple")


# Subset data for pH experiments
# Select rows 58 to 109, 191 to 209, and 230 to 244, and relevant columns:
# - Column 1: Treatment
# - Column 3: Targeted pH (pH_targeted)
# - Column 6: Measured pH (pH)
# - Column 9: Growth rate (rate)
pH_only <- full_data_single[c(82:133), c(1,3,6,9)]
names(pH_only) <- c("Treatment", "pH_targeted", "pH", "rate")  # Rename columns for clarity
# Remove rows with any NA values in pH_only
pH_only <- na.omit(pH_only)

# Create the 'Experiment' column
#pH_only$Experiment <- ifelse(pH_only$Treatment %in% c("P1", "P2", "P3", "P4", "P5"), "Single", "Multiple")



# Subset data for lithium experiments
# Select rows 110 to 190, 191 to 209, and 245 to 262, and relevant columns:
# - Column 1: Treatment
# - Column 4: Targeted lithium concentration (Li_targeted)
# - Column 7: Measured lithium concentration (lithium)
# - Column 9: Growth rate (rate)
lithium_only <- full_data_single[c(1:81), c(1,4,7,9)]
names(lithium_only) <- c("Treatment", "Li_targeted", "lithium", "rate")  # Rename columns for clarity
# Remove rows with any NA values in lithium_only
lithium_only <- na.omit(lithium_only)


# Create the 'Experiment' column
#lithium_only$Experiment <- ifelse(lithium_only$Treatment %in% c("L1", "L2", "L3", "L4", "L5"), "Single", "Multiple")


# Visualize the data distribution with dot charts
# Dot charts help visualize the spread of measured values and growth rates for each stressor

# Temperature experiments: Measured temperature and growth rate
dotchart(temp_only$temp, main = "Measured Temperature (°C)", xlab = "Temperature")

dotchart(temp_only$rate, main = "Growth Rate (Temperature Experiments)", xlab = "Growth Rate")

# pH experiments: Measured pH and growth rate
dotchart(pH_only$pH, main = "Measured pH", xlab = "pH")

dotchart(pH_only$rate, main = "Growth Rate (pH Experiments)", xlab = "Growth Rate")

# Lithium experiments: Measured lithium concentration and growth rate
dotchart(lithium_only$lithium, main = "Measured Lithium Concentration (ppm)", xlab = "Lithium")

dotchart(lithium_only$rate, main = "Growth Rate (Lithium Experiments)", xlab = "Growth Rate")


# Fit a simple linear model to temperature data (reference model)
LM_temp <- lm(rate ~ temp, data = temp_only)
summary(LM_temp)  # Check the summary for model performance

# Fit a polynomial model (degree 2) to capture the bell-shaped curve
P2_temp <- lm(rate ~ poly(temp, 2), data = na.omit(temp_only))
summary(P2_temp)  # Check the performance of the polynomial model

# Fit a Generalized Additive Model (GAM) for comparison
GAM_temp <- gam(rate ~ s(temp, k = 1), family = Gamma(link = "log"), data = temp_only)
summary(GAM_temp)

# Compare model performance using AIC (Akaike Information Criterion)
AIC(LM_temp, P2_temp, GAM_temp)


# For polynomial model:

# Check homogeneity by extracting residuals and fitted values
E2 <- rstandard(P2_temp)  # Standardized residuals
F2 <- fitted(P2_temp)     # Fitted values

# Plot residuals vs fitted values to check for patterns
par(mfrow = c(1, 1), mar = c(5, 5, 2, 2), cex.lab = 1.5)
plot(F2, E2, xlab = "Fitted values", ylab = "Residuals", main = "Residuals vs Fitted")
abline(h = 0, col = "red", lty = 2)

# Check normality of residuals using a histogram
hist(E2, main = "Histogram of Residuals", xlab = "Residuals", col = "lightblue", border = "black")

# Check for influential observations using Cook's Distance
par(mfrow = c(1, 1), mar = c(5, 5, 2, 2), cex.lab = 1.5)
plot(cooks.distance(P2_temp), type = "h", ylim = c(0, 1), main = "Cook's Distance")
abline(h = 1, col = "red", lty = 2)

# Check for independence by plotting residuals against temperature
temp_only$E2 <- E2
plot(temp_only$temp, temp_only$E2, main = "Residuals vs Temperature", xlab = "Temperature", ylab = "Residuals")
abline(h = 0, col = "red", lty = 2)

# Cross-Validation

MyData <- expand.grid(temp = seq(min(temp_only$temp) - 5, max(temp_only$temp) + 5, length = 25))

# Predict growth rate and calculate confidence intervals for new data
P2 <- predict(P2_temp, newdata = MyData, se = TRUE)
MyData$Pred <- P2$fit                     # Predicted values
MyData$selo <- P2$fit - 1.96 * P2$se.fit  # Lower bound of 95% CI
MyData$seup <- P2$fit + 1.96 * P2$se.fit  # Upper bound of 95% CI






p_temperature <- ggplot() +
  geom_point(data = temp_only, 
             aes(x = temp, y = rate, color = "Observations"), 
             shape = 16, size = 3) +
  geom_line(data = MyData, 
            aes(x = temp, y = Pred, color = "Model Prediction"), 
            linetype = "solid") +
  geom_ribbon(data = MyData, 
              aes(x = temp, ymin = selo, ymax = seup, fill = "Model Confidence Interval"), 
              alpha = 0.3) +
  xlab(expression(paste("Temperature ("*degree*"C)"))) +
  ylab(expression(paste("Growth rate ("*mu*"m"%.%"day"^-1*")"))) +
  theme_bw() +
  theme(
    text = element_text(size = 15),         # base size
    axis.title = element_text(size = 14),   # x and y axis titles
    axis.text = element_text(size = 13),    # tick labels
    legend.title = element_text(size = 14), # legend title
    legend.text = element_text(size = 12),  # legend entries
    strip.text = element_text(size = 13),    # facet labels (if used)
    legend.position = "none",
    ) +
  scale_color_manual(name = "Legend", 
                     values = c("Observations" = "#000000", 
                                "Model Prediction" = "#0072B2")) +
  scale_fill_manual(name = "Legend", 
                    values = c("Model Confidence Interval" = "#56B4E9"))

p_temperature



### pH ###

LM_pH<- lm(rate~pH, data=pH_only)# start with the simple linear for the reference
summary(LM_pH)

#Step 2: Fit a Second-Degree Polynomial Model
P2_pH<- lm(rate~poly(pH,2), data=na.omit(pH_only))# quadratic function accoeding to the literature
summary(P2_pH)

#Step 3: Fit a Generalized Additive Model (GAM)
gam_pH<- gam(rate~s(pH,k=1), family='gaussian', data=pH_only)# quadratic function accoeding to the literature
summary(gam_pH)

AIC(LM_pH, P2_pH, gam_pH)

#Cross validation for pH

MyData <- expand.grid(pH = seq(min(pH_only$pH-0.3), 
                               max(pH_only$pH),
                               length = 25),
                      rate = seq(min(pH_only$rate), 
                                 max(pH_only$rate),
                                 length = 25))
L2 <- predict(P2_pH, newdata = MyData, se = TRUE)

# 3. Calculate a measure of uncertainty for these predicted values.
#    Say a 95% confidence interval.
MyData$Pred <- L2$fit                    #Predicted values
MyData$selo <- L2$fit - 1.96 * P2$se.fit #lower bound
MyData$seup <- L2$fit + 1.96 * P2$se.fit #lower bound
head(MyData)



p_pH <- ggplot() +
  geom_point(data = pH_only, 
             aes(y = rate, x = pH, color = "Observations"),
             shape = 16, 
             size = 3) +
  geom_line(data = MyData, 
            aes(x = pH, y = Pred, color = "Model Prediction"), 
            linetype = "solid") +
  geom_ribbon(data = MyData, 
              aes(x = pH, ymax = seup, ymin = selo, fill = "Model Confidence Interval"),
              alpha = 0.3) +
  xlab("pH") +
  ylab(expression(paste("Growth rate ("*mu*"m"%.%"day"^-1*")"))) +
  theme_bw() +
  theme(
    text = element_text(size = 15),         # base size
    axis.title = element_text(size = 14),   # x and y axis titles
    axis.text = element_text(size = 13),    # tick labels
    legend.title = element_text(size = 14), # legend title
    legend.text = element_text(size = 12),  # legend entries
    strip.text = element_text(size = 13),    # facet labels (if used)
    legend.position = "none",
  ) +
  scale_color_manual(name = "Legend", 
                     values = c("Observations" = "#0072B2", "Model Prediction" = "#CC79A7")) +  # Colorblind-friendly palette
  scale_fill_manual(name = "Legend", 
                    values = c("Model Confidence Interval" = "#D4B9DA"))  # Colorblind-friendly palette

p_pH


### Lithium

#Step 1: Fit a Linear Model
LM_Li<-lm(rate~lithium, data=lithium_only)
summary(LM_Li)

#Step 2: Fit a Second-Degree Polynomial Model
P2_Li<-lm(rate~poly(lithium, 2), data=lithium_only)
summary(P2_Li)

#Step 3: Fit a Generalized Additive Model (GAM)
gam_Li<-gam(rate~s(lithium, k=5), data=lithium_only)
summary(gam_Li)

# Cross validation for lithium

MyData <- expand.grid(lithium = seq(min(lithium_only$lithium), 
                                    max(lithium_only$lithium+5),
                                    length = 25),
                      rate = seq(min(lithium_only$rate), 
                                 max(lithium_only$rate),
                                 length = 25))

L3 <- predict(P2_Li, newdata = MyData, se = TRUE)

# 3. Calculate a measure of uncertainty for these predicted values.
#    Say a 95% confidence interval.
MyData$Pred <- L3$fit                    #Predicted values
MyData$selo <- L3$fit - 1.96 * P2$se.fit #lower bound
MyData$seup <- L3$fit + 1.96 * P2$se.fit #lower bound
head(MyData)


p_Li <- ggplot() +
  geom_point(data = lithium_only, 
             aes(y = rate, x = lithium, color = "Observations"),
             shape = 16, 
             size = 3) +
  geom_line(data = MyData, 
            aes(x = lithium, y = Pred, color = "Model Prediction"), 
            linetype = "solid") +
  geom_ribbon(data = MyData, 
              aes(x = lithium, ymax = seup, ymin = selo, fill = "Model Confidence Interval"),
              alpha = 0.3) +
  xlab("Lithium concentration (ppm)") +
  ylab(expression(paste("Growth rate ("*mu*"m"%.%"day"^-1*")"))) +
  theme_bw() +
  theme(
    text = element_text(size = 15),         # base size
    axis.title = element_text(size = 14),   # x and y axis titles
    axis.text = element_text(size = 13),    # tick labels
    legend.title = element_text(size = 14), # legend title
    legend.text = element_text(size = 12),  # legend entries
    strip.text = element_text(size = 13),    # facet labels (if used)
    legend.position = "none",
  ) +
  scale_color_manual(name = "Legend", 
                     values = c("Observations" = "#E69F00", "Model Prediction" = "#56B4E9")) +  # Colorblind-friendly palette
  scale_fill_manual(name = "Legend", 
                    values = c("Model Confidence Interval" = "#A6DCE5"))  # Colorblind-friendly palette

p_Li 


### Combine all into one plot ###

png("plot_SSmodels.png", width = 5.5, height = 13, units = "in", res = 300)
plot_grid(p_temperature, p_pH, p_Li, ncol = 1, align = "v", axis = "lr")
dev.off()



add_split_factor <- function(data, fraction_model = 2/3) {
  data %>%
    group_split(Treatment) %>%
    map_dfr(~ {
      n_rows <- nrow(.x)
      # Calculate number of rows for 'model' and 'validation'
      n_model <- floor(fraction_model * n_rows)
      n_validation <- n_rows - n_model
      # Shuffle indices
      shuffled_indices <- sample(seq_len(n_rows))
      # Assign 'model' and 'validation' labels
      .x$split_factor <- 'validation'  # Default all rows as 'validation'
      .x$split_factor[shuffled_indices[1:n_model]] <- 'model'  # First n_model rows as 'model'
      return(.x)
    })
}

# Add the split_factor column
full_data_single1 <- add_split_factor(full_data_single, fraction_model = 2/3)

table_t<-subset(full_data_single1, split_factor=="model")
table_v<-subset(full_data_single1, split_factor=="validation")


M_add<- lm(GR~poly(T_measured,3)+poly(pH_measured,3)+poly(Li_measured,2), data = table_t)
summary(M_add)

M_add_gam<- gam(GR~s(T_measured,k=3)+s(pH_measured,k=3)+s(Li_measured,k=2), family = Gamma(link = "log"),data = table_t)
summary(M_add_gam)


# Homogeneity
E2 <- rstandard(M_add)
F2 <- fitted(M_add)

#Plot residuals vs fitted values
par(mfrow = c(1,1), mar = c(5,5,2,2), cex.lab = 1.5)
plot(x = F2, 
     y = E2,
     xlab = "Fitted values",
     ylab = "Residuals")
abline(h = 0, v = 0)


# Normality
hist(E2)

# Influential observations
plot(cooks.distance(M_add), type = "h", ylim = c(0, 1))
abline(h = 1)

# Independence due to model misfit
table_t$E2 <- E2   #Put E2 inside the dataset
plot(E2~T_measured, data=table_t)
abline(h = 0)

plot(E2~pH_measured, data=table_t)
abline(h = 0)


plot(E2~Li_measured, data=table_t)
abline(h = 0)

P2 <- predict(M_add, newdata = table_v, se = TRUE)

# 3. Calculate a measure of uncertainty for these predicted values.
#    Say a 95% confidence interval.
table_v$Pred <- P2$fit                    #Predicted values
table_v$selo <- P2$fit - 1.96 * P2$se.fit #lower bound
table_v$seup <- P2$fit + 1.96 * P2$se.fit #lower bound


head(table_v)

table_v<-table_v%>%arrange(Treatment)

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}
table_v$Treatment_ID_gr<-substrRight(table_v$Treatment, 1)
substrLeft <- function(x, n){
  substr(x, 1, 1)
}
table_v$Treatment_ID<-substrLeft(table_v$Treatment)
table_v$Treatment_ID<-as.factor(table_v$Treatment_ID)
table_v$Treatment_ID_gr<-as.factor(table_v$Treatment_ID_gr)

trend2<-data.frame(Pred=seq(from = min(table_v$Pred)-10, 
                            to = max(table_v$Pred)+10, 
                            length = 100))
trend2$GR<-trend2$Pred

out2<-summary(M_add)

#png("plot.png", width = 8, height = 6, units = "in", res = 300)
p_cvss<- ggplot() +
  geom_point(data = table_v, 
             aes(y = Pred, x = GR, 
                 shape = Treatment_ID_gr, 
                 color = Treatment_ID),
             size = 3) +
  scale_shape_manual(name = "Treatment level", 
                     values = c(4, 6, 8, 1, 0)) +
  scale_color_manual(name = "Treatment", 
                     values = c("L" = "#1B9E77", 
                                "P" = "#D95F02", 
                                "T" = "#7570B3"),
                     labels = c("L" = "Lithium", 
                                "P" = "pH", 
                                "T" = "Temperature")) +
  xlab("GR measured") + 
  ylab("GR estimated") +
  theme_bw() +
  theme(text = element_text(size = 16),  # larger text
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 17)) +
  guides(color = guide_legend(order = 1),  # Treatment on top
         shape = guide_legend(order = 2)) + # Treatment Level below
  geom_line(data = trend2, 
            aes(x = GR, y = Pred), color = "black") +
  geom_ribbon(data = trend2, 
              aes(x = GR, 
                  ymax = Pred + (1.96 * out2$sigma), 
                  ymin = Pred - (1.96 * out2$sigma)),
              alpha = 0.2, fill = "lightgrey")
#dev.off()


# Addition of data from multiple stressor experiment

full_multiple <- read_excel("full_multiple.xlsx")

P_multi <- predict(M_add, newdata = full_multiple, se = TRUE)

# 3. Calculate a measure of uncertainty for these predicted values.
#    Say a 95% confidence interval.
full_multiple$Pred <- P_multi$fit                    #Predicted values
full_multiple$selo <- P_multi$fit - 1.96 * P_multi$se.fit #lower bound
full_multiple$seup <- P_multi$fit + 1.96 * P_multi$se.fit #lower bound
head(full_multiple)

full_multiple<-full_multiple%>%arrange(Treatment)

trend3<-data.frame(Pred=seq(from = min(full_multiple$Pred)-10, 
                            to = max(full_multiple$Pred)+10, 
                            length = 100))
trend3$GR<-trend3$Pred

out2<-summary(M_add)

full_multiple$Treatment_T_gr<-as.factor(full_multiple$Treatment_T_gr)
full_multiple$`Treatment_pH&Li_gr`<-as.factor(full_multiple$`Treatment_pH&Li_gr`)


#png("plot2.png", width = 8, height = 6, units = "in", res = 300)
p_cvms <- ggplot() +
  geom_point(data = full_multiple, 
             aes(y = Pred, x = GR, shape = Treatment_T_gr, color = `Treatment_pH&Li_gr`),
             size = 3) +
  scale_shape_manual(name='Temperature',
                     values = c(13, 16)) +
  scale_color_manual(name='Lithium / pH levels',
                     values = c("#0072B2", "#CC79A7", "#009E73", "#E69F00")) +
  xlab("GR measured") + 
  ylab("GR estimated") +
  theme(text = element_text(size = 15)) +
  geom_line(data = trend3, 
            aes(x = GR, y = Pred), color = "black") +
  geom_ribbon(data = trend3, 
              aes(x = GR, ymax = Pred + (1.96 * out2$sigma), 
                  ymin = Pred - (1.96 * out2$sigma)),
              alpha = 0.2, fill = "lightgrey") +
  theme_bw()+
  theme(text = element_text(size = 16),  # larger text
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 17)) +
  guides(color = guide_legend(order = 1),  # Treatment on top
         shape = guide_legend(order = 2)) + # Treatment Level below
  geom_line(data = trend2, 
            aes(x = GR, y = Pred), color = "black") +
  geom_ribbon(data = trend2, 
              aes(x = GR, 
                  ymax = Pred + (1.96 * out2$sigma), 
                  ymin = Pred - (1.96 * out2$sigma)),
              alpha = 0.2, fill = "lightgrey")

#dev.off()



# Both plots together:

library(cowplot)



png("plotCV_comparison.png", width = 8, height = 10, units = "in", res = 300)
plot_grid(p_cvss, p_cvms, 
          ncol = 1,       # One column = vertical layout
          align = "v",    # Align vertically
          axis = "lr")    # Align left and right axesdev.off()
dev.off()



Li_case<-lithium_only[,1:4]
MS_Li_only<-full_multiple[,c(1,9, 10,4)]
names(Li_case)<-c("Treatment" ,"Li_targeted", "Li_measured","GR")
names(MS_Li_only)<-c("Treatment" ,"Li_targeted", "Li_measured","GR")
MS_Li_only$Stressors<-rep("Multiple",nrow(MS_Li_only))
Li_case$Stressors<-rep("Single",nrow(Li_case))
Li_case_full<-rbind(Li_case,MS_Li_only)


p <- ggplot() +
  geom_point(data = Li_case_full, 
             aes(y = GR, x = Li_measured, fill = Stressors),
             shape = 21, 
             size = 2) +
  xlab("Lithium concentration (ppm)") +
  ylab(expression(paste("Growth rate ("*mu*"m"%.%"day"^-1*")"))) +
  theme_bw() +
  theme(text = element_text(size = 15)) +
  scale_fill_manual(name = "Stressors", 
                    values = c("Single" = "#E69F00", "Multiple" = "#56B4E9"))

p


Li_case_for_stat<-subset(Li_case_full, Li_targeted==0.18|Li_targeted==7)
Li_case_for_stat$Treatment<-as.factor(Li_case_for_stat$Treatment)
# Ensure GR is numeric
Li_case_for_stat$GR <- as.numeric(Li_case_for_stat$GR)

# Subset data by Li_targeted levels
Li_case_for_stat_018<- subset(Li_case_for_stat, Li_targeted == 0.18)
Li_case_for_stat_7 <- subset(Li_case_for_stat, Li_targeted == 7)

# Shapiro-Wilk Test
shapiro_results <- Li_case_for_stat %>%
  group_by(Treatment) %>%
  summarise(shapiro_p_value = shapiro.test(GR)$p.value)

cat("\nShapiro-Wilk Test Results:\n")

print(as.data.frame(shapiro_results))  # Convert tibble to data frame

# Levene's Test
levene_test_018 <- leveneTest(GR ~ Stressors, data = Li_case_for_stat_018)
levene_test_7 <- leveneTest(GR ~ Stressors, data = Li_case_for_stat_7)

levene_results_table <- data.frame(
  Group = c("Li targeted = 0.18 ppm", "Li targeted = 7 ppm"),
  F_value = c(levene_test_018$`F value`[1], levene_test_7$`F value`[1]),
  P_value = c(levene_test_018$`Pr(>F)`[1], levene_test_7$`Pr(>F)`[1])
)


cat("\nLevene's Test Results:\n")

print(levene_results_table)


# Wilcoxon rank-sum tests
Li_case_for_stat_018_s<-subset(Li_case_for_stat_018, Stressors=="Single")
Li_case_for_stat_018_m<-subset(Li_case_for_stat_018, Stressors=="Multiple")

Li_case_for_stat_7_s<-subset(Li_case_for_stat_7, Stressors=="Single")
Li_case_for_stat_7_m<-subset(Li_case_for_stat_7, Stressors=="Multiple")

wilk_018<-wilcox.test(Li_case_for_stat_018_s$GR,Li_case_for_stat_018_m$GR)
wilk_7<-wilcox.test(Li_case_for_stat_7_s$GR,Li_case_for_stat_7_m$GR)


wilk_results_table <- data.frame(
  Group = c("Li targeted = 0.18 ppm", "Li targeted = 7 ppm"),
  W_value = c(wilk_018$statistic, wilk_7$statistic),
  P_value = c(wilk_018$p.value, wilk_7$p.value)
)

cat("\nWilcoxon Rank-Sum Test Results:\n")

print(wilk_results_table)

# ANOVA

anova_18<-aov(GR~Stressors, data=Li_case_for_stat_018)
anova_7<-aov(GR~Stressors, data=Li_case_for_stat_7)
s_anova_18<-summary(anova_18)
s_anova_7<-summary(anova_7)

anova_results_table <- data.frame(
  Group = c("Li targeted = 0.18 ppm", "Li targeted = 7 ppm"),
  F_value = c(summary(anova_18)[[1]]$`F value`[1], summary(anova_7)[[1]]$`F value`[1]),
  P_value = c(summary(anova_18)[[1]]$`Pr(>F)`[1], summary(anova_7)[[1]]$`Pr(>F)`[1])
)

cat("\nANOVA Results:\n")

print(anova_results_table)


Li_case_for_stat$Li_targeted_label <- factor(
  Li_case_for_stat$Li_targeted,
  levels = c(0.18, 7.00),
  labels = c("Li targeted: 0.18 ppm", "Li targeted: 7 ppm")
)

pLi <- ggplot(Li_case_for_stat, aes(x = Stressors, y = GR, fill = Stressors)) +
  geom_boxplot() +
  facet_grid(~Li_targeted_label) +
  labs(
    x = "Drivers",
    y = expression(paste("Growth rate ("*mu*"m"%.%"day"^-1*")"))
  ) +
  theme_bw() +
  theme(
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 16),
    strip.text = element_text(size = 15)
  ) +
  scale_fill_manual(
    name = "Drivers",
    values = c("Single" = "#E69F00", "Multiple" = "#56B4E9")
  )

pLi


temp_case<-temp_only[,1:4]
MS_temp_only<-full_multiple[,c(1,5,6,4)]
names(temp_case)<-c("Treatment" ,"T_targeted", "T_measured","GR")
names(MS_temp_only)<-c("Treatment" ,"T_targeted", "T_measured","GR")
MS_temp_only$Stressors<-rep("Multiple",nrow(MS_temp_only))
temp_case$Stressors<-rep("Single",nrow(temp_case))
temp_case_full<-rbind(temp_case,MS_temp_only)

p <- ggplot() +
  geom_point(data = temp_case_full, 
             aes(y = GR, x = T_measured, fill = Stressors),
             shape = 21, 
             size = 2) +
  xlab("Temperature (°C)") +
  ylab(expression(paste("Growth rate ("*mu*"m"%.%"day"^-1*")"))) +
  theme_bw() +
  theme(text = element_text(size = 15)) +
  scale_fill_manual(name = "Stressors", 
                    values = c("Single" = "#D55E00", "Multiple" = "#009E73"))

p


temp_case_for_stat<-subset(temp_case_full, T_targeted==20|T_targeted==24)
temp_case_for_stat$Treatment<-as.factor(temp_case_for_stat$Treatment)
# Ensure GR is numeric
temp_case_for_stat$GR <- as.numeric(temp_case_for_stat$GR)

# Subset data by Li_targeted levels
temp_case_for_stat_20<- subset(temp_case_for_stat, T_targeted == 20)
temp_case_for_stat_24 <- subset(temp_case_for_stat, T_targeted == 24)

# Shapiro-Wilk Test
shapiro_results <- temp_case_for_stat %>%
  group_by(Treatment) %>%
  summarise(shapiro_p_value = shapiro.test(GR)$p.value)

cat("\nShapiro-Wilk Test Results:\n")

print(as.data.frame(shapiro_results))  # Convert tibble to data frame

# Levene's Test
levene_test_20 <- leveneTest(GR ~ Stressors, data = temp_case_for_stat_20)
levene_test_24 <- leveneTest(GR ~ Stressors, data = temp_case_for_stat_24)

levene_results_table <- data.frame(
  Group = c("T targeted: 20 °C", "T targeted: 24 °C"),
  F_value = c(levene_test_20$`F value`[1], levene_test_24$`F value`[1]),
  P_value = c(levene_test_20$`Pr(>F)`[1], levene_test_24$`Pr(>F)`[1])
)


cat("\nLevene's Test Results:\n")

print(levene_results_table)

# Wilcoxon rank-sum tests
temp_case_for_stat_20_s<-subset(temp_case_for_stat_20, Stressors=="Single")
temp_case_for_stat_20_m<-subset(temp_case_for_stat_20, Stressors=="Multiple")

temp_case_for_stat_24_s<-subset(temp_case_for_stat_24, Stressors=="Single")
temp_case_for_stat_24_m<-subset(temp_case_for_stat_24, Stressors=="Multiple")

wilk_20<-wilcox.test(temp_case_for_stat_20_s$GR,temp_case_for_stat_20_m$GR)
wilk_24<-wilcox.test(temp_case_for_stat_24_s$GR,temp_case_for_stat_24_m$GR)


wilk_results_table <- data.frame(
  Group = c("T targeted: 20 °C", "T targeted: 24 °C"),
  W_value = c(wilk_20$statistic, wilk_24$statistic),
  P_value = c(wilk_20$p.value, wilk_24$p.value)
)

cat("\nWilcoxon Rank-Sum Test Results:\n")

print(wilk_results_table)

# ANOVA

anova_20<-aov(GR~Stressors, data=temp_case_for_stat_20)
anova_24<-aov(GR~Stressors, data=temp_case_for_stat_24)
s_anova_20<-summary(anova_20)
s_anova_24<-summary(anova_24)

anova_results_table <- data.frame(
  Group = c("T targeted: 20 °C", "T targeted: 24 °C"),
  F_value = c(summary(anova_20)[[1]]$`F value`[1], summary(anova_20)[[1]]$`F value`[1]),
  P_value = c(summary(anova_24)[[1]]$`Pr(>F)`[1], summary(anova_24)[[1]]$`Pr(>F)`[1])
)

cat("\nANOVA Results:\n")

print(anova_results_table)

temp_case_for_stat$temp_targeted_label <- factor(
  temp_case_for_stat$T_targeted,
  levels = c(20, 24),
  labels = c("T targeted: 20 °C", "T targeted: 24 °C")
)

ptemp <- ggplot(temp_case_for_stat, aes(x = Stressors, y = GR, fill = Stressors)) +
  geom_boxplot() +
  facet_grid(~temp_targeted_label) +
  labs(
    x = "Drivers",
    y = expression(paste("Growth rate ("*mu*"m"%.%"day"^-1*")"))
  ) +
  theme_bw() +
  theme(axis.text = element_text(size = 12)) +
  theme(
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 16),
    strip.text = element_text(size = 15)
  ) +
  scale_fill_manual(
    name = "Drivers", 
                    values = c("Single" = "#D55E00", "Multiple" = "#009E73"))

ptemp


pH_case<-pH_only[,1:4]
MS_pH_only<-full_multiple[,c(1,7,8,4)]
names(pH_case)<-c("Treatment" ,"pH_targeted", "pH_measured","GR")
names(MS_pH_only)<-c("Treatment" ,"pH_targeted", "pH_measured","GR")
MS_pH_only$Stressors<-rep("Multiple",nrow(MS_pH_only))
pH_case$Stressors<-rep("Single",nrow(pH_case))
pH_case_full<-rbind(pH_case,MS_pH_only)

p <- ggplot() +
  geom_point(data = pH_case_full, 
             aes(y = GR, x = pH_measured, fill = Stressors),
             shape = 21, 
             size = 2) +
  xlab("pH") +
  ylab(expression(paste("Growth rate ("*mu*"m"%.%"day"^-1*")"))) +
  theme_bw() +
  theme(text = element_text(size = 15)) +
  scale_fill_manual(name = "Stressors", 
                    values = c("Single" = "#0072B2", "Multiple" = "#CC79A7"))

p

pH_case_for_stat<-subset(pH_case_full, pH_targeted==7.7|pH_targeted==8.1)
pH_case_for_stat$Treatment<-as.factor(pH_case_for_stat$Treatment)
# Ensure GR is numeric
pH_case_for_stat$GR <- as.numeric(pH_case_for_stat$GR)

# Subset data by Li_targeted levels
pH_case_for_stat_77<- subset(pH_case_for_stat, pH_targeted == 7.7)
pH_case_for_stat_81 <- subset(pH_case_for_stat, pH_targeted == 8.1)

# Shapiro-Wilk Test
shapiro_results <- pH_case_for_stat %>%
  group_by(Treatment) %>%
  summarise(shapiro_p_value = shapiro.test(GR)$p.value)

cat("\nShapiro-Wilk Test Results:\n")

print(as.data.frame(shapiro_results))  # Convert tibble to data frame

# Levene's Test
levene_test_77 <- leveneTest(GR ~ Stressors, data = pH_case_for_stat_77)
levene_test_81 <- leveneTest(GR ~ Stressors, data = pH_case_for_stat_81)

levene_results_table <- data.frame(
  Group = c("pH targeted: 7.7", "pH targeted: 8.1"),
  F_value = c(levene_test_77$`F value`[1], levene_test_81$`F value`[1]),
  P_value = c(levene_test_77$`Pr(>F)`[1], levene_test_81$`Pr(>F)`[1])
)


cat("\nLevene's Test Results:\n")

print(levene_results_table)

# Wilcoxon rank-sum tests
pH_case_for_stat_77_s<-subset(pH_case_for_stat_77, Stressors=="Single")
pH_case_for_stat_77_m<-subset(pH_case_for_stat_77, Stressors=="Multiple")

pH_case_for_stat_81_s<-subset(pH_case_for_stat_81, Stressors=="Single")
pH_case_for_stat_81_m<-subset(pH_case_for_stat_81, Stressors=="Multiple")

wilk_77<-wilcox.test(pH_case_for_stat_77_s$GR,pH_case_for_stat_77_m$GR)
wilk_81<-wilcox.test(pH_case_for_stat_81_s$GR,pH_case_for_stat_81_m$GR)


wilk_results_table <- data.frame(
  Group = c("pH targeted: 7.7", "pH targeted: 8.1"),
  W_value = c(wilk_77$statistic, wilk_81$statistic),
  P_value = c(wilk_77$p.value, wilk_81$p.value)
)

cat("\nWilcoxon Rank-Sum Test Results:\n")

print(wilk_results_table)

# ANOVA

anova_77<-aov(GR~Stressors, data=pH_case_for_stat_77)
anova_81<-aov(GR~Stressors, data=pH_case_for_stat_81)
s_anova_77<-summary(anova_77)
s_anova_81<-summary(anova_81)

anova_results_table <- data.frame(
  Group = c("pH targeted: 7.7", "pH targeted: 8.1"),
  F_value = c(summary(anova_77)[[1]]$`F value`[1], summary(anova_77)[[1]]$`F value`[1]),
  P_value = c(summary(anova_81)[[1]]$`Pr(>F)`[1], summary(anova_81)[[1]]$`Pr(>F)`[1])
)

cat("\nANOVA Results:\n")

print(anova_results_table)

pH_case_for_stat$pH_targeted_label <- factor(
  pH_case_for_stat$pH_targeted,
  levels = c(7.7,8.1),
  labels = c("pH targeted: 7.7", "pH targeted: 8.1")
)

ppH <- ggplot(pH_case_for_stat, aes(x = Stressors, y = GR, fill = Stressors)) +
  geom_boxplot() +
  facet_grid(~pH_targeted_label) +
  labs(
    x = "Drivers",
    y = expression(paste("Growth rate ("*mu*"m"%.%"day"^-1*")"))
  ) +
  theme_bw() +
  theme(axis.text = element_text(size = 12)) +
  theme(
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 16),
    strip.text = element_text(size = 15)
  ) +
  scale_fill_manual(
    name = "Drivers", 
                    values = c("Single" = "#0072B2", "Multiple" = "#CC79A7"))

ppH



#png("plot_comparison.png", width = 7, height = 13, units = "in", res = 300)
plot_grid(ptemp, ppH, pLi, ncol = 1, align = "v", axis = "lr")
#dev.off()

