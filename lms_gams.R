## ============================================================
## Model performance via 10x10 repeated CV (RMSE & classical R²)
## ============================================================

## ---- packages ----
library(readxl)
library(caTools)    # sample.split
library(dplyr)
library(purrr)
library(tidyr)
library(mgcv)
library(caret)      # for createMultiFolds convenience

## ============================================================
## 1. Load data & create 80/20 split (hold-out reserved; CV uses train_set)
## ============================================================
d <- read_excel("ms_ss_bathtemp.xlsx") #%>%
  #mutate(across(everything(), as.numeric))

set.seed(123)
split <- sample.split(d$rate, SplitRatio = 0.80)
train_set <- d[split, ]
test_set  <- d[!split, ]  # not used in CV metrics below; keep if you want final external test

## ============================================================
## 2. Centre/scale predictors on *training* data only
## ============================================================
ctr <- train_set %>%
  summarise(
    m_temp = mean(temp),  s_temp = sd(temp),
    m_pHts = mean(pHts),  s_pHts = sd(pHts),
    m_Li   = mean(Li),    s_Li   = sd(Li)
  )

scale_with <- function(x, m, s) (x - m) / s

train_set <- train_set %>%
  mutate(
    ztemp = scale_with(temp, ctr$m_temp, ctr$s_temp),
    zpHts = scale_with(pHts, ctr$m_pHts, ctr$s_pHts),
    zLi   = scale_with(Li,   ctr$m_Li,   ctr$s_Li)
  )

test_set <- test_set %>%
  mutate(
    ztemp = scale_with(temp, ctr$m_temp, ctr$s_temp),
    zpHts = scale_with(pHts, ctr$m_pHts, ctr$s_pHts),
    zLi   = scale_with(Li,   ctr$m_Li,   ctr$s_Li)
  )

## Add quadratic terms used in polynomial models
train_set <- train_set %>% mutate(ztemp2 = ztemp^2, zpHts2 = zpHts^2, zLi2 = zLi^2)
test_set  <- test_set  %>% mutate(ztemp2 = ztemp^2, zpHts2 = zpHts^2, zLi2 = zLi^2)

## ============================================================
## 3. AICc helper (works for lm & gam fits)
## ============================================================
AICc_generic <- function(fit) {
  aic <- AIC(fit)
  k   <- length(coef(fit))
  n   <- nobs(fit)
  aic + (2 * k * (k + 1)) / (n - k - 1)
}

## ============================================================
## 4. Cross-validation fold index set (10 folds × 10 repeats)
##    caret::createMultiFolds returns training indices for each split.
##    We'll use the complement as the validation fold.
## ============================================================
set.seed(42)
fold_list <- createMultiFolds(y = train_set$rate, k = 10, times = 10)
# fold_list is a named list: each element = training indices for that resample.

## Utility: out-of-fold predictions for a given model
cv_out_of_fold <- function(formula, data, fit_type = c("lm","gam")) {
  fit_type <- match.arg(fit_type)
  n <- nrow(data)
  pred_vec <- rep(NA_real_, n)
  
  for (idx in fold_list) {
    tr_idx <- idx
    te_idx <- setdiff(seq_len(n), tr_idx)
    if (fit_type == "lm") {
      fit <- lm(formula, data = data[tr_idx, , drop = FALSE])
    } else {
      fit <- gam(formula, data = data[tr_idx, , drop = FALSE],
                 method = "REML", select = TRUE)
    }
    pred_vec[te_idx] <- predict(fit, newdata = data[te_idx, , drop = FALSE])
  }
  
  # any rows never held out (shouldn't happen) will be NA; drop them
  keep <- !is.na(pred_vec)
  obs  <- data$rate[keep]
  pred <- pred_vec[keep]
  
  rmse <- sqrt(mean((obs - pred)^2))
  r2   <- 1 - sum((obs - pred)^2) / sum((obs - mean(obs))^2)
  
  list(pred = pred_vec, RMSE = rmse, R2 = r2)
}

## ============================================================
## 5. Define polynomial ladder (nested driver contribution + interactions)
## ============================================================
form_S0_Int      <- rate ~ 1
form_S1_T_lin    <- rate ~ ztemp
form_S1_T_quad   <- rate ~ ztemp + ztemp2
form_S2_TP_lin   <- rate ~ ztemp + ztemp2 + zpHts
form_S2_TP_quad  <- rate ~ ztemp + ztemp2 + zpHts + zpHts2
form_S3_TPL_lin  <- rate ~ ztemp + ztemp2 + zpHts + zpHts2 + zLi
form_S3_TPL_quad <- rate ~ ztemp + ztemp2 + zpHts + zpHts2 + zLi + zLi2
form_S4_TPL_int  <- rate ~ ztemp + ztemp2 + zpHts + zpHts2 + zLi + zLi2 +
  ztemp:zpHts + ztemp:zLi + zpHts:zLi + ztemp:zpHts:zLi

poly_specs <- tibble::tibble(
  model = c("S0_Int",
            "S1_T_lin","S1_T_quad",
            "S2_TP_lin","S2_TP_quad",
            "S3_TPL_lin","S3_TPL_quad",
            "S4_TPL_int"),
  composition = c(
    "Intercept only",
    "Temp (linear)",
    "Temp (linear+quad)",
    "Temp (lin+quad) + pH (linear)",
    "Temp (lin+quad) + pH (linear+quad)",
    "Temp (lin+quad) + pH (lin+quad) + Li (linear)",
    "Temp (lin+quad) + pH (lin+quad) + Li (linear+quad)",
    "Add linear interactions: temp×pH + temp×Li + pH×Li + 3-way"
  ),
  formula = list(
    form_S0_Int,
    form_S1_T_lin,   form_S1_T_quad,
    form_S2_TP_lin,  form_S2_TP_quad,
    form_S3_TPL_lin, form_S3_TPL_quad,
    form_S4_TPL_int
  )
)

## ============================================================
## 6. Define GAMs (additive & temp×pH interaction)
## ============================================================
gam_add_form <- rate ~ s(ztemp, k=9, bs="cr") +
  s(zpHts, k=9, bs="cr") +
  s(zLi,   k=9, bs="cr")

gam_int_form <- rate ~ s(ztemp, k=9, bs="cr") +
  s(zpHts, k=9, bs="cr") +
  s(zLi,   k=9, bs="cr") +
  te(ztemp, zpHts, k=c(5,5), bs=c("cr","cr"))

gam_specs <- tibble::tibble(
  model = c("GAM_add","GAM_int"),
  composition = c("s(temp) + s(pH) + s(Li)",
                  "s(temp) + s(pH) + s(Li) + te(temp,pH)"),
  formula = list(gam_add_form, gam_int_form)
)

## ============================================================
## 7.  CV evaluation for each polynomial model
## ============================================================
poly_cv <- poly_specs %>%
  mutate(
    cv  = map(formula, ~cv_out_of_fold(.x, train_set, fit_type = "lm")),
    RMSE_test = map_dbl(cv, "RMSE"),
    R2_test   = map_dbl(cv, "R2"),
    # fit once on full training set for AICc & n_par
    full_fit  = map(formula, ~lm(.x, data = train_set)),
    n_par      = map_dbl(full_fit, ~length(coef(.x))),
    AICc_train = map_dbl(full_fit, AICc_generic)
  ) %>%
  select(-cv, -full_fit) %>%
  mutate(model_group = "Polynomial")

## ============================================================
## 8.  CV evaluation for each GAM model
## ============================================================
gam_cv <- gam_specs %>%
  mutate(
    cv  = map(formula, ~cv_out_of_fold(.x, train_set, fit_type = "gam")),
    RMSE_test = map_dbl(cv, "RMSE"),
    R2_test   = map_dbl(cv, "R2"),
    # fit once on full training set for AICc & n_par
    full_fit  = map(formula, ~gam(.x, data = train_set, method="REML", select=TRUE)),
    n_par      = map_dbl(full_fit, ~length(coef(.x))),
    AICc_train = map_dbl(full_fit, AICc_generic)
  ) %>%
  select(-cv, -full_fit) %>%
  mutate(model_group = "GAM")

## ============================================================
## 9.  Combine & compute improvements vs CV intercept baseline
## ============================================================
all_results <- bind_rows(poly_cv, gam_cv)

baseline_rmse <- all_results$RMSE_test[all_results$model == "S0_Int"]
baseline_r2   <- all_results$R2_test[all_results$model == "S0_Int"]

all_results <- all_results %>%
  mutate(
    dRMSE_vs_baseline = baseline_rmse - RMSE_test,  # >0 ⇒ better than baseline
    dR2_vs_baseline   = R2_test - baseline_r2
  )

## ============================================================
## 10. Final tidy table (publishable subset)
## ============================================================
model_results_pub <- all_results %>%
  select(model_group, model, composition,
         n_par, AICc_train,
         RMSE_test, dRMSE_vs_baseline,
         R2_test, dR2_vs_baseline) %>%
  arrange(factor(model, levels = c(poly_specs$model, gam_specs$model)))

model_results_pub_fmt <- model_results_pub %>%
  mutate(across(where(is.numeric), ~round(., 3)))

print(model_results_pub_fmt)

## ============================================================
## 11. Write CSV for supplement
## ============================================================
write.csv(model_results_pub_fmt,
          file = "supplement_model_performance_CV.csv",
          row.names = FALSE)

message("Cross-validated comparison written: supplement_model_performance_CV.csv")

## ============================================================
## 12. (Optional) external hold-out scoring for top models
##      Uncomment to compare CV vs the untouched 20% test_set.
## ============================================================
# best_poly_formula <- form_S4_TPL_int
# best_gam_formula  <- gam_int_form
# 
# best_poly_fit <- lm(best_poly_formula, data = train_set)
# best_gam_fit  <- gam(best_gam_formula, data = train_set, method="REML", select=TRUE)
# 
# pred_poly_hold <- predict(best_poly_fit, newdata=test_set)
# pred_gam_hold  <- predict(best_gam_fit,  newdata=test_set)
# 
# rmse_poly_hold <- sqrt(mean((test_set$rate - pred_poly_hold)^2))
# rmse_gam_hold  <- sqrt(mean((test_set$rate - pred_gam_hold)^2))
# 
# r2_poly_hold   <- 1 - sum((test_set$rate - pred_poly_hold)^2) / sum((test_set$rate - mean(test_set$rate))^2)
# r2_gam_hold    <- 1 - sum((test_set$rate - pred_gam_hold)^2)  / sum((test_set$rate - mean(test_set$rate))^2)
# 
# cat("\nHold-out performance:\n")
# cat("  Polynomial (full int)  RMSE =", round(rmse_poly_hold,3), " R2 =", round(r2_poly_hold,3), "\n")
# cat("  GAM_int                RMSE =", round(rmse_gam_hold,3),  " R2 =", round(r2_gam_hold,3),  "\n")




## ============================================================
## HOLD-OUT (20 %) EVALUATION FOR SELECTED MODELS
## ============================================================
## 1.  Choose which polynomial to call “best”.
##     Here we use the final step of the ladder (S4_TPL_int).
best_poly_formula <- form_S4_TPL_int

## 2.  Re-fit models on the *full* training 80 %
poly_fit <- lm(best_poly_formula, data = train_set)

gam_add_fit <- gam(gam_add_form, data = train_set,
                   method = "REML", select = TRUE)

gam_int_fit <- gam(gam_int_form, data = train_set,
                   method = "REML", select = TRUE)

## Baseline intercept (mean of training rate)
baseline_pred <- rep(mean(train_set$rate), nrow(test_set))

## 3.  Predict hold-out
pred_poly <- predict(poly_fit, newdata = test_set)
pred_add  <- predict(gam_add_fit, newdata = test_set)
pred_int  <- predict(gam_int_fit, newdata = test_set)

## 4.  Compute metrics
hold_metrics <- function(pred, obs) {
  rmse <- sqrt(mean((obs - pred)^2))
  r2   <- 1 - sum((obs - pred)^2) / sum((obs - mean(obs))^2)
  c(RMSE = rmse, R2 = r2)
}

obs <- test_set$rate
tbl_hold <- tibble::tibble(
  Model = c("Intercept", "Polynomial_S4", "GAM_add", "GAM_int"),
  RMSE  = c(
    hold_metrics(baseline_pred, obs)[1],
    hold_metrics(pred_poly,     obs)[1],
    hold_metrics(pred_add,      obs)[1],
    hold_metrics(pred_int,      obs)[1]
  ),
  R2    = c(
    hold_metrics(baseline_pred, obs)[2],
    hold_metrics(pred_poly,     obs)[2],
    hold_metrics(pred_add,      obs)[2],
    hold_metrics(pred_int,      obs)[2]
  )
) %>%
  mutate(across(where(is.numeric), ~round(., 3)))

print(tbl_hold)

write.csv(tbl_hold,
          file = "hold_out20_performance.csv",
          row.names = FALSE)


### The following is old code that I have updated with the one above










library(ggplot2)
library(readxl)
library(lubridate) ## Parse Dates
library(caTools)   ## split dataset into train and test sets
library(caret)     ## K cross validation
#library(gam) 
library(mgcv)
library(AICcmodavg)

#Read the data. This is both single and multiple stressor data. The temperature is that of the bath they were in
d <- read_excel("ms_ss_bathtemp.xlsx")
str(d)

#Split into training and test sets
set.seed(123)
split <- sample.split(d$rate, SplitRatio = 0.80)
train_set <- subset(d, split == TRUE)
test_set <- subset(d, split == FALSE)


#Test with a linear model
lm1 <- lm(rate~temp, data = train_set)
summary(lm1)

#Test predictions on the test set
pred.lm1 = predict(lm1,newdata=test_set)
postResample(pred.lm1,test_set$rate)

#Test with linear model with interaction
lm2 <- lm(rate~temp*pHts*Li, data = train_set)
summary(lm2)

#Test predictions on the test set
pred.lm2 = predict(lm2,newdata=test_set)
postResample(pred.lm2,test_set$rate)

#Test with second degree linear model with interaction. Separate by degree
lm3 <- lm(rate~temp*pHts*Li+temp^2*pHts^2*Li^2, data = train_set)
summary(lm3)

#Test predictions on the test set
pred.lm3 = predict(lm3,newdata=test_set)
postResample(pred.lm3,test_set$rate)

#Test with second degree linear model with interaction. 
lm4 <- lm(rate~temp*pHts*Li*temp^2*pHts^2*Li^2, data = train_set)
summary(lm4)

#Test predictions on the test set
pred.lm4 = predict(lm4,newdata=test_set)
postResample(pred.lm4,test_set$rate)

#Test with second degree linear model with all possible interactions
model_multiple_interaction <- lm(rate ~ poly(temp, 2)  * poly(pHts, 2)* poly(Li, 2), data = d)
summary(model_multiple_interaction) #This is the best model of the above

#AICc(model_multiple_interaction)

#Test predictions on the test set
pred.lmmulti = predict(model_multiple_interaction,newdata=test_set)
postResample(pred.lmmulti,test_set$rate) #The Rsquared is 0.08 lower than in training



                              ############## GAMs #################

#GAM with all separately
gam1 <- gam(rate~s(temp, k=3)+s(pHts, k=3)+s(Li, k=3), data = train_set, method="ML", select = TRUE)
summary(gam1)
plot(gam1, pages = 1, , rug = TRUE, residuals = TRUE, pch = 1,
     cex = 1, shade=TRUE, shade.col = "lightblue", seWithMean = TRUE,
     shift = coef(gam1)[1])

#Test predictions on the test set
pred.gam1 = predict(gam1,newdata=test_set)
postResample(pred.gam1,test_set$rate)


AICc(gam1)

#Check the above GAM
gam.check(gam1, test = test_set)


#concurvity() function measures concurvity in model variables.
concurvity(gam1, full = TRUE) #Nice
# if the value is high (say, over 0.8), inspect your model more carefully.

concurvity(gam1, full = FALSE)


#GAM with pH-temp interaction
gam_inter <-gam(rate~s(temp, k=3)+s(pHts, k=3)+s(Li, k=3) + 
                  + s(temp, pHts), data = train_set, method="ML", select = TRUE)
summary(gam_inter)
plot(gam_inter, pages = 1, , rug = TRUE, residuals = TRUE, pch = 1,
     cex = 1, shade=TRUE, shade.col = "lightblue", seWithMean = TRUE,
     shift = coef(gam1)[1])

AICc(gam_inter)

#Test predictions on the test set
pred.gam_inter = predict(gam_inter,newdata=test_set)
postResample(pred.gam_inter,test_set$rate)



# GAM with three way interaction 1313 
# Is worse than the pH-temp interaction
 
gam_threeway <- gam(rate ~ s(temp, pHts, Li, k = 5), data = train_set, method = "ML", select = TRUE)
summary(gam_threeway)
plot(gam_threeway, pages = 1, , rug = TRUE, residuals = TRUE, pch = 1,
     cex = 1, shade=TRUE, shade.col = "lightblue", seWithMean = TRUE,
     shift = coef(gam_threeway)[1])


AICc(gam_threeway)

#Test predictions on the test set
pred.gam_threeway = predict(gam_threeway,newdata=test_set)
postResample(pred.gam_threeway,test_set$rate)
