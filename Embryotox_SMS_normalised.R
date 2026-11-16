# Load libraries
install.packages("drc")
# install.packages("ggplot2")
library(drc)
library(ggplot2)
#install.packages("boot")
library(boot)

# Set working directory
## To do this on your computer, save the spreadsheet as a csv file in a folder
## then go to "Session", "Set working directory", "Choose directory", find the folder with your data file and save that
## as your directory. The pathfile will appear in the console at the bottom of the screen. Copy and paste it as I have below.
setwd("C:/Users/hannahg/Cawthron/Hannah G PhD Team - Documents/General/R/Julien")


################BTX2 DR#####################################################################################
# Load data 
AD_norm=read.csv("Embryotox_DR_norm.csv")

## Subset to temps and location

BB_17=subset(AD_norm, Temperature=="17" & ID =="BB")
BB_19= subset(AD_norm, Temperature=="19" & ID =="BB")
BB_21= subset(AD_norm, Temperature=="21" & ID =="BB")

W_17=subset(AD_norm, Temperature=="17" & ID =="W")
W_19= subset(AD_norm, Temperature=="19" & ID =="W")
W_21= subset(AD_norm, Temperature=="21" & ID =="W")

G_17=subset(AD_norm, Temperature=="17" & ID =="G")
G_19= subset(AD_norm, Temperature=="19" & ID =="G")
G_21= subset(AD_norm, Temperature=="21" & ID =="G")

SK_17=subset(AD_norm, Temperature=="17" & ID =="SK")
SK_19= subset(AD_norm, Temperature=="19" & ID =="SK")
SK_21= subset(AD_norm, Temperature=="21" & ID =="SK")
# 
# # Select appropriate model
# BB17_model_LL4 <- drm(Surv ~ Conc, data = BB_17, fct = LL.4())
# BB17_model_LL3 <-drm(Surv ~ Conc, data = BB_17, fct = LL.3())
# BB17_model_LL2 <- drm(Surv ~ Conc, data = BB_17, fct = LL.2())
# BB17_model_W14 <- drm(Surv ~ Conc, data = BB_17, fct = W1.4())
# BB17_model_BC4 <- drm(Surv ~ Conc, data = BB_17, fct = BC.4())
# BB17_model_BC5 <- drm(Surv ~ Conc, data = BB_17, fct = BC.5())
# 
# # Compare models using AIC
# AIC(BB17_model_LL4, BB17_model_LL3, BB17_model_LL2, BB17_model_W14, BB17_model_BC4, BB17_model_BC5)
# 
# #Extract model with lowest AIC
# models <- list(LL4 = BB17_model_LL4, LL3 = BB17_model_LL3, LL2= BB17_model_LL2, W14 = BB17_model_W14, BC4 = BB17_model_BC4, BC5= BB17_model_BC5)
# best_model <- models[[which.min(sapply(models, AIC))]]
# summary(best_model) ### BC4 is best, but won't provide an EC50. LL4 model is very close to being as good. 
# summary(BB17_model_LL3)
# 
# 
# # 2. Fit a  3-parameter log-logistic model
# model <- drm(Surv ~ Conc,
#              data = BB_17,
#              fct = LL.3())
# 
# summary(model)
# 
# # 3. Extract EC10 and EC50 with 95% CI
# ed_vals <- ED(model, c(10, 50), interval = "delta")
# 
# ed10_value <- ed_vals[1,1]; ed10_low <- ed_vals[1,2]; ed10_high <- ed_vals[1,3]
# ed50_value <- ed_vals[2,1]; ed50_low <- ed_vals[2,2]; ed50_high <- ed_vals[2,3]
# ed10_se    <- ed_vals[1, "Std. Error"]
# ed50_se    <- ed_vals[2, "Std. Error"]
# # Predicted responses at EC10 and EC50
# ed10_response <- predict(model, newdata = data.frame(Conc = ed10_value))
# ed50_response <- predict(model, newdata = data.frame(Conc = ed50_value))
# 
# 
# # 4. Generate smooth predictions + CI
# newdata <- data.frame(Conc = exp(seq(log(min(BB_17$Conc[BB_17$Conc > 0])),
#                                      log(max(BB_17$Conc[BB_17$Conc > 0])),
#                                      length.out = 200)))
# 
# 
# preds <- predict(model, newdata, interval = "confidence")
# newdata$fit <- preds[,1]
# newdata$lwr <- preds[,2]
# newdata$upr <- preds[,3]
# 
# # Plot 
# ggplot() +
#   
#   # BTX2 (toxic) raw points
#   geom_point(data = BB_17,
#              aes(x = Conc, y = Surv),
#              size = 3, alpha = 0.8, color = "black") +
#   
#   # BTX2 fitted curve + CI
#   geom_ribbon(data = newdata,
#               aes(x = Conc, ymin = lwr, ymax = upr),
#               fill = "grey25", alpha = 0.2) +
#   geom_line(data = newdata,
#             aes(x = Conc, y = fit),
#             color = "black", linewidth = 2) +
#   
#   # EC10
#   annotate("point", x = ed10_value, y = ed10_response,
#            color = "black", size = 3) +
#   geom_vline(xintercept = ed10_value,
#              linetype = "dotted", color = "black", linewidth = 1.4) +
#   geom_hline(yintercept = ed10_response,
#              linetype = "dotted", color = "black", linewidth = 1.4) +
#   
#   # EC50
#   annotate("point", x = ed50_value, y = ed50_response,
#            color = "black", size = 3) +
#   geom_vline(xintercept = ed50_value,
#              linetype = "dashed", color = "black", linewidth = 1.4) +
#   geom_hline(yintercept = ed50_response,
#              linetype = "dashed", color = "black", linewidth = 1.4) +
#   
#   scale_x_log10() +
#   
#   labs(
#     x = "Concentration (%)",
#     y = "Embryo survival (%)"
#   ) +
#   
#   theme_classic(base_size = 14) +
#   theme(
#     axis.title = element_text(size = 16),
#     axis.text = element_text(size = 14)
#   )
# 
# ############# BB 19 deg ####################################################################################
# # Select appropriate model
# BB19_model_LL4 <- drm(Surv ~ Conc, data = BB_19, fct = LL.4())
# BB19_model_LL3 <-drm(Surv ~ Conc, data = BB_19, fct = LL.3())
# BB19_model_LL2 <- drm(Surv ~ Conc, data = BB_19, fct = LL.2())
# BB19_model_W14 <- drm(Surv ~ Conc, data = BB_19, fct = W1.4())
# BB19_model_BC4 <- drm(Surv ~ Conc, data = BB_19, fct = BC.4())
# BB19_model_BC5 <- drm(Surv ~ Conc, data = BB_19, fct = BC.5())
# 
# # Compare models using AIC
# AIC(BB19_model_LL4, BB19_model_LL3, BB19_model_LL2, BB19_model_W14, BB19_model_BC4, BB19_model_BC5)
# 
# #Extract model with lowest AIC
# models <- list(LL4 = BB19_model_LL4, LL3 = BB19_model_LL3, LL2= BB19_model_LL2, W14 = BB19_model_W14, BC4 = BB19_model_BC4, BC5= BB19_model_BC5)
# best_model <- models[[which.min(sapply(models, AIC))]]
# summary(best_model) ### BC4 is best, but won't provide an EC50. LL4 model is very close to being as good. 
# summary(BB19_model_LL3)
# 
# 
# # 2. Fit a  3-parameter log-logistic model
# model <- drm(Surv ~ Conc,
#              data = BB_19,
#              fct = LL.3())
# 
# summary(model)
# 
# # 3. Extract EC10 and EC50 with 95% CI
# ed_vals <- ED(model, c(10, 50), interval = "delta")
# 
# ed10_value <- ed_vals[1,1]; ed10_low <- ed_vals[1,2]; ed10_high <- ed_vals[1,3]
# ed50_value <- ed_vals[2,1]; ed50_low <- ed_vals[2,2]; ed50_high <- ed_vals[2,3]
# ed10_se    <- ed_vals[1, "Std. Error"]
# ed50_se    <- ed_vals[2, "Std. Error"]
# # Predicted responses at EC10 and EC50
# ed10_response <- predict(model, newdata = data.frame(Conc = ed10_value))
# ed50_response <- predict(model, newdata = data.frame(Conc = ed50_value))
# 
# 
# # 4. Generate smooth predictions + CI
# newdata <- data.frame(Conc = exp(seq(log(min(BB_19$Conc[BB_19$Conc > 0])),
#                                      log(max(BB_19$Conc[BB_19$Conc > 0])),
#                                      length.out = 200)))
# 
# 
# preds <- predict(model, newdata, interval = "confidence")
# newdata$fit <- preds[,1]
# newdata$lwr <- preds[,2]
# newdata$upr <- preds[,3]
# 
# # Plot 
# ggplot() +
#   
#   # BTX2 (toxic) raw points
#   geom_point(data = BB_19,
#              aes(x = Conc, y = Surv),
#              size = 3, alpha = 0.8, color = "black") +
#   
#   # BTX2 fitted curve + CI
#   geom_ribbon(data = newdata,
#               aes(x = Conc, ymin = lwr, ymax = upr),
#               fill = "grey25", alpha = 0.2) +
#   geom_line(data = newdata,
#             aes(x = Conc, y = fit),
#             color = "black", linewidth = 2) +
#   
#   # EC10
#   annotate("point", x = ed10_value, y = ed10_response,
#            color = "black", size = 3) +
#   geom_vline(xintercept = ed10_value,
#              linetype = "dotted", color = "black", linewidth = 1.4) +
#   geom_hline(yintercept = ed10_response,
#              linetype = "dotted", color = "black", linewidth = 1.4) +
#   
#   # EC50
#   annotate("point", x = ed50_value, y = ed50_response,
#            color = "black", size = 3) +
#   geom_vline(xintercept = ed50_value,
#              linetype = "dashed", color = "black", linewidth = 1.4) +
#   geom_hline(yintercept = ed50_response,
#              linetype = "dashed", color = "black", linewidth = 1.4) +
#   
#   scale_x_log10() +
#   
#   labs(
#     x = "Concentration (%)",
#     y = "Embryo survival (%)"
#   ) +
#   
#   theme_classic(base_size = 14) +
#   theme(
#     axis.title = element_text(size = 16),
#     axis.text = element_text(size = 14)
#   )
# 
# 
# ################BB 21deg#####################################################################################
# 
# BB21_model_LL4 <- drm(Surv ~ Conc, data = BB_21, fct = LL.4())
# BB21_model_LL3 <-drm(Surv ~ Conc, data = BB_21, fct = LL.3())
# BB21_model_LL2 <- drm(Surv ~ Conc, data = BB_21, fct = LL.2())
# BB21_model_W14 <- drm(Surv ~ Conc, data = BB_21, fct = W1.4())
# BB21_model_BC4 <- drm(Surv ~ Conc, data = BB_21, fct = BC.4())
# BB21_model_BC5 <- drm(Surv ~ Conc, data = BB_21, fct = BC.5())
# 
# # Compare models using AIC
# AIC(BB21_model_LL4, BB21_model_LL3, BB21_model_LL2, BB21_model_W14, BB21_model_BC4, BB21_model_BC5)
# 
# #Extract model with lowest AIC
# models <- list(LL4 = BB21_model_LL4, LL3 = BB21_model_LL3, LL2= BB21_model_LL2, W14 = BB21_model_W14, BC4 = BB21_model_BC4, BC5= BB21_model_BC5)
# best_model <- models[[which.min(sapply(models, AIC))]]
# summary(best_model) ### BC4 is best, but won't provide an EC50. LL4 model is very close to being as good. 
# summary(BB21_model_LL3)
# 
# 
# # 2. Fit a  3-parameter log-logistic model
# model <- drm(Surv ~ Conc,
#              data = BB_21,
#              fct = LL.3())
# 
# summary(model)
# 
# # 3. Extract EC10 and EC50 with 95% CI
# ed_vals <- ED(model, c(10, 50), interval = "delta")
# 
# ed10_value <- ed_vals[1,1]; ed10_low <- ed_vals[1,2]; ed10_high <- ed_vals[1,3]
# ed50_value <- ed_vals[2,1]; ed50_low <- ed_vals[2,2]; ed50_high <- ed_vals[2,3]
# ed10_se    <- ed_vals[1, "Std. Error"]
# ed50_se    <- ed_vals[2, "Std. Error"]
# # Predicted responses at EC10 and EC50
# ed10_response <- predict(model, newdata = data.frame(Conc = ed10_value))
# ed50_response <- predict(model, newdata = data.frame(Conc = ed50_value))
# 
# 
# # 4. Generate smooth predictions + CI
# newdata <- data.frame(Conc = exp(seq(log(min(BB_21$Conc[BB_21$Conc > 0])),
#                                      log(max(BB_21$Conc[BB_21$Conc > 0])),
#                                      length.out = 200)))
# 
# 
# preds <- predict(model, newdata, interval = "confidence")
# newdata$fit <- preds[,1]
# newdata$lwr <- preds[,2]
# newdata$upr <- preds[,3]
# 
# # Plot 
# ggplot() +
#   
#   # BTX2 (toxic) raw points
#   geom_point(data = BB_21,
#              aes(x = Conc, y = Surv),
#              size = 3, alpha = 0.8, color = "black") +
#   
#   # BTX2 fitted curve + CI
#   geom_ribbon(data = newdata,
#               aes(x = Conc, ymin = lwr, ymax = upr),
#               fill = "grey25", alpha = 0.2) +
#   geom_line(data = newdata,
#             aes(x = Conc, y = fit),
#             color = "black", linewidth = 2) +
#   
#   # EC10
#   annotate("point", x = ed10_value, y = ed10_response,
#            color = "black", size = 3) +
#   geom_vline(xintercept = ed10_value,
#              linetype = "dotted", color = "black", linewidth = 1.4) +
#   geom_hline(yintercept = ed10_response,
#              linetype = "dotted", color = "black", linewidth = 1.4) +
#   
#   # EC50
#   annotate("point", x = ed50_value, y = ed50_response,
#            color = "black", size = 3) +
#   geom_vline(xintercept = ed50_value,
#              linetype = "dashed", color = "black", linewidth = 1.4) +
#   geom_hline(yintercept = ed50_response,
#              linetype = "dashed", color = "black", linewidth = 1.4) +
#   
#   scale_x_log10() +
#   
#   labs(
#     x = "Concentration (%)",
#     y = "Embryo survival (%)"
#   ) +
#   
#   theme_classic(base_size = 14) +
#   theme(
#     axis.title = element_text(size = 16),
#     axis.text = element_text(size = 14)
#   )


################DIFFERENT METHOD#####################################################################################
#---------------------BB sediment ----------------------------------------
## Fit each model

model_BB17 <- drm(Surv ~ Conc, data = BB_17, fct = LL.3())

# 3. Extract EC10 and EC50 with 95% CI
ed_vals <- ED(model_BB17, c(10, 25, 50), interval = "delta")

BB_17_EC <- data.frame(
  Sediment = "Beatrix Bay",
  Temperature = "17 °C",
  Model = "LL.3",
  EC10 = ed_vals[1, "Estimate"],
  EC10_low = ed_vals[1, "Lower"],
  EC10_high = ed_vals[1, "Upper"],
  EC10_SE = ed_vals[1, "Std. Error"],
  EC25 = ed_vals[2, "Estimate"],
  EC25_low = ed_vals[2, "Lower"],
  EC25_high = ed_vals[2, "Upper"],
  EC25_SE = ed_vals[2, "Std. Error"],
  EC50 = ed_vals[3, "Estimate"],
  EC50_low = ed_vals[3, "Lower"],
  EC50_high = ed_vals[3, "Upper"],
  EC50_SE = ed_vals[3, "Std. Error"]
)

model_BB19 <- drm(Surv ~ Conc, data = BB_19, fct = LL.3())

# 3. Extract EC10 and EC50 with 95% CI
ed_vals <- ED(model_BB19, c(10, 25, 50), interval = "delta")

BB_19_EC <- data.frame(
  Sediment = "Beatrix Bay",
  Temperature = "19 °C",
  Model = "LL.3",
  EC10 = ed_vals[1, "Estimate"],
  EC10_low = ed_vals[1, "Lower"],
  EC10_high = ed_vals[1, "Upper"],
  EC10_SE = ed_vals[1, "Std. Error"],
  EC25 = ed_vals[2, "Estimate"],
  EC25_low = ed_vals[2, "Lower"],
  EC25_high = ed_vals[2, "Upper"],
  EC25_SE = ed_vals[2, "Std. Error"],
  EC50 = ed_vals[3, "Estimate"],
  EC50_low = ed_vals[3, "Lower"],
  EC50_high = ed_vals[3, "Upper"],
  EC50_SE = ed_vals[3, "Std. Error"]
)

model_BB21 <- drm(Surv ~ Conc, data = BB_21, fct = LL.3())

# 3. Extract EC10 and EC50 with 95% CI
ed_vals <- ED(model_BB21, c(10, 25, 50), interval = "delta")

BB_21_EC <- data.frame(
  Sediment = "Beatrix Bay",
  Temperature = "21 °C",
  Model = "LL.3",
  EC10 = ed_vals[1, "Estimate"],
  EC10_low = ed_vals[1, "Lower"],
  EC10_high = ed_vals[1, "Upper"],
  EC10_SE = ed_vals[1, "Std. Error"],
  EC25 = ed_vals[2, "Estimate"],
  EC25_low = ed_vals[2, "Lower"],
  EC25_high = ed_vals[2, "Upper"],
  EC25_SE = ed_vals[2, "Std. Error"],
  EC50 = ed_vals[3, "Estimate"],
  EC50_low = ed_vals[3, "Lower"],
  EC50_high = ed_vals[3, "Upper"],
  EC50_SE = ed_vals[3, "Std. Error"]
)

# Create prediction data for each
new_BB17 <- data.frame(
  Conc = exp(seq(log(min(BB_17$Conc)),
                 log(max(BB_17$Conc)),
                 length.out = 200))
)

pred17 <- predict(model_BB17,
                  newdata = new_BB17,
                  interval = "confidence")

new_BB17$fit <- pred17[,1]
new_BB17$lwr <- pred17[,2]
new_BB17$upr <- pred17[,3]
new_BB17$Temp <- "17°C"

new_BB19 <- data.frame(
  Conc = exp(seq(log(min(BB_19$Conc)),
                 log(max(BB_19$Conc)),
                 length.out = 200))
)

pred19 <- predict(model_BB19,
                  newdata = new_BB19,
                  interval = "confidence")

new_BB19$fit <- pred19[,1]
new_BB19$lwr <- pred19[,2]
new_BB19$upr <- pred19[,3]
new_BB19$Temp <- "19°C"

new_BB21 <- data.frame(
  Conc = exp(seq(log(min(BB_21$Conc)),
                 log(max(BB_21$Conc)),
                 length.out = 200))
)

pred21 <- predict(model_BB21,
                  newdata = new_BB21,
                  interval = "confidence")

new_BB21$fit <- pred21[,1]
new_BB21$lwr <- pred21[,2]
new_BB21$upr <- pred21[,3]
new_BB21$Temp <- "21°C"


## Combine predictions and raw data
pred_all <- rbind(new_BB17, new_BB19, new_BB21)

BB_17$Temp <- "17°C"
BB_19$Temp <- "19°C"
BB_21$Temp <- "21°C"

raw_all <- rbind(BB_17, BB_19, BB_21)

# Plot
DR_BB= ggplot() +
  
  geom_ribbon(
    data = pred_all,
    aes(x = Conc,
        ymin = lwr,
        ymax = upr,
        fill = Temp),
    alpha = 0.2,
    colour = NA
  ) +
  
  geom_line(
    data = pred_all,
    aes(x = Conc,
        y = fit,
        colour = Temp),
    linewidth = 1.2
  ) +
  
  geom_point(
    data = raw_all,
    aes(x = Conc,
        y = Surv,
        colour = Temp),
    size = 2,
    alpha = 0.7
  ) +
  
  scale_x_log10(
    limits = c(0.39, 12.5),
    breaks = c(0.2, 0.39, 0.78, 1.56, 3.13, 6.25, 12.5)
  )+
  
  coord_cartesian(ylim = c(0, 120)) +
  
  
  scale_colour_manual(
    values = c(
      "17°C" = "#1042B2",
      "19°C" = "#41D9A1",
      "21°C" = "#E27D60"
    )
  ) +
  
  scale_fill_manual(
    values = c(
      "17°C" = "#1042B2",
      "19°C" = "#41D9A1",
      "21°C" = "#E27D60"
    )
  ) +
  
  labs(
    x = "Elutriate (%)",
    y = "Survival, D-yield (%)",
    colour = "Temperature",
    fill = "Temperature"
  ) +
  
  theme_classic(base_size = 14)

DR_BB



#---------------------SK sediment ----------------------------------------
# Best fit? 
# Select appropriate model
model_SK17_LL4 <- drm(Surv ~ Conc, data = SK_17, fct = LL.4())
model_SK17_LL3 <-drm(Surv ~ Conc, data = SK_17, fct = LL.3())
model_SK17_LL2 <- drm(Surv ~ Conc, data = SK_17, fct = LL.2())
model_SK17_W14 <- drm(Surv ~ Conc, data = SK_17, fct = W1.4())
model_SK17_BC4 <- drm(Surv ~ Conc, data = SK_17, fct = BC.4())
model_SK17_BC5 <- drm(Surv ~ Conc, data = SK_17, fct = BC.5())

# Compare models using AIC
AIC(model_SK17_LL4, model_SK17_LL3, model_SK17_LL2, model_SK17_W14, model_SK17_BC4, model_SK17_BC5)

# Select appropriate model
model_SK19_LL4 <- drm(Surv ~ Conc, data = SK_19, fct = LL.4())
model_SK19_LL3 <-drm(Surv ~ Conc, data = SK_19, fct = LL.3())
model_SK19_LL2 <- drm(Surv ~ Conc, data = SK_19, fct = LL.2())
model_SK19_W14 <- drm(Surv ~ Conc, data = SK_19, fct = W1.4())
model_SK19_BC4 <- drm(Surv ~ Conc, data = SK_19, fct = BC.4())
model_SK19_BC5 <- drm(Surv ~ Conc, data = SK_19, fct = BC.5())

# Compare models using AIC
AIC(model_SK19_LL4, model_SK19_LL3, model_SK19_LL2, model_SK19_W14, model_SK19_BC4, model_SK19_BC5)

# Select appropriate model
model_SK21_LL4 <- drm(Surv ~ Conc, data = SK_21, fct = LL.4())
model_SK21_LL3 <-drm(Surv ~ Conc, data = SK_21, fct = LL.3())
model_SK21_LL2 <- drm(Surv ~ Conc, data = SK_21, fct = LL.2())
model_SK21_W14 <- drm(Surv ~ Conc, data = SK_21, fct = W1.4())
model_SK21_BC4 <- drm(Surv ~ Conc, data = SK_21, fct = BC.4())
model_SK21_BC5 <- drm(Surv ~ Conc, data = SK_21, fct = BC.5())

# Compare models using AIC
AIC(model_SK21_LL4, model_SK21_LL3, model_SK21_LL2, model_SK21_W14, model_SK21_BC4, model_SK21_BC5)


## Fit each model

model_SK17 <- drm(Surv ~ Conc, data = SK_17, fct = LL.3())

# 3. Extract EC10 and EC50 with 95% CI
ed_vals <- ED(model_SK17, c(10, 25, 50), interval = "delta")

SK_17_EC <- data.frame(
  Sediment = "Skiddaw",
  Temperature = "17 °C",
  Model = "LL.3",
  EC10 = ed_vals[1, "Estimate"],
  EC10_low = ed_vals[1, "Lower"],
  EC10_high = ed_vals[1, "Upper"],
  EC10_SE = ed_vals[1, "Std. Error"],
  EC25 = ed_vals[2, "Estimate"],
  EC25_low = ed_vals[2, "Lower"],
  EC25_high = ed_vals[2, "Upper"],
  EC25_SE = ed_vals[2, "Std. Error"],
  EC50 = ed_vals[3, "Estimate"],
  EC50_low = ed_vals[3, "Lower"],
  EC50_high = ed_vals[3, "Upper"],
  EC50_SE = ed_vals[3, "Std. Error"]
)


model_SK19 <- drm(Surv ~ Conc, data = SK_19, fct = LL.3())

# 3. Extract EC10 and EC50 with 95% CI
ed_vals <- ED(model_SK19, c(10, 25, 50), interval = "delta")

SK_19_EC <- data.frame(
  Sediment = "Skiddaw",
  Temperature = "19 °C",
  Model = "LL.3",
  EC10 = ed_vals[1, "Estimate"],
  EC10_low = ed_vals[1, "Lower"],
  EC10_high = ed_vals[1, "Upper"],
  EC10_SE = ed_vals[1, "Std. Error"],
  EC25 = ed_vals[2, "Estimate"],
  EC25_low = ed_vals[2, "Lower"],
  EC25_high = ed_vals[2, "Upper"],
  EC25_SE = ed_vals[2, "Std. Error"],
  EC50 = ed_vals[3, "Estimate"],
  EC50_low = ed_vals[3, "Lower"],
  EC50_high = ed_vals[3, "Upper"],
  EC50_SE = ed_vals[3, "Std. Error"]
)

model_SK21 <- drm(Surv ~ Conc, data = SK_21, fct = LL.3())

# 3. Extract EC10 and EC50 with 95% CI
ed_vals <- ED(model_SK21, c(10, 25, 50), interval = "delta")

SK_21_EC <- data.frame(
  Sediment = "Skiddaw",
  Temperature = "21 °C",
  Model = "LL.3",
  EC10 = ed_vals[1, "Estimate"],
  EC10_low = ed_vals[1, "Lower"],
  EC10_high = ed_vals[1, "Upper"],
  EC10_SE = ed_vals[1, "Std. Error"],
  EC25 = ed_vals[2, "Estimate"],
  EC25_low = ed_vals[2, "Lower"],
  EC25_high = ed_vals[2, "Upper"],
  EC25_SE = ed_vals[2, "Std. Error"],
  EC50 = ed_vals[3, "Estimate"],
  EC50_low = ed_vals[3, "Lower"],
  EC50_high = ed_vals[3, "Upper"],
  EC50_SE = ed_vals[3, "Std. Error"]
)



# Create prediction data for each
new_SK17 <- data.frame(
  Conc = exp(seq(log(min(SK_17$Conc)),
                 log(max(SK_17$Conc)),
                 length.out = 200))
)

pred17 <- predict(model_SK17,
                  newdata = new_SK17,
                  interval = "confidence")

new_SK17$fit <- pred17[,1]
new_SK17$lwr <- pred17[,2]
new_SK17$upr <- pred17[,3]
new_SK17$Temp <- "17°C"

new_SK19 <- data.frame(
  Conc = exp(seq(log(min(SK_19$Conc)),
                 log(max(SK_19$Conc)),
                 length.out = 200))
)

pred19 <- predict(model_SK19,
                  newdata = new_SK19,
                  interval = "confidence")

new_SK19$fit <- pred19[,1]
new_SK19$lwr <- pred19[,2]
new_SK19$upr <- pred19[,3]
new_SK19$Temp <- "19°C"

new_SK21 <- data.frame(
  Conc = exp(seq(log(min(SK_21$Conc)),
                 log(max(SK_21$Conc)),
                 length.out = 200))
)

pred21 <- predict(model_SK21,
                  newdata = new_SK21,
                  interval = "confidence")

new_SK21$fit <- pred21[,1]
new_SK21$lwr <- pred21[,2]
new_SK21$upr <- pred21[,3]
new_SK21$Temp <- "21°C"


## Combine predictions and raw data
pred_all_SK <- rbind(new_SK17, new_SK19, new_SK21)

SK_17$Temp <- "17°C"
SK_19$Temp <- "19°C"
SK_21$Temp <- "21°C"

raw_all_SK <- rbind(SK_17, SK_19, SK_21)

# Plot
DR_SK= ggplot() +
  
  geom_ribbon(
    data = pred_all_SK,
    aes(x = Conc,
        ymin = lwr,
        ymax = upr,
        fill = Temp),
    alpha = 0.2,
    colour = NA
  ) +
  
  geom_line(
    data = pred_all_SK,
    aes(x = Conc,
        y = fit,
        colour = Temp),
    linewidth = 1.2
  ) +
  
  geom_point(
    data = raw_all_SK,
    aes(x = Conc,
        y = Surv,
        colour = Temp),
    size = 2,
    alpha = 0.7
  ) +
  
  scale_x_log10(
    limits = c(0.39, 12.5),
    breaks = c(0.2, 0.39, 0.78, 1.56, 3.13, 6.25, 12.5)
  )+
  
  coord_cartesian(ylim = c(0, 120)) +
  
  
  scale_colour_manual(
    values = c(
      "17°C" = "#1042B2",
      "19°C" = "#41D9A1",
      "21°C" = "#E27D60"
    )
  ) +
  
  scale_fill_manual(
    values = c(
      "17°C" = "#1042B2",
      "19°C" = "#41D9A1",
      "21°C" = "#E27D60"
    )
  ) +
  
  labs(
    x = "Elutriate (%)",
    y = "Survival, D-yield (%)",
    colour = "Temperature",
    fill = "Temperature"
  ) +
  
  theme_classic(base_size = 14)

DR_SK


#---------------------Waiona sediment ----------------------------------------
# Select appropriate model
model_W17_LL4 <- drm(Surv ~ Conc, data = W_17, fct = LL.4())
model_W17_LL3 <-drm(Surv ~ Conc, data = W_17, fct = LL.3())
model_W17_LL2 <- drm(Surv ~ Conc, data = W_17, fct = LL.2())
model_W17_W14 <- drm(Surv ~ Conc, data = W_17, fct = W1.4())
model_W17_BC4 <- drm(Surv ~ Conc, data = W_17, fct = BC.4())
model_W17_BC5 <- drm(Surv ~ Conc, data = W_17, fct = BC.5())

# Compare models using AIC
AIC(model_W17_LL4, model_W17_LL3, model_W17_LL2, model_W17_W14, model_W17_BC4, model_W17_BC5)

# Select appropriate model
model_W19_LL4 <- drm(Surv ~ Conc, data = W_19, fct = LL.4())
model_W19_LL3 <-drm(Surv ~ Conc, data = W_19, fct = LL.3())
model_W19_LL2 <- drm(Surv ~ Conc, data = W_19, fct = LL.2())
model_W19_W14 <- drm(Surv ~ Conc, data = W_19, fct = W1.4())
model_W19_BC4 <- drm(Surv ~ Conc, data = W_19, fct = BC.4())
model_W19_BC5 <- drm(Surv ~ Conc, data = W_19, fct = BC.5())

# Compare models using AIC
AIC(model_W19_LL4, model_W19_LL3, model_W19_LL2, model_W19_W14, model_W19_BC4, model_W19_BC5)


# Select appropriate model
model_W21_LL4 <- drm(Surv ~ Conc, data = W_21, fct = LL.4())
model_W21_LL3 <-drm(Surv ~ Conc, data = W_21, fct = LL.3())
model_W21_LL2 <- drm(Surv ~ Conc, data = W_21, fct = LL.2())
model_W21_W14 <- drm(Surv ~ Conc, data = W_21, fct = W1.4())
model_W21_BC4 <- drm(Surv ~ Conc, data = W_21, fct = BC.4())
model_W21_BC5 <- drm(Surv ~ Conc, data = W_21, fct = BC.5())

# Compare models using AIC
AIC(model_W21_LL4, model_W21_LL3, model_W21_LL2, model_W21_W14, model_W21_BC4, model_W21_BC5)

## Fit each model

model_W17 <- drm(Surv ~ Conc, data = W_17, fct = LL.3())

# 3. Extract EC10 and EC50 with 95% CI
ed_vals <- ED(model_W17, c(10, 25, 50), interval = "delta")

W_17_EC <- data.frame(
  Sediment = "Waiona Bay",
  Temperature = "17 °C",
  Model = "LL.3",
  EC10 = ed_vals[1, "Estimate"],
  EC10_low = ed_vals[1, "Lower"],
  EC10_high = ed_vals[1, "Upper"],
  EC10_SE = ed_vals[1, "Std. Error"],
  EC25 = ed_vals[2, "Estimate"],
  EC25_low = ed_vals[2, "Lower"],
  EC25_high = ed_vals[2, "Upper"],
  EC25_SE = ed_vals[2, "Std. Error"],
  EC50 = ed_vals[3, "Estimate"],
  EC50_low = ed_vals[3, "Lower"],
  EC50_high = ed_vals[3, "Upper"],
  EC50_SE = ed_vals[3, "Std. Error"]
)

model_W19 <- drm(Surv ~ Conc, data = W_19, fct = LL.3())

# 3. Extract EC10 and EC50 with 95% CI
ed_vals <- ED(model_W19, c(10, 25, 50), interval = "delta")

W_19_EC <- data.frame(
  Sediment = "Waiona Bay",
  Temperature = "19 °C",
  Model = "LL.3",
  EC10 = ed_vals[1, "Estimate"],
  EC10_low = ed_vals[1, "Lower"],
  EC10_high = ed_vals[1, "Upper"],
  EC10_SE = ed_vals[1, "Std. Error"],
  EC25 = ed_vals[2, "Estimate"],
  EC25_low = ed_vals[2, "Lower"],
  EC25_high = ed_vals[2, "Upper"],
  EC25_SE = ed_vals[2, "Std. Error"],
  EC50 = ed_vals[3, "Estimate"],
  EC50_low = ed_vals[3, "Lower"],
  EC50_high = ed_vals[3, "Upper"],
  EC50_SE = ed_vals[3, "Std. Error"]
)

model_W21 <- drm(Surv ~ Conc, data = W_21, fct = LL.3())

# 3. Extract EC10 and EC50 with 95% CI
ed_vals <- ED(model_W21, c(10, 25, 50), interval = "delta")

W_21_EC <- data.frame(
  Sediment = "Waiona Bay",
  Temperature = "21 °C",
  Model = "LL.3",
  EC10 = ed_vals[1, "Estimate"],
  EC10_low = ed_vals[1, "Lower"],
  EC10_high = ed_vals[1, "Upper"],
  EC10_SE = ed_vals[1, "Std. Error"],
  EC25 = ed_vals[2, "Estimate"],
  EC25_low = ed_vals[2, "Lower"],
  EC25_high = ed_vals[2, "Upper"],
  EC25_SE = ed_vals[2, "Std. Error"],
  EC50 = ed_vals[3, "Estimate"],
  EC50_low = ed_vals[3, "Lower"],
  EC50_high = ed_vals[3, "Upper"],
  EC50_SE = ed_vals[3, "Std. Error"]
)

# Create prediction data for each
new_W17 <- data.frame(
  Conc = exp(seq(log(min(W_17$Conc)),
                 log(max(W_17$Conc)),
                 length.out = 200))
)

pred17 <- predict(model_W17,
                  newdata = new_W17,
                  interval = "confidence")

new_W17$fit <- pred17[,1]
new_W17$lwr <- pred17[,2]
new_W17$upr <- pred17[,3]
new_W17$Temp <- "17°C"

new_W19 <- data.frame(
  Conc = exp(seq(log(min(W_19$Conc)),
                 log(max(W_19$Conc)),
                 length.out = 200))
)

pred19 <- predict(model_W19,
                  newdata = new_W19,
                  interval = "confidence")

new_W19$fit <- pred19[,1]
new_W19$lwr <- pred19[,2]
new_W19$upr <- pred19[,3]
new_W19$Temp <- "19°C"

new_W21 <- data.frame(
  Conc = exp(seq(log(min(W_21$Conc)),
                 log(max(W_21$Conc)),
                 length.out = 200))
)

pred21 <- predict(model_W21,
                  newdata = new_W21,
                  interval = "confidence")

new_W21$fit <- pred21[,1]
new_W21$lwr <- pred21[,2]
new_W21$upr <- pred21[,3]
new_W21$Temp <- "21°C"


## Combine predictions and raw data
pred_all_W <- rbind(new_W17, new_W19, new_W21)

W_17$Temp <- "17°C"
W_19$Temp <- "19°C"
W_21$Temp <- "21°C"

raw_all_W <- rbind(W_17, W_19, W_21)

# Plot
DR_W= ggplot() +
  
  geom_ribbon(
    data = pred_all_W,
    aes(x = Conc,
        ymin = lwr,
        ymax = upr,
        fill = Temp),
    alpha = 0.2,
    colour = NA
  ) +
  
  geom_line(
    data = pred_all_W,
    aes(x = Conc,
        y = fit,
        colour = Temp),
    linewidth = 1.2
  ) +
  
  geom_point(
    data = raw_all_W,
    aes(x = Conc,
        y = Surv,
        colour = Temp),
    size = 2,
    alpha = 0.7
  ) +
  
  scale_x_log10(
    limits = c(0.39, 12.5),
    breaks = c(0.2, 0.39, 0.78, 1.56, 3.13, 6.25, 12.5)
  )+
  
  coord_cartesian(ylim = c(0, 120)) +
  
  
  scale_colour_manual(
    values = c(
      "17°C" = "#1042B2",
      "19°C" = "#41D9A1",
      "21°C" = "#E27D60"
    )
  ) +
  
  scale_fill_manual(
    values = c(
      "17°C" = "#1042B2",
      "19°C" = "#41D9A1",
      "21°C" = "#E27D60"
    )
  ) +
  
  labs(
    x = "Elutriate (%)",
    y = "Survival, D-yield (%)",
    colour = "Temperature",
    fill = "Temperature"
  ) +
  
  theme_classic(base_size = 14)

DR_W


#---------------------Garnes sediment ----------------------------------------
## Fit each model
# Select appropriate model
model_G17_LL4 <- drm(Surv ~ Conc, data = G_17, fct = LL.4())
model_G17_LL3 <-drm(Surv ~ Conc, data = G_17, fct = LL.3())
model_G17_LL2 <- drm(Surv ~ Conc, data = G_17, fct = LL.2())
model_G17_W14 <- drm(Surv ~ Conc, data = G_17, fct = W1.4())
model_G17_BC4 <- drm(Surv ~ Conc, data = G_17, fct = BC.4())
model_G17_BC5 <- drm(Surv ~ Conc, data = G_17, fct = BC.5())

# Compare models using AIC
AIC(model_G17_LL4, model_G17_LL3, model_G17_LL2, model_G17_W14, model_G17_BC4, model_G17_BC5)


# Select appropriate model
model_G19_LL4 <- drm(Surv ~ Conc, data = G_19, fct = LL.4())
model_G19_LL3 <-drm(Surv ~ Conc, data = G_19, fct = LL.3())
model_G19_LL2 <- drm(Surv ~ Conc, data = G_19, fct = LL.2())
model_G19_W14 <- drm(Surv ~ Conc, data = G_19, fct = W1.4())
model_G19_BC4 <- drm(Surv ~ Conc, data = G_19, fct = BC.4())
model_G19_BC5 <- drm(Surv ~ Conc, data = G_19, fct = BC.5())

# Compare models using AIC
AIC(model_G19_LL4, model_G19_LL3, model_G19_LL2, model_G19_W14, model_G19_BC4, model_G19_BC5)

# Select appropriate model
model_G21_LL4 <- drm(Surv ~ Conc, data = G_21, fct = LL.4())
model_G21_LL3 <-drm(Surv ~ Conc, data = G_21, fct = LL.3())
model_G21_LL2 <- drm(Surv ~ Conc, data = G_21, fct = LL.2())
model_G21_W14 <- drm(Surv ~ Conc, data = G_21, fct = W1.4())
model_G21_BC4 <- drm(Surv ~ Conc, data = G_21, fct = BC.4())
model_G21_BC5 <- drm(Surv ~ Conc, data = G_21, fct = BC.5())

# Compare models using AIC
AIC(model_G21_LL4, model_G21_LL3, model_G21_LL2, model_G21_W14, model_G21_BC4, model_G21_BC5)

model_G17 <- drm(Surv ~ Conc, data = G_17, fct = LL.3())

# 3. Extract EC10 and EC50 with 95% CI
ed_vals <- ED(model_G17, c(10, 25, 50), interval = "delta")

G_17_EC <- data.frame(
  Sediment = "Garne Bay",
  Temperature = "17 °C",
  Model = "LL.3",
  EC10 = ed_vals[1, "Estimate"],
  EC10_low = ed_vals[1, "Lower"],
  EC10_high = ed_vals[1, "Upper"],
  EC10_SE = ed_vals[1, "Std. Error"],
  EC25 = ed_vals[2, "Estimate"],
  EC25_low = ed_vals[2, "Lower"],
  EC25_high = ed_vals[2, "Upper"],
  EC25_SE = ed_vals[2, "Std. Error"],
  EC50 = ed_vals[3, "Estimate"],
  EC50_low = ed_vals[3, "Lower"],
  EC50_high = ed_vals[3, "Upper"],
  EC50_SE = ed_vals[3, "Std. Error"]
)

model_G19 <- drm(Surv ~ Conc, data = G_19, fct = LL.3())

# 3. Extract EC10 and EC50 with 95% CI
ed_vals <- ED(model_G19, c(10, 25, 50), interval = "delta")

G_19_EC <- data.frame(
  Sediment = "Garne Bay",
  Temperature = "19 °C",
  Model = "LL.3",
  EC10 = ed_vals[1, "Estimate"],
  EC10_low = ed_vals[1, "Lower"],
  EC10_high = ed_vals[1, "Upper"],
  EC10_SE = ed_vals[1, "Std. Error"],
  EC25 = ed_vals[2, "Estimate"],
  EC25_low = ed_vals[2, "Lower"],
  EC25_high = ed_vals[2, "Upper"],
  EC25_SE = ed_vals[2, "Std. Error"],
  EC50 = ed_vals[3, "Estimate"],
  EC50_low = ed_vals[3, "Lower"],
  EC50_high = ed_vals[3, "Upper"],
  EC50_SE = ed_vals[3, "Std. Error"]
)

model_G21 <- drm(Surv ~ Conc, data = G_21, fct = LL.3())

# 3. Extract EC10 and EC50 with 95% CI
ed_vals <- ED(model_G21, c(10, 25, 50), interval = "delta")

G_21_EC <- data.frame(
  Sediment = "Garne Bay",
  Temperature = "21 °C",
  Model = "LL.3",
  EC10 = ed_vals[1, "Estimate"],
  EC10_low = ed_vals[1, "Lower"],
  EC10_high = ed_vals[1, "Upper"],
  EC10_SE = ed_vals[1, "Std. Error"],
  EC25 = ed_vals[2, "Estimate"],
  EC25_low = ed_vals[2, "Lower"],
  EC25_high = ed_vals[2, "Upper"],
  EC25_SE = ed_vals[2, "Std. Error"],
  EC50 = ed_vals[3, "Estimate"],
  EC50_low = ed_vals[3, "Lower"],
  EC50_high = ed_vals[3, "Upper"],
  EC50_SE = ed_vals[3, "Std. Error"]
)

# Create prediction data for each
new_G17 <- data.frame(
  Conc = exp(seq(log(min(G_17$Conc)),
                 log(max(G_17$Conc)),
                 length.out = 200))
)

pred17 <- predict(model_G17,
                  newdata = new_G17,
                  interval = "confidence")

new_G17$fit <- pred17[,1]
new_G17$lwr <- pred17[,2]
new_G17$upr <- pred17[,3]
new_G17$Temp <- "17°C"

new_G19 <- data.frame(
  Conc = exp(seq(log(min(G_19$Conc)),
                 log(max(G_19$Conc)),
                 length.out = 200))
)

pred19 <- predict(model_G19,
                  newdata = new_G19,
                  interval = "confidence")

new_G19$fit <- pred19[,1]
new_G19$lwr <- pred19[,2]
new_G19$upr <- pred19[,3]
new_G19$Temp <- "19°C"

new_G21 <- data.frame(
  Conc = exp(seq(log(min(G_21$Conc)),
                 log(max(G_21$Conc)),
                 length.out = 200))
)

pred21 <- predict(model_G21,
                  newdata = new_G21,
                  interval = "confidence")

new_G21$fit <- pred21[,1]
new_G21$lwr <- pred21[,2]
new_G21$upr <- pred21[,3]
new_G21$Temp <- "21°C"


## Combine predictions and raw data
pred_all_G <- rbind(new_G17, new_G19, new_G21)

G_17$Temp <- "17°C"
G_19$Temp <- "19°C"
G_21$Temp <- "21°C"

raw_all_G <- rbind(G_17, G_19, G_21)

# Plot
DR_G= ggplot() +
  
  geom_ribbon(
    data = pred_all_G,
    aes(x = Conc,
        ymin = lwr,
        ymax = upr,
        fill = Temp),
    alpha = 0.2,
    colour = NA
  ) +
  
  geom_line(
    data = pred_all_G,
    aes(x = Conc,
        y = fit,
        colour = Temp),
    linewidth = 1.2
  ) +
  
  geom_point(
    data = raw_all_G,
    aes(x = Conc,
        y = Surv,
        colour = Temp),
    size = 2,
    alpha = 0.7
  ) +
  
  coord_cartesian(ylim = c(0, 120)) +
  
  scale_x_log10(
    limits = c(0.39, 12.5),
    breaks = c(0.2, 0.39, 0.78, 1.56, 3.13, 6.25, 12.5)
  )+
  
  
  scale_colour_manual(
    values = c(
      "17°C" = "#1042B2",
      "19°C" = "#41D9A1",
      "21°C" = "#E27D60"
    )
  ) +
  
  scale_fill_manual(
    values = c(
      "17°C" = "#1042B2",
      "19°C" = "#41D9A1",
      "21°C" = "#E27D60"
    )
  ) +
  
  labs(
    x = "Elutriate (%)",
    y = "Survival, D-yield (%)",
    colour = "Temperature",
    fill = "Temperature"
  ) +
  
  theme_classic(base_size = 14)

DR_G


# Combine all the EC data frames
EC_results_embryotox <- rbind(
  BB_17_EC,
  BB_19_EC,
  BB_21_EC,
  SK_17_EC,
  SK_19_EC,
  SK_21_EC,
  W_17_EC,
  W_19_EC,
  W_21_EC,
  G_17_EC,
  G_19_EC,
  G_21_EC
)

EC_results_embryotox


write.csv(
  EC_results_embryotox,
  "EC_results_SMS_embryotox_normalised.csv",
  row.names = FALSE
)


# Different plot to show diffs in EC50s
ec50_df=read.csv("EC_results_SMS_embryotox_normalised.csv")



## Remove extra axis labels
DR_G <- DR_G +
  theme(
    axis.title.x = element_blank()
  )

DR_W <- DR_W +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )

DR_SK <- DR_SK +
  theme(
    axis.title.y = element_blank()
  )

# Combined plot 

library(patchwork)

DR_combined <-
  (DR_G | DR_W) /
  (DR_BB | DR_SK) +
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = "A") &
  theme(
    legend.position = "bottom",
    plot.tag = element_text(
      size = 14,
      face = "bold"
    )
  )
DR_combined


ggsave(
  filename = "FIGURE2_DRcurves_normalised to control.png",
  plot = DR_combined,
  width = 11,
  height = 8,
  units = "in",
  dpi = 300
)

# Forest plot with sediment along top, temp on yaxis
library(tidyverse)

# Order strains however you prefer
ec50_df$Sediment <- factor(
  ec50_df$Sediment,
  levels = c("Beatrix Bay", "Skiddaw", "Waiona Bay", "Garne Bay")
)

ec50_df$Temperature <- factor(
  ec50_df$Temperature,
  levels = c("17 °C", "19 °C", "21 °C")
)

ec50_df_plot <- ec50_df %>%
  filter(
    !(Sediment == "Garne Bay" & Temperature == "19 °C"),
    !(Sediment == "Garne Bay" & Temperature == "17 °C")
  )

ec50_df_plot$Temperature <- factor(
  ec50_df_plot$Temperature,
  levels = c("21 °C", "19 °C", "17 °C")
)   

EC50_plot= ggplot(
  ec50_df_plot,
  aes(
    x = EC50,
    y = Temperature,
    colour = Temperature
  )
) +
  
  geom_errorbarh(
    aes(xmin = EC50_low,
        xmax = EC50_high),
    height = 0.15,
    linewidth = 0.7
  ) +
  
  geom_point(
    size = 3
  ) +
  
  scale_colour_manual(
    values = c(
      "17 °C" = "#1042B2",
      "19 °C" = "#41D9A1",
      "21 °C" = "#E27D60"
    )) +
  
  scale_x_continuous() +
  
  labs(
    x = expression(
      LC[50]~"(percentage (%) dilution of sediment elutriate)"
    ),
    y = NULL,
    colour = NULL
  ) +
  facet_wrap(~Sediment, nrow = 1)+
  
  theme_classic(base_size = 12) +
  theme(
    strip.text = element_text(
      size = 14,
      face = "bold"
    )
  )+
  
  
  theme(
    legend.position = "top",
    legend.justification = "left",
    
    legend.text = element_text(size = 11),
    
    axis.text.y = element_text(
      size = 13,
      face = "bold"
    ),
    
    axis.text.x = element_text(size = 12),
    
    axis.title.x = element_text(
      size = 13,
      face = "bold"
    ),
    
    axis.line = element_line(
      linewidth = 0.7
    ),
    
    plot.margin = margin(
      10, 10, 10, 10
    )
  )
EC50_plot

ggsave("LC50 forest plot SMS embryotox.png", plot = last_plot(),
       width = 8, height = 6, dpi = 600, bg="white")



# Forest plot with temp along top, sediment on yaxis
library(tidyverse)

# # Order strains however you prefer
# ec50_df$Sediment <- factor(
#   ec50_df$Sediment,
#   levels = c("Beatrix Bay", "Skiddaw", "Waiona Bay", "Garne Bay")
# )
# 
# ec50_df$Temperature <- factor(
#   ec50_df$Temperature,
#   levels = c("17 °C", "19 °C", "21 °C")
# )
# 
# ec50_df_plot <- ec50_df %>%
#   filter(
#     !(Sediment == "Garne Bay" & Temperature == "19 °C"),
#     !(Sediment == "Garne Bay" & Temperature == "17 °C")
#   )
# 
# ec50_df_plot$Temperature <- factor(
#   ec50_df_plot$Temperature,
#   levels = c("17 °C", "19 °C", "21 °C")
# )   
# 
# EC50_plot= ggplot(
#   ec50_df_plot,
#   aes(
#     x = EC50,
#     y = Sediment,
#     colour = Temperature
#   )
# ) +
#   
#   geom_errorbarh(
#     aes(xmin = EC50_low,
#         xmax = EC50_high),
#     height = 0.15,
#     linewidth = 0.8
#   ) +
#   
#   geom_point(
#     size = 3.5
#   ) +
#   
#   scale_colour_manual(
#     values = c(
#       "17 °C" = "#1042B2",
#       "19 °C" = "#41D9A1",
#       "21 °C" = "#E27D60"
#   )) +
#   
#   scale_x_continuous() +
#   
#   labs(
#     x = expression(
#       LC[50]~"(percentage (%) dilution of sediment elutriate)"
#     ),
#     y = NULL,
#     colour = NULL
#   ) +
#   facet_wrap(~Temperature, nrow = 1)+
#   
#   theme_classic(base_size = 12) +
#   theme(
#     strip.text = element_text(
#       size = 14,
#       face = "bold"
#     )
#   )+
#   
#   
#   theme(
#     legend.position = "top",
#     legend.justification = "left",
#     
#     legend.text = element_text(size = 11),
#     
#     axis.text.y = element_text(
#       size = 13,
#       face = "bold"
#     ),
#     
#     axis.text.x = element_text(size = 12),
#     
#     axis.title.x = element_text(
#       size = 13,
#       face = "bold"
#     ),
#     
#     axis.line = element_line(
#       linewidth = 0.8
#     ),
#     
#     plot.margin = margin(
#       10, 10, 10, 10
#     )
#   )
# EC50_plot
# 
# ggsave("LC50 forest plot SMS embryotox.png", plot = last_plot(),
#        width = 8, height = 6, dpi = 600, bg="white")


# Forest plot for Al equiv
# Different plot to show diffs in EC50s
lc50_df=read.csv("Al_LC50.csv")

lc50_df$Temperature <- factor(
  lc50_df$Temperature,
  levels = c(17, 19, 21),
  labels = c("17 °C", "19 °C", "21 °C")
)

library(ggplot2)
lc50_df$Site <- factor(
  lc50_df$Site,
  levels = c("Beatrix Bay", "Skiddaw", "Waiona Bay", "Garne Bay")
)

lc50_df$Temperature <- factor(
  lc50_df$Temperature,
  levels = c("21 °C", "19 °C", "17 °C")
)

lc50_df$Sediment <- lc50_df$Site

Al_LC50 <- ggplot(
  lc50_df,
  aes(
    x = LC50,
    y = Temperature,
    colour = Temperature
  )
) +
  
  geom_errorbarh(
    aes(
      xmin = Lower,
      xmax = Upper
    ),
    height = 0.15,
    linewidth = 0.8
  ) +
  
  geom_point(
    size = 3.5
  ) +
  
  scale_colour_manual(
    values = c(
      "17 °C" = "#1042B2",
      "19 °C" = "#41D9A1",
      "21 °C" = "#E27D60"
    )
  ) +
  
  labs(
    x = expression(
      LC[50]~"("*mu*"g Al eq. L"^{-1}*")"
    ),
    y = NULL,
    colour = NULL
  ) +
  
  facet_wrap(
    ~Sediment,
    nrow = 1
  ) +
  
  theme_classic(base_size = 12) +
  
  theme(
    strip.text = element_text(
      size = 14,
      face = "bold"
    ),
    
    legend.position = "top",
    legend.justification = "left",
    
    legend.text = element_text(
      size = 11
    ),
    
    axis.text.y = element_text(
      size = 13,
      face = "bold"
    ),
    
    axis.text.x = element_text(
      size = 12
    ),
    
    axis.title.x = element_text(
      size = 13,
      face = "bold"
    ),
    
    axis.line = element_line(
      linewidth = 0.7
    ),
    
    plot.margin = margin(
      10, 10, 10, 10
    )
  )
Al_LC50

ggsave("LC50 forest plot SMS embryotox.png", plot = last_plot(),
       width = 8, height = 6, dpi = 600, bg="white")


## Prep for 2-panel
Al_LC50 <- Al_LC50 +
  theme(
    strip.text = element_blank(),
    strip.background = element_blank()
  )

library(patchwork)

library(patchwork)

combined_plot <-
  (EC50_plot / Al_LC50) +
  plot_annotation(
    tag_levels = "A"
  ) &
  theme(
    legend.position = "none",
    plot.tag = element_text(
      size = 18,
      face = "bold"
    )
  )

combined_plot

# FIGURE_3 IN MANUSCRIPT
ggsave("Combined_forest_plot.png", plot = combined_plot,
       width = 9, height = 8, dpi = 600, bg="white")
