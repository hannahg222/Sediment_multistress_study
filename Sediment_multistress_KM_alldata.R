library(dplyr)
library(tidyr)
library(purrr)
library(survival) 
packageVersion("survival")
packageVersion("survminer")
library(survminer)
library(RColorBrewer)
library(openxlsx)

setwd("C:/Users/hannahg/Cawthron/Hannah G PhD Team - Documents/General/R/Julien")

# Read in your dataset
AD <- read.csv("Sediment_temp_alldata_JV.csv")  # adjust if TSV/Excel
# Create one treatment variable
AD2 <- AD %>%
  mutate(
    Treatment = case_when(
      Sediment == "FSW" ~ 0,
      TRUE ~ as.numeric(gsub("%", "", Treatment))
    ),
    Treatment = factor(Treatment)  # treat as categorical
  )


######## FILTER DF TO REMOVE 19 DEGREE TREATMENT ###########
AD2 = AD2 %>% 
  filter(Temperature != "19")


#--------------------------------------------------
# 1. Check structure
#--------------------------------------------------
names(AD2)
str(AD2)

#--------------------------------------------------
# 2. Clean variable types
#--------------------------------------------------
AD2 <- AD2 %>%
  mutate(
    Time = as.numeric(Time),
    Tank = as.factor(Tank),
    Replicate = as.factor(Replicate),
    Temperature = as.factor(Temperature),
    Treatment = as.factor(Treatment),
    Initial = as.integer(Initial),
    Dead = as.integer(Dead),
    Total_dead = as.integer(Total_dead),
    Alive = as.integer(Alive)
  )

# Optional: check treatment levels
levels(AD2$Treatment)

#--------------------------------------------------
# 3. Consistency check
#    Does Initial - cumulative Dead match Alive?
#--------------------------------------------------
AD2_check <- AD2 %>%
  arrange(Tank, Time) %>%
  group_by(Tank) %>%
  mutate(
    Cum_dead_calc = cumsum(Dead),
    Alive_calc = first(Initial) - Cum_dead_calc,
    Match = Alive_calc == Alive
  ) %>%
  ungroup()


# Show any mismatches
Mismatches <- AD2_check %>%
  filter(!Match)
if (nrow(Mismatches) > 0) {
  message("Warning: some rows do not match calculated Alive values")
  print(Mismatches %>%
          select(Tank, Replicate, Temperature, Treatment, Time, Initial, Dead, Alive, Alive_calc))
} else {
  message("All Alive values match Initial - cumulative Dead")
}




#==================================================
# 1. Clean data types properly
#==================================================

AD2 <- AD2 %>%
  mutate(
    Time = as.numeric(Time),
    Tank = as.factor(Tank),
    Replicate = as.factor(Replicate),
    Temperature = as.factor(Temperature),
    Treatment = as.character(Treatment),
    Treatment = gsub("%", "", Treatment),
    Treatment = as.numeric(Treatment),
    Initial = as.integer(Initial),
    Dead = as.integer(Dead),
    Total_dead = as.integer(Total_dead),
    Alive = as.integer(Alive)
  )

# Check treatment values
sort(unique(AD2$Treatment))

#==================================================
# 2. Censor failed tank after day 2
#==================================================
failed_tanks <- tibble::tribble(
  ~Tank, ~Replicate, ~Temperature,
  "40",  "A",        "19",
  "41",  "B",        "19"
)

AD2_flagged <- AD2 %>%
  left_join(failed_tanks %>% mutate(Failed = TRUE),
            by = c("Tank", "Replicate", "Temperature")) %>%
  mutate(Failed = ifelse(is.na(Failed), FALSE, Failed))



AD2_censored <- AD2_flagged %>%
  mutate(
    Dead = ifelse(Failed & Time > 2, 0L, Dead)
  ) %>%
  group_by(Tank) %>%
  arrange(Time, .by_group = TRUE) %>%
  mutate(
    Total_dead = cumsum(Dead),
    Alive = first(Initial) - Total_dead
  ) %>%
  ungroup()





#==================================================
# 3. Tank-specific end times- keep only final time point
#==================================================
Tank_end_times <- AD2_censored %>%
  group_by(Tank, Replicate, Temperature, Sediment, Treatment) %>%
  summarise(
    Final_time = max(Time),
    .groups = "drop"
  )

#==================================================
# 4. Expand to one row per individual
#==================================================
Surv_data_censored <- AD2_censored %>%
  group_by(Tank, Replicate, Temperature, Sediment, Treatment) %>%
  summarise(
    Initial = first(Initial),
    Death_times = list(rep(Time, Dead)),
    .groups = "drop"
  ) %>%
  left_join(
    Tank_end_times,
    by = c("Tank", "Replicate", "Temperature", "Sediment", "Treatment")
  ) %>%
  mutate(
    N_dead = map_int(Death_times, length),
    N_censored = Initial - N_dead,
    Individual_data = map2(Death_times, N_censored, ~{
      Died_rows <- if (length(.x) > 0) {
        data.frame(Time = .x, Status = 1)
      } else {
        data.frame(Time = numeric(0), Status = integer(0))
      }
      
      Censored_rows <- if (.y > 0) {
        data.frame(Time = rep(NA_real_, .y), Status = 0)
      } else {
        data.frame(Time = numeric(0), Status = integer(0))
      }
      
      bind_rows(Died_rows, Censored_rows)
    })
  ) %>%
  rowwise() %>%
  mutate(
    Individual_data = list(
      Individual_data %>%
        mutate(Time = ifelse(is.na(Time), Final_time, Time))
    )
  ) %>%
  ungroup() %>%
  select(Tank, Replicate, Temperature, Sediment, Treatment, Individual_data) %>%
  unnest(Individual_data)

# Check treatments survived correctly
sort(unique(Surv_data_censored$Treatment))

# Check failed tank
Surv_data_censored %>%
  filter(Tank == "41") %>%
  count(Time, Status)

Surv_data_censored %>%
  filter(Tank == "40") %>%
  count(Time, Status)

Surv_data_censored <- Surv_data_censored %>%
  mutate(
    Treatment_f = factor(
      Treatment,
      levels = c(0, 3, 6, 13, 25, 50),
      labels = c("0%", "3%", "6%", "13%", "25%", "50%")
    )
  )

# --- Plot censored data for tank 41
fit <- survfit(Surv(Time, Status) ~ Treatment_f, data = Surv_data_censored)
summary(fit, Time=9)


ggsurvplot(
  fit,
  data = Surv_data_censored,
  conf.int = TRUE,
  pval = TRUE,
  legend.title = "Treatment"
)

# Get percentage survival of controls at each temp (regardless of location)
# for 17deg
control_17 <- Surv_data_censored %>%
  filter(
    Temperature == 17,
    Treatment_f == "0%"
  )

fit_control_17 <- survfit(
  Surv(Time, Status) ~ 1,
  data = control_17
)

summary(fit_control_17, times = 9)
# for 21 deg
control_21 <- Surv_data_censored %>%
  filter(
    Temperature == 21,
    Treatment_f == "0%"
  )

fit_control_21 <- survfit(
  Surv(Time, Status) ~ 1,
  data = control_21
)

summary(fit_control_21, times = 9)

# Does sediment location impact survival?
survdiff(
  Surv(Time, Status) ~ Sediment,
  data = filter(Surv_data_censored,
                Treatment_f == "0%")
)

# Formally test control curves at each temp
control_only <- Surv_data_censored %>%
  filter(Treatment_f == "0%")

survdiff(
  Surv(Time, Status) ~ Temperature,
  data = control_only
)

# Or plot the pooled control curves
fit_controls <- survfit(
  Surv(Time, Status) ~ Temperature,
  data = filter(Surv_data_censored,
                Treatment_f == "0%")
)

ggsurvplot(
  fit_controls,
  surv.scale = "percent",
  conf.int = TRUE,
  pval = TRUE
)

summary(fit_controls, times = 9)

lr <- survdiff(
  Surv(Time, Status) ~ Temperature,
  data = control_only
)

lr

# extract X2 and p
chisq <- lr$chisq

p <- pchisq(
  chisq,
  df = length(lr$n) - 1,
  lower.tail = FALSE
)

chisq
p

# this model treats treatment as factor i.e. captures non linear effects
Cox_fit_censored_factor= coxph(Surv(Time, Status) ~ factor(Treatment) * Temperature + cluster(Tank), 
                               data= Surv_data_censored)
summary(Cox_fit_censored_factor)



Surv_data_censored <- Surv_data_censored %>%
  mutate(
    Treatment_f = factor(
      Treatment,
      levels = c(0, 3, 6, 13, 25, 50),
      labels = c("0%", "3%", "6%", "13%", "25%", "50%")
    )
  )

ggsurvplot(
  survfit(Surv(Time, Status) ~ Treatment_f, data = Surv_data_censored),
  data = Surv_data_censored,
  facet.by = "Sediment",
  conf.int = FALSE,
  legend.title = "Treatment"
)


# # ---- Filter survival data to just 0, 25 and 50 ---
# Surv_data_sub <- Surv_data_censored %>%
#   filter(Treatment %in% c(0, 25, 50))
# 
# fit_sub <- survfit(Surv(Time, Status) ~ Treatment, data = Surv_data_sub)
# 
# ggsurvplot(fit_sub, data = Surv_data_sub)
# 
# ggsurvplot(
#   fit_sub,
#   data = Surv_data_sub,
#   facet.by = "Temperature",
#   conf.int = FALSE,
#   legend.title = "Treatment"
# )
# 
# 
# # ---- Filter survival data to just BB ---
# Surv_data_BB <- Surv_data_censored %>%
#   filter(Sediment == "BB") %>%
#   mutate(
#     Temperature = factor(
#       as.character(Temperature),
#       levels = c("17", "21"),
#       labels = c(
#         "A. Beatrix Bay sediment (17°C)",
#         "B. Beatrix Bay sediment (21°C)"
#       )
#     )
#   )
# 
# levels(Surv_data_BB$Temperature)
# 
# fit_sub_BB <- survfit(Surv(Time, Status) ~ Treatment_f, data = Surv_data_BB)
# summary(fit_sub_BB, Time=9)
# # ggsurvplot(fit_sub_BB, data = Surv_data_BB)
# # 
# # ggsurvplot(
# #   fit_sub_BB,
# #   data = Surv_data_BB,
# #   facet.by = "Temperature",
# #   conf.int = FALSE,
# #   legend.title = "Treatment"
# # )
# 
# p_BB <- ggsurvplot(
#   fit_sub_BB,
#   data = Surv_data_BB,
#   facet.by = "Temperature",
#   conf.int = FALSE,
#   legend.title = "Sediment concentration (%)",   # new legend title
#   legend.labs = c("0%", "3%", "6%", "13%", "25%", "50%"),  # custom labels
#   palette = c("#E27D60", "#41B3A3", "#85C1E9", "#F4D03F", "#AF7AC5", "#0072B2"), # custom colors
#   linetype = "solid",      # line style
#   censor.shape = 124,      # shape for censored points
#   censor.size = 3,         # size of censor marks
#   surv.scale = "percent",  # y-axis in %
#   xlab = "Time (days)",
#   ylab = "Survival probability")
# p_BB
# 
# 
# p_BB = p_BB+
#   theme_classic() +
#   theme(
#     strip.background = element_rect(fill = "#f0f0f0", color = NA),
#     strip.text = element_text(size = 13, face = "bold"),
#     axis.title = element_text(size = 13, face = "bold"),
#     axis.text = element_text(size = 12),
#     legend.position = "bottom",
#     legend.title = element_text(size = 13, face = "bold"),
#     legend.text = element_text(size = 12)
#   )+
#   coord_cartesian(ylim = c(0.3, 1)) +
#   ggplot2::facet_grid(
#     cols = ggplot2::vars(Temperature),
#     labeller = ggplot2::label_value
#   )
# 
# # ---- Filter survival data to just SK ---
# 
# Surv_data_SK <- Surv_data_censored %>%
#   filter(Sediment == "SK") %>%
#   mutate(
#     Temperature = factor(
#       as.character(Temperature),
#       levels = c("17", "21"),
#       labels = c(
#         "C. Skiddaw sediment (17°C)",
#         "D. Skiddaw sediment (21°C)"
#       )
#     )
#   )
# 
# fit_sub_SK <- survfit(Surv(Time, Status) ~ Treatment_f, data = Surv_data_SK)
# summary(fit_sub_SK, Time=9)
# class(fit_sub_SK)
# 
# 
# 
# p_SK <- ggsurvplot(
#   fit_sub_SK,
#   data = Surv_data_SK,
#   facet.by = "Temperature",
#   conf.int = FALSE,
#   legend.title = "Sediment concentration (%)",   # new legend title
#   legend.labs = c("0%", "3%", "6%", "13%", "25%", "50%"),  # custom labels
#   palette = c("#E27D60", "#41B3A3", "#85C1E9", "#F4D03F", "#AF7AC5", "#0072B2"), # custom colors
#   linetype = "solid",      # line style
#   censor.shape = 124,      # shape for censored points
#   censor.size = 3,         # size of censor marks
#   surv.scale = "percent",  # y-axis in %
#   xlab = "Time (days)",
#   ylab = "Survival probability")
#  p_SK
#  
# 
# p_SK= p_SK +
#   theme_classic() +
#   theme(
#     strip.background = element_rect(fill = "#f0f0f0", color = NA),
#     strip.text = element_text(size = 13, face = "bold"),
#     axis.title = element_text(size = 13, face = "bold"),
#     axis.text = element_text(size = 12),
#     legend.position = "bottom",
#     legend.title = element_text(size = 13, face = "bold"),
#     legend.text = element_text(size = 12)
#   )+
#   coord_cartesian(ylim = c(0.3, 1))+
#   facet_grid(
#     cols = vars(Temperature),
#     labeller = label_value
#   )
# 
# ####### Try to generate a 4 panel plot
# 
# library(patchwork)
# 
# combined_plot <- p_BB / p_SK +
#   plot_layout(guides = "collect") &
#   theme(
#     legend.position = "top"
#   )
# 
# combined_plot
############## FOLLOWING CODE USED IN JV MANUSCRIPT ###################
#######################################################################

############ THIS TIME FILTERING BY SEDIMENT AND TEMPRATURE TO GENERATE INDIVIDUAL KM CURVES ################
# ---- Filter survival data to just BB and 17deg ---

BB_17 <- Surv_data_censored %>%
  filter(
    Sediment == "BB",
    Temperature == 17
  )

fit_BB_17 <- survfit(
  Surv(Time, Status) ~ Treatment_f,
  data = BB_17
)
summary(fit_BB_17, Time=9)

p_BB_17 <- ggsurvplot(
  fit_BB_17,
  data = BB_17,
  conf.int = FALSE,
  title = "A. Beatrix Bay sediment (17°C)",
  legend.title = "Sediment concentration (%)",
  legend.labs = c("0%", "3%", "6%", "13%", "25%", "50%"),
  palette = c(
    "#E27D60", "#41D9A1", "#85C1E9",
    "#F4D03F", "#AF7AC5", "#1042B2"
  ),
  linetype = "solid",
  censor.shape = 124,
  censor.size = 3,
  surv.scale = "percent",
  xlab = "Time (days)",
  ylab = "Survival probability",
  ylim = c(0.3, 1)
)

p_BB_17

# Survival of control
fit_control <- survfit(
  Surv(Time, Status) ~ 1,
  data = filter(BB_17, Treatment_f == "0%")
)

summary(fit_control, times = 9)

#Survival for all concs simultaneously
fit <- survfit(
  Surv(Time, Status) ~ Treatment_f,
  data = BB_17
)

summary(fit, times = 9)

# ---- Filter survival data to just BB and 21deg ---

BB_21 <- Surv_data_censored %>%
filter(Sediment == "BB", Temperature == 21)

fit_BB_21 <- survfit(
  Surv(Time, Status) ~ Treatment_f,
  data = BB_21
)


p_BB_21 <- ggsurvplot(
  fit_BB_21,
  data = BB_21,
  conf.int = FALSE,
  title = "A. Beatrix Bay sediment (17°C)",
  legend.title = "Sediment concentration (%)",
  legend.labs = c("0%", "3%", "6%", "13%", "25%", "50%"),
  palette = c(
    "#E27D60", "#41D9A1", "#85C1E9",
    "#F4D03F", "#AF7AC5", "#1042B2"
  ),
  linetype = "solid",
  censor.shape = 124,
  censor.size = 3,
  surv.scale = "percent",
  xlab = "Time (days)",
  ylab = "Survival probability",
  ylim = c(0.3, 1)
)

p_BB_21

fit_control_BB21 <- survfit(
  Surv(Time, Status) ~ 1,
  data = filter(BB_21, Treatment_f == "0%")
)

summary(fit_control_BB21, times = 9)

# ---- Filter survival data to just SK and 17deg ---
SK_17 <- Surv_data_censored %>%
  filter(
    Sediment == "SK",
    Temperature == 17
  )

fit_SK_17 <- survfit(
  Surv(Time, Status) ~ Treatment_f,
  data = SK_17
)
p_SK_17 <- ggsurvplot(
  fit_SK_17,
  data = SK_17,
  conf.int = FALSE,
  title = "C. Skiddaw sediment (17°C)",
  legend.title = "Sediment concentration (%)",
  legend.labs = c("0%", "3%", "6%", "13%", "25%", "50%"),
  palette = c(
    "#E27D60", "#41D9A1", "#85C1E9",
    "#F4D03F", "#AF7AC5", "#1042B2"
  ),
  linetype = "solid",
  censor.shape = 124,
  censor.size = 3,
  surv.scale = "percent",
  xlab = "Time (days)",
  ylab = "Survival probability",
  ylim = c(0.3, 1)
)

p_SK_17

fit_control_SK17 <- survfit(
  Surv(Time, Status) ~ 1,
  data = filter(SK_17, Treatment_f == "0%")
)

summary(fit_control_SK17, times = 9)

# ---- Filter survival data to just SK and 21deg ---
SK_21 <- Surv_data_censored %>%
  filter(
    Sediment == "SK",
    Temperature == 21
  )

fit_SK_21 <- survfit(
  Surv(Time, Status) ~ Treatment_f,
  data = SK_21
)
p_SK_21 <- ggsurvplot(
  fit_SK_21,
  data = SK_21,
  conf.int = FALSE,
  title = "C. Skiddaw sediment (21°C)",
  legend.title = "Sediment concentration (%)",
  legend.labs = c("0%", "3%", "6%", "13%", "25%", "50%"),
  palette = c(
    "#E27D60", "#41D9A1", "#85C1E9",
    "#F4D03F", "#AF7AC5", "#1042B2"
  ),
  linetype = "solid",
  censor.shape = 124,
  censor.size = 3,
  surv.scale = "percent",
  xlab = "Time (days)",
  ylab = "Survival probability",
  ylim = c(0.3, 1)
)

p_SK_21

fit_control_SK21 <- survfit(
  Surv(Time, Status) ~ 1,
  data = filter(SK_21, Treatment_f == "0%")
)

summary(fit_control_SK21, times = 9)

#### Combined to grid- FIGURE 3
g_BB_17 <- p_BB_17$plot
g_BB_21 <- p_BB_21$plot
g_SK_17 <- p_SK_17$plot
g_SK_21 <- p_SK_21$plot


g_BB_17 <- g_BB_17 +
  labs(title = "Beatrix Bay sediment (17 °C)")

g_BB_21 <- g_BB_21 +
  labs(title = "Beatrix Bay sediment (21 °C)")

g_SK_17 <- g_SK_17 +
  labs(title = "Skiddaw sediment (17 °C)")

g_SK_21 <- g_SK_21 +
  labs(title = "Skiddaw sediment (21 °C)")

plot_theme <- theme_classic() +
  theme(
    plot.title = element_text(
      size = 12,
      face = "bold",
      hjust = 0.5
    ),
    axis.title = element_text(
      size = 12,
      face = "bold"
    ),
    axis.text = element_text(size = 11),
    legend.title = element_text(
      size = 12,
      face = "bold"
    ),
    legend.text = element_text(size = 11)
  )

g_BB_17 <- g_BB_17 + plot_theme
g_BB_21 <- g_BB_21 + plot_theme
g_SK_17 <- g_SK_17 + plot_theme
g_SK_21 <- g_SK_21 + plot_theme


library(patchwork)

combined_plot_2 <-
  (g_BB_17 | g_BB_21) /
  (g_SK_17 | g_SK_21) +
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = "A") &
  theme(
    legend.position = "right",
    plot.tag = element_text(
      size = 14,
      face = "bold"
    )
  )

combined_plot_2

ggsave(
  filename = "FIGURE4_four_panel_survival_plot.png",
  plot = combined_plot_2,
  width = 11,
  height = 8,
  units = "in",
  dpi = 300
)


############# STATS ON CURVES
BB_17$Treatment_f <- relevel(
  factor(BB_17$Treatment_f),
  ref = "0%"
)

BB_21$Treatment_f <- relevel(
  factor(BB_21$Treatment_f),
  ref = "0%"
)

SK_17$Treatment_f <- relevel(
  factor(SK_17$Treatment_f),
  ref = "0%"
)

SK_21$Treatment_f <- relevel(
  factor(SK_21$Treatment_f),
  ref = "0%"
)


# Fit cox models
library(survival)

cox_BB_17 <- coxph(
  Surv(Time, Status) ~ Treatment_f+
    cluster(Replicate),
  data = BB_17,
  ties = "efron",
  x = TRUE
)

cox_BB_21 <- coxph(
  Surv(Time, Status) ~ Treatment_f+
    cluster(Replicate),
  ties = "efron",
  x = TRUE
)

cox_SK_17 <- coxph(
  Surv(Time, Status) ~ Treatment_f+
    cluster(Replicate),
  ties = "efron",
  x = TRUE
)

cox_SK_21 <- coxph(
  Surv(Time, Status) ~ Treatment_f+
    cluster(Replicate),
  ties = "efron",
  x = TRUE
)

library(broom)
library(dplyr)

# Extract hazard ratios, CIs and p-vals
results_BB_17 <- tidy(
  cox_BB_17,
  exponentiate = TRUE,
  conf.int = TRUE
) %>%
  mutate(
    Panel = "Beatrix Bay sediment (17°C)"
  )

results_BB_21 <- tidy(
  cox_BB_21,
  exponentiate = TRUE,
  conf.int = TRUE
) %>%
  mutate(
    Panel = "Beatrix Bay sediment (21°C)"
  )

results_SK_17 <- tidy(
  cox_SK_17,
  exponentiate = TRUE,
  conf.int = TRUE
) %>%
  mutate(
    Panel = "Skiddaw sediment (17°C)"
  )

results_SK_21 <- tidy(
  cox_SK_21,
  exponentiate = TRUE,
  conf.int = TRUE
) %>%
  mutate(
    Panel = "Skiddaw sediment (21°C)"
  )

# Combine results
cox_results <- bind_rows(
  results_BB_17,
  results_BB_21,
  results_SK_17,
  results_SK_21
) %>%
  select(
    Panel,
    term,
    estimate,
    conf.low,
    conf.high,
    p.value
  ) %>%
  rename(
    Treatment = term,
    HR = estimate,
    CI_low = conf.low,
    CI_high = conf.high,
    p_unadjusted = p.value
  )

cox_results

cox_results <- cox_results %>%
  mutate(
    p_adjusted = p.adjust(
      p_unadjusted,
      method = "holm"
    )
  )
# Asterixes for use on plots
cox_results <- cox_results %>%
  group_by(Panel) %>%
  mutate(
    p_adjusted = p.adjust(
      p_unadjusted,
      method = "holm"
    )
  ) %>%
  ungroup() %>%
  mutate(
    significance = case_when(
      p_adjusted < 0.001 ~ "***",
      p_adjusted < 0.01  ~ "**",
      p_adjusted < 0.05  ~ "*",
      TRUE               ~ ""
    )
  )

cox_results %>%
  select(
    Panel,
    Treatment,
    HR,
    CI_low,
    CI_high,
    p_unadjusted,
    p_adjusted,
    significance
  )

# Test cox proportional hazards model
ph_BB_17 <- cox.zph(cox_BB_17)
ph_BB_21 <- cox.zph(cox_BB_21)
ph_SK_17 <- cox.zph(cox_SK_17)
ph_SK_21 <- cox.zph(cox_SK_21)

ph_BB_17
ph_BB_21
ph_SK_17
ph_SK_21

plot(ph_BB_17)
plot(ph_BB_21)
plot(ph_SK_17)
plot(ph_SK_21)




## Model comparisons
m1 <- coxph(
  Surv(Time, Status) ~ factor(Treatment) * Temperature + Sediment + cluster(Tank),
  data = Surv_data_censored
)
m1

m2 <- coxph(
  Surv(Time, Status) ~ factor(Treatment) * Temperature + Sediment +
    Sediment:factor(Treatment) + cluster(Tank),
  data = Surv_data_censored
)
m2

m3 <- coxph(
  Surv(Time, Status) ~ factor(Treatment) * Temperature + Sediment +
    Sediment:Temperature + cluster(Tank),
  data = Surv_data_censored
)

############ THIS TIME FILTERING BY SEDIMENT AND TEMPRATURE TO GENERATE INDIVIDUAL KM CURVES ################
# ---- Filter survival data to just BB and 17deg ---

BB_17 <- Surv_data_censored %>%
  filter(
    Sediment == "BB",
    Temperature == 17
  )

fit_BB_17 <- survfit(
  Surv(Time, Status) ~ Treatment_f,
  data = BB_17
)
p_BB_17 <- ggsurvplot(
  fit_BB_17,
  data = BB_17,
  conf.int = FALSE,
  title = "A. Beatrix Bay sediment (17°C)",
  legend.title = "Sediment concentration (%)",
  legend.labs = c("0%", "3%", "6%", "13%", "25%", "50%"),
  palette = c(
    "#E27D60", "#41D9A1", "#85C1E9",
    "#F4D03F", "#AF7AC5", "#1042B2"
  ),
  linetype = "solid",
  censor.shape = 124,
  censor.size = 3,
  surv.scale = "percent",
  xlab = "Time (days)",
  ylab = "Survival probability",
  ylim = c(0.3, 1)
)

p_BB_17

# ---- Filter survival data to just BB and 21deg ---

BB_21 <- Surv_data_censored %>%
filter(Sediment == "BB", Temperature == 21)

fit_BB_21 <- survfit(
  Surv(Time, Status) ~ Treatment_f,
  data = BB_21
)


p_BB_21 <- ggsurvplot(
  fit_BB_21,
  data = BB_21,
  conf.int = FALSE,
  title = "A. Beatrix Bay sediment (17°C)",
  legend.title = "Sediment concentration (%)",
  legend.labs = c("0%", "3%", "6%", "13%", "25%", "50%"),
  palette = c(
    "#E27D60", "#41D9A1", "#85C1E9",
    "#F4D03F", "#AF7AC5", "#1042B2"
  ),
  linetype = "solid",
  censor.shape = 124,
  censor.size = 3,
  surv.scale = "percent",
  xlab = "Time (days)",
  ylab = "Survival probability",
  ylim = c(0.3, 1)
)

p_BB_21



# ---- Filter survival data to just SK and 17deg ---
SK_17 <- Surv_data_censored %>%
  filter(
    Sediment == "SK",
    Temperature == 17
  )

fit_SK_17 <- survfit(
  Surv(Time, Status) ~ Treatment_f,
  data = SK_17
)
p_SK_17 <- ggsurvplot(
  fit_SK_17,
  data = SK_17,
  conf.int = FALSE,
  title = "C. Skiddaw sediment (17°C)",
  legend.title = "Sediment concentration (%)",
  legend.labs = c("0%", "3%", "6%", "13%", "25%", "50%"),
  palette = c(
    "#E27D60", "#41D9A1", "#85C1E9",
    "#F4D03F", "#AF7AC5", "#1042B2"
  ),
  linetype = "solid",
  censor.shape = 124,
  censor.size = 3,
  surv.scale = "percent",
  xlab = "Time (days)",
  ylab = "Survival probability",
  ylim = c(0.3, 1)
)

p_SK_17

# ---- Filter survival data to just SK and 21deg ---
SK_21 <- Surv_data_censored %>%
  filter(
    Sediment == "SK",
    Temperature == 21
  )

fit_SK_21 <- survfit(
  Surv(Time, Status) ~ Treatment_f,
  data = SK_21
)
p_SK_21 <- ggsurvplot(
  fit_SK_21,
  data = SK_21,
  conf.int = FALSE,
  title = "C. Skiddaw sediment (21°C)",
  legend.title = "Sediment concentration (%)",
  legend.labs = c("0%", "3%", "6%", "13%", "25%", "50%"),
  palette = c(
    "#E27D60", "#41D9A1", "#85C1E9",
    "#F4D03F", "#AF7AC5", "#1042B2"
  ),
  linetype = "solid",
  censor.shape = 124,
  censor.size = 3,
  surv.scale = "percent",
  xlab = "Time (days)",
  ylab = "Survival probability",
  ylim = c(0.3, 1)
)

p_SK_21

#### Combined to grid- FIGURE 3
g_BB_17 <- p_BB_17$plot
g_BB_21 <- p_BB_21$plot
g_SK_17 <- p_SK_17$plot
g_SK_21 <- p_SK_21$plot


g_BB_17 <- g_BB_17 +
  labs(title = "Beatrix Bay sediment (17°C)")

g_BB_21 <- g_BB_21 +
  labs(title = "Beatrix Bay sediment (21°C)")

g_SK_17 <- g_SK_17 +
  labs(title = "Skiddaw sediment (17°C)")

g_SK_21 <- g_SK_21 +
  labs(title = "Skiddaw sediment (21°C)")

plot_theme <- theme_classic() +
  theme(
    plot.title = element_text(
      size = 12,
      face = "bold",
      hjust = 0.5
    ),
    axis.title = element_text(
      size = 12,
      face = "bold"
    ),
    axis.text = element_text(size = 11),
    legend.title = element_text(
      size = 12,
      face = "bold"
    ),
    legend.text = element_text(size = 11)
  )

g_BB_17 <- g_BB_17 + plot_theme
g_BB_21 <- g_BB_21 + plot_theme
g_SK_17 <- g_SK_17 + plot_theme
g_SK_21 <- g_SK_21 + plot_theme


library(patchwork)

combined_plot_2 <-
  (g_BB_17 | g_BB_21) /
  (g_SK_17 | g_SK_21) +
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = "A") &
  theme(
    legend.position = "bottom",
    plot.tag = element_text(
      size = 14,
      face = "bold"
    )
  )

combined_plot_2

ggsave(
  filename = "FIGURE2_four_panel_survival_plot.png",
  plot = combined_plot_2,
  width = 11,
  height = 8,
  units = "in",
  dpi = 300
)


############# STATS ON CURVES
BB_17$Treatment_f <- relevel(
  factor(BB_17$Treatment_f),
  ref = "0%"
)

BB_21$Treatment_f <- relevel(
  factor(BB_21$Treatment_f),
  ref = "0%"
)

SK_17$Treatment_f <- relevel(
  factor(SK_17$Treatment_f),
  ref = "0%"
)

SK_21$Treatment_f <- relevel(
  factor(SK_21$Treatment_f),
  ref = "0%"
)


# Fit cox models
library(survival)

cox_BB_17 <- coxph(
  Surv(Time, Status) ~ Treatment_f+
    cluster(Replicate),
  data = BB_17,
  ties = "efron",
  x = TRUE
)

cox_BB_21 <- coxph(
  Surv(Time, Status) ~ Treatment_f+
    cluster(Replicate),
  data = BB_21,
  ties = "efron",
  x = TRUE
)

cox_SK_17 <- coxph(
  Surv(Time, Status) ~ Treatment_f+
    cluster(Replicate),
  data = SK_17,
  ties = "efron",
  x = TRUE
)

cox_SK_21 <- coxph(
  Surv(Time, Status) ~ Treatment_f+
    cluster(Replicate),
  data= SK_21,
  ties = "efron",
  x = TRUE
)

library(broom)
library(dplyr)

# Extract hazard ratios, CIs and p-vals
results_BB_17 <- tidy(
  cox_BB_17,
  exponentiate = TRUE,
  conf.int = TRUE
) %>%
  mutate(
    Panel = "Beatrix Bay sediment (17°C)"
  )

results_BB_21 <- tidy(
  cox_BB_21,
  exponentiate = TRUE,
  conf.int = TRUE
) %>%
  mutate(
    Panel = "Beatrix Bay sediment (21°C)"
  )

results_SK_17 <- tidy(
  cox_SK_17,
  exponentiate = TRUE,
  conf.int = TRUE
) %>%
  mutate(
    Panel = "Skiddaw sediment (17°C)"
  )

results_SK_21 <- tidy(
  cox_SK_21,
  exponentiate = TRUE,
  conf.int = TRUE
) %>%
  mutate(
    Panel = "Skiddaw sediment (21°C)"
  )

# Combine results
cox_results <- bind_rows(
  results_BB_17,
  results_BB_21,
  results_SK_17,
  results_SK_21
) %>%
  select(
    Panel,
    term,
    estimate,
    conf.low,
    conf.high,
    p.value
  ) %>%
  rename(
    Treatment = term,
    HR = estimate,
    CI_low = conf.low,
    CI_high = conf.high,
    p_unadjusted = p.value
  )

cox_results

cox_results <- cox_results %>%
  mutate(
    p_adjusted = p.adjust(
      p_unadjusted,
      method = "holm"
    )
  )
# Asterixes for use on plots
cox_results <- cox_results %>%
  group_by(Panel) %>%
  mutate(
    p_adjusted = p.adjust(
      p_unadjusted,
      method = "holm"
    )
  ) %>%
  ungroup() %>%
  mutate(
    significance = case_when(
      p_adjusted < 0.001 ~ "***",
      p_adjusted < 0.01  ~ "**",
      p_adjusted < 0.05  ~ "*",
      TRUE               ~ ""
    )
  )

cox_results %>%
  select(
    Panel,
    Treatment,
    HR,
    CI_low,
    CI_high,
    p_unadjusted,
    p_adjusted,
    significance
  )

# Test cox proportional hazards model
ph_BB_17 <- cox.zph(cox_BB_17)
ph_BB_21 <- cox.zph(cox_BB_21)
ph_SK_17 <- cox.zph(cox_SK_17)
ph_SK_21 <- cox.zph(cox_SK_21)

ph_BB_17
ph_BB_21
ph_SK_17
ph_SK_21

plot(ph_BB_17)
plot(ph_BB_21)
plot(ph_SK_17)
plot(ph_SK_21)




## Model comparisons
# Test dose of sediment 
cox_dose <- coxph(
  Surv(Time, Status) ~ Treatment + Temperature + Sediment,
  data = Surv_data_censored
)

summary(cox_dose)

# Is skiddaw more toxic than BB?
cox_sed <- coxph(
  Surv(Time, Status) ~ Treatment + Temperature + Sediment,
  data = filter(
    Surv_data_censored,
    Sediment %in% c("BB", "SK")
  )
)

summary(cox_sed)

# Did all 3 factors interact on spat survival?
cox_3way <- coxph(
  Surv(Time, Status) ~
    Treatment * Temperature * Sediment,
  data = filter(
    Surv_data_censored,
    Sediment %in% c("BB","SK")
  )
)

summary(cox_3way)


cox_2way <- coxph(
  Surv(Time, Status) ~
    (Treatment + Temperature + Sediment)^2,
  data = filter(
    Surv_data_censored,
    Sediment %in% c("BB","SK")
  )
)

anova(cox_2way, cox_3way, test = "LRT")




m1 <- coxph(
  Surv(Time, Status) ~ factor(Treatment) * Temperature + Sediment + cluster(Tank),
  data = Surv_data_censored
)
m1

m2 <- coxph(
  Surv(Time, Status) ~ factor(Treatment) * Temperature + Sediment +
    Sediment:factor(Treatment) + cluster(Tank),
  data = Surv_data_censored
)
m2

m3 <- coxph(
  Surv(Time, Status) ~ factor(Treatment) * Temperature + Sediment +
    Sediment:Temperature + cluster(Tank),
  data = Surv_data_censored
)


# Table of proportion survival at day 9
library(dplyr)

surv_day9 <- Surv_data_censored %>%
  group_by(Temperature, Sediment, Treatment_f, Replicate) %>%
  summarise(
    Survival = mean(Status == 0) * 100,
    .groups = "drop"
  )

# Mean of the two reps
surv_summary <- surv_day9 %>%
  group_by(Temperature, Sediment, Treatment_f) %>%
  summarise(
    MeanSurvival = mean(Survival),
    SD = sd(Survival),
    N = n(),
    .groups = "drop"
  )

write.csv(surv_summary, "Proportion_survival_SMS.csv")


packageVersion("survival")
packageVersion("ggsurvplot")
packageVersion("broom")
packageVersion("dplyr")
packageVersion("patchwork")