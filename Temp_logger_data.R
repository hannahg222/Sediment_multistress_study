library(tidyverse)
library(lubridate)
# EMBRYO EXPERIMENT
# Read data
temp <- read_csv("temperature_data_embryo.csv")

# Convert datetime
temp <- temp %>%
  mutate(
    date_time = mdy_hms(date_time)
  )

# Calculate hourly means
temp_hourly <- temp %>%
  mutate(Hour = floor_date(date_time, unit = "hour")) %>%
  group_by(Hour) %>%
  summarise(
    `17deg` = mean(`17deg`, na.rm = TRUE),
    `19deg` = mean(`19deg`, na.rm = TRUE),
    `21deg` = mean(`21deg`, na.rm = TRUE)
  )

# Convert to long format
temp_long <- temp_hourly %>%
  pivot_longer(
    cols = c(`17deg`, `19deg`, `21deg`),
    names_to = "Logger",
    values_to = "Temperature"
  )

# Plot
ggplot(temp_long,
       aes(x = Hour,
           y = Temperature,
           colour = Logger)) +
  geom_line(linewidth = 1) +
  scale_colour_manual(
    values = c(
      "17deg" = "#1042B2",
      "19deg" = "#41D9A1",
      "21deg" = "#E27D60"
    ),
    labels = c(
      "17 °C",
      "19 °C",
      "21 °C"
    )
  ) +
  labs(
    x = "Time",
    y = expression(Temperature ~ (degree * C)),
    colour = "Treatment"
  ) +
  theme_classic()+
  theme(
    axis.title = element_text(size = 13),
    axis.text = element_text(size = 11),
    legend.title = element_text(size = 13),
    legend.text = element_text(size = 11)
  )

ggsave("Embryotox_temploggers.png", plot = last_plot(),
       width = 9, height = 5, dpi = 600, bg="white")


# SPAT EXPERIMENT
temp_spat <- read_csv("temperature_data_spat.csv")

library(lubridate)

temp_spat <- temp_spat %>%
  mutate(
    dt = parse_date_time(
      date_time,
      orders = c(
        "Ymd HMS",    # 2023-12-08 23:00:00
        "mdy IMS p"   # 08/13/23 12:00:00 AM
      )
    )
  )

temp_daily <- temp_spat %>%
  group_by(Days) %>%
  summarise(
    Temp17 = mean(`17deg`, na.rm = TRUE),
    Temp21 = mean(`21deg`, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = c(Temp17, Temp21),
    names_to = "Treatment",
    values_to = "Temperature"
  )

ggplot(temp_daily,
       aes(x = Days,
           y = Temperature,
           colour = Treatment)) +
  
  annotate("rect",
           xmin = 0.5,
           xmax = 5.5,
           ymin = -Inf,
           ymax = Inf,
           fill = "grey80",
           alpha = 0.4) +
  
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  
  scale_colour_manual(
    values = c(
      "Temp17" = "#1042B2",
      "Temp21" = "#E27D60"
    ),
    labels = c(
      "17 °C",
      "21 °C"
    )
  ) +
  
  labs(
    x = "Experimental Day",
    y = expression(Temperature~(degree*C)),
    colour = "Treatment"
  ) +
  
  theme_classic()+
  theme(
    axis.title = element_text(size = 13),
    axis.text = element_text(size = 11),
    legend.title = element_text(size = 13),
    legend.text = element_text(size = 11)
  )



ggsave("Spat_temploggers.png", plot = last_plot(),
       width = 9, height = 5, dpi = 600, bg="white")
