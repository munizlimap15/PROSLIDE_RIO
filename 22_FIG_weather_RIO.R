library(tidyverse)
library(arrow)
library(lubridate)
library(patchwork)
library(scales)

# Load datasets
temp_min <- open_dataset("C:/Users/pedro/Downloads/weather_R_saldanha/daily/Tmin_3.2.3.parquet")
temp_max <- open_dataset("C:/Users/pedro/Downloads/weather_R_saldanha/daily/Tmax_3.2.3.parquet")
precip   <- open_dataset("C:/Users/pedro/Downloads/weather_R_saldanha/daily/pr_3.2.3.parquet")

get_indicator_mean <- function(dataset, name_filter, code = 3304557, start_year = 1961) {
  dataset |>
    filter(code_muni == code) |>
    filter(year(date) >= start_year) |>
    filter(name == name_filter) |>
    collect()
}

# Load data
rio_tmin   <- get_indicator_mean(temp_min,  "Tmin_3.2.3_mean")
rio_tmax   <- get_indicator_mean(temp_max,  "Tmax_3.2.3_mean")
rio_precip <- get_indicator_mean(precip,    "pr_3.2.3_mean")

# Combine temperature and prepare doy
rio_temp <- bind_rows(
  rio_tmin |> mutate(var = "Daily minimum"),
  rio_tmax |> mutate(var = "Daily maximum")
) |>
  mutate(
    doy = yday(date),
    year = year(date)
  )

# Temperature plot
plot_temp <- ggplot(rio_temp, aes(x = doy, y = value, color = var)) +
  geom_line(alpha = 0.3, linewidth = 0.3) +  # THIN LINES INSTEAD OF POINTS
  geom_smooth(se = TRUE, span = 0.01, linewidth = 1) +
  scale_color_manual(values = c("Daily minimum" = "darkblue", "Daily maximum" = "darkred")) +
  scale_x_continuous(
    breaks = seq(15, 365, by = 30.5),
    labels = month.abb,
    expand = expansion(mult = c(0, 0))
  )+
  labs(
    title = "A) Seasonal temperature Pattern (°C) [1950–2024]",
    #subtitle = "Based on TerraClimate dataset",
    x = NULL,
    y = "Temperature °C"
  ) +
  theme(
    legend.position = c(0.15, 0.25),
    legend.justification = c("right", "top"),
    legend.title = element_blank(),
    text = element_text(size = 13),
    plot.title = element_text(size = 16),
    axis.title = element_text(size = 13),
    axis.text = element_text(size = 11),
    legend.text = element_text(size = 11)
  )

# Precipitation data: daily to monthly
plot_precip_data <- rio_precip |>
  mutate(
    year = year(date),
    month_num = month(date)
  ) |>
  group_by(year, month_num) |>
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop") |>
  group_by(month_num) |>
  summarise(value = mean(value, na.rm = TRUE), .groups = "drop") |>
  mutate(
    doy = 15 + (month_num - 1) * 30.5,
    label = month.abb[month_num]
  )


# Precipitation plot (aligned by doy)
plot_precip <- ggplot(plot_precip_data, aes(x = doy, y = value)) +
  geom_col(fill = "steelblue", alpha = 0.5, width = 28) +
  geom_text(aes(label = round(value, 1)), vjust = -0.5, size = 4) +
  scale_x_continuous(
    breaks = seq(15, 365, by = 30.5),
    labels = month.abb,
    expand = expansion(mult = c(0, 0))
  ) +
  labs(
    title = "B) Average Monthly Precipitation Pattern (mm) [1950–2024]",
    #subtitle = "Based on TerraClimate dataset",
    x = "Month",
    y = "Precipitation (mm)"
  ) +
  ylim(0, 190)+
  theme(
    text = element_text(size = 13),             # base font size for all text
    plot.title = element_text(size = 16),
    axis.title = element_text(size = 13),
    axis.text = element_text(size = 11),
    legend.text = element_text(size = 11),
    legend.title = element_blank()              # already present, just kept for reference
  )


# Combine plots
final_plot <- plot_temp / plot_precip + plot_layout(heights = c(1, 0.8))

# Save figure
ggsave(
  filename = "D:/PROslide_RIO/Figs/rio_climate_plot.png",
  plot = final_plot,
  width = 14,
  height = 10,
  dpi = 300
)
