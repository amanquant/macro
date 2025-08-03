# US Unemployment Rate Analysis (1955-2025)
# Load required libraries
library(readr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)
library(forecast)
library(changepoint)
library(plotly)

# Read the data
# Assuming the CSV file is in your working directory
unemployment_data <- read_csv("openbb__economy_unemployment_20250803_141512.csv")

# Data preprocessing
unemployment_data <- unemployment_data %>%
  mutate(
    date = as.Date(date),
    year = year(date),
    month = month(date),
    unemployment_rate_pct = value * 100,  # Convert to percentage
    decade = floor(year / 10) * 10
  )

# Basic data summary
cat("=== DATA SUMMARY ===\n")
cat("Date range:", min(unemployment_data$date), "to", max(unemployment_data$date), "\n")
cat("Number of observations:", nrow(unemployment_data), "\n")
cat("Mean unemployment rate:", round(mean(unemployment_data$unemployment_rate_pct), 2), "%\n")
cat("Median unemployment rate:", round(median(unemployment_data$unemployment_rate_pct), 2), "%\n")
cat("Min unemployment rate:", round(min(unemployment_data$unemployment_rate_pct), 2), "% (", 
    unemployment_data$date[which.min(unemployment_data$unemployment_rate_pct)], ")\n")
cat("Max unemployment rate:", round(max(unemployment_data$unemployment_rate_pct), 2), "% (", 
    unemployment_data$date[which.max(unemployment_data$unemployment_rate_pct)], ")\n")

# 1. Time Series Plot
p1 <- ggplot(unemployment_data, aes(x = date, y = unemployment_rate_pct)) +
  geom_line(color = "steelblue", size = 0.7) +
  geom_smooth(method = "loess", se = TRUE, alpha = 0.3, color = "red") +
  labs(
    title = "US Unemployment Rate Evolution (1955-2025)",
    subtitle = "Monthly data with trend line",
    x = "Year",
    y = "Unemployment Rate (%)",
    caption = "Source: OpenBB Economy Data"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12)
  ) +
  scale_x_date(date_breaks = "10 years", date_labels = "%Y")

print(p1)

# 2. Decade-wise comparison with outlier analysis
decade_stats <- unemployment_data %>%
  group_by(decade) %>%
  summarise(
    mean_rate = mean(unemployment_rate_pct),
    median_rate = median(unemployment_rate_pct),
    min_rate = min(unemployment_rate_pct),
    max_rate = max(unemployment_rate_pct),
    std_dev = sd(unemployment_rate_pct),
    q1 = quantile(unemployment_rate_pct, 0.25),
    q3 = quantile(unemployment_rate_pct, 0.75),
    iqr = q3 - q1,
    .groups = 'drop'
  ) %>%
  mutate(
    outlier_threshold_high = q3 + 1.5 * iqr,
    outlier_threshold_low = q1 - 1.5 * iqr
  )

# Count outliers by decade
outlier_counts <- unemployment_data %>%
  left_join(decade_stats, by = "decade") %>%
  mutate(
    is_outlier = unemployment_rate_pct > outlier_threshold_high | 
      unemployment_rate_pct < outlier_threshold_low
  ) %>%
  group_by(decade) %>%
  summarise(
    total_observations = n(),
    outlier_count = sum(is_outlier),
    outlier_percentage = round((outlier_count / total_observations) * 100, 1),
    extreme_values = paste(unemployment_rate_pct[is_outlier], collapse = ", "),
    .groups = 'drop'
  )

print("=== DECADE-WISE STATISTICS WITH OUTLIERS ===")
print(left_join(decade_stats, outlier_counts, by = "decade"))

# Calculate whisker values for each decade
whisker_data <- unemployment_data %>%
  group_by(decade) %>%
  summarise(
    q1 = quantile(unemployment_rate_pct, 0.25),
    q3 = quantile(unemployment_rate_pct, 0.75),
    iqr = q3 - q1,
    lower_whisker = max(min(unemployment_rate_pct), q1 - 1.5 * iqr),
    upper_whisker = min(max(unemployment_rate_pct), q3 + 1.5 * iqr),
    .groups = 'drop'
  )

# Create enhanced boxplot with whisker annotations
p2 <- ggplot(unemployment_data, aes(x = factor(decade), y = unemployment_rate_pct)) +
  geom_boxplot(fill = "lightblue", alpha = 0.7, outlier.color = "red", 
               outlier.size = 2, size = 0.8) +
  geom_jitter(alpha = 0.2, width = 0.2, color = "navy", size = 0.8) +
  # Add whisker value labels
  geom_text(data = whisker_data, 
            aes(x = factor(decade), y = upper_whisker, 
                label = paste("↑", round(upper_whisker, 1), "%")), 
            vjust = -0.5, size = 2.5, color = "darkgreen", fontface = "bold") +
  geom_text(data = whisker_data, 
            aes(x = factor(decade), y = lower_whisker, 
                label = paste("↓", round(lower_whisker, 1), "%")), 
            vjust = 1.5, size = 2.5, color = "darkgreen", fontface = "bold") +
  # Add outlier count annotations
  geom_text(data = outlier_counts, 
            aes(x = factor(decade), y = Inf, 
                label = paste("Outliers:", outlier_count)), 
            vjust = 2.5, size = 3, color = "red", fontface = "bold") +
  labs(
    title = "Unemployment Rate Distribution by Decade with Whiskers",
    subtitle = "Red dots = outliers, Green arrows = whisker limits (Q1±1.5×IQR, Q3±1.5×IQR)",
    x = "Decade",
    y = "Unemployment Rate (%)",
    caption = "Whiskers extend to furthest points within 1.5×IQR from quartiles"
  ) +
  theme_minimal() +
  theme(
    plot.subtitle = element_text(size = 10),
    plot.caption = element_text(size = 9)
  )

print(p2)

# Print whisker summary table
cat("\n=== WHISKER VALUES BY DECADE ===\n")
whisker_summary <- whisker_data %>%
  select(decade, lower_whisker, upper_whisker) %>%
  mutate(
    whisker_range = upper_whisker - lower_whisker,
    decade = paste0(decade, "s")
  )
print(whisker_summary)
# 3. Seasonal Analysis
seasonal_data <- unemployment_data %>%
  group_by(month) %>%
  summarise(
    avg_rate = mean(unemployment_rate_pct),
    median_rate = median(unemployment_rate_pct),
    .groups = 'drop'
  ) %>%
  mutate(month_name = month.abb[month])

p3 <- ggplot(seasonal_data, aes(x = factor(month), y = avg_rate)) +
  geom_col(fill = "coral", alpha = 0.8) +
  geom_text(aes(label = round(avg_rate, 1)), vjust = -0.5, size = 3) +
  labs(
    title = "Average Unemployment Rate by Month (1955-2025)",
    x = "Month",
    y = "Average Unemployment Rate (%)"
  ) +
  scale_x_discrete(labels = month.abb) +
  theme_minimal()

print(p3)

# COVID-19 Impact Analysis - Special focus on 2020s outliers
covid_analysis <- unemployment_data %>%
  filter(year >= 2020) %>%
  arrange(desc(unemployment_rate_pct))

cat("\n=== COVID-19 PANDEMIC IMPACT (2020s OUTLIERS) ===\n")
cat("The 2020s decade shows extreme outliers due to COVID-19:\n")
print(head(covid_analysis, 10))

# Compare COVID spike to historical context
historical_peaks <- unemployment_data %>%
  filter(year < 2020) %>%
  arrange(desc(unemployment_rate_pct)) %>%
  head(5)

cat("\n=== HISTORICAL CONTEXT: Pre-2020 Highest Unemployment Rates ===\n")
print(historical_peaks)

cat("\n=== COVID vs HISTORICAL COMPARISON ===\n")
covid_peak <- max(unemployment_data$unemployment_rate_pct[unemployment_data$year >= 2020])
historical_peak <- max(unemployment_data$unemployment_rate_pct[unemployment_data$year < 2020])
cat("COVID-19 peak unemployment:", round(covid_peak, 1), "%\n")
cat("Pre-2020 historical peak:", round(historical_peak, 1), "%\n")
cat("COVID spike was", round(covid_peak - historical_peak, 1), "percentage points higher!\n")
# Identify periods where unemployment rate > 8%
high_unemployment <- unemployment_data %>%
  filter(unemployment_rate_pct > 8.0) %>%
  arrange(date)

cat("\n=== HIGH UNEMPLOYMENT PERIODS (>8%) ===\n")
if(nrow(high_unemployment) > 0) {
  recession_periods <- high_unemployment %>%
    mutate(
      period_group = cumsum(c(1, diff(as.numeric(date)) > 180))  # Group periods >6 months apart
    ) %>%
    group_by(period_group) %>%
    summarise(
      start_date = min(date),
      end_date = max(date),
      max_unemployment = max(unemployment_rate_pct),
      avg_unemployment = mean(unemployment_rate_pct),
      duration_months = n(),
      .groups = 'drop'
    )
  
  print(recession_periods)
} else {
  cat("No periods with unemployment rate > 8% found in the dataset.\n")
}

# 5. Change point detection
ts_data <- ts(unemployment_data$unemployment_rate_pct, 
              start = c(1955, 1), frequency = 12)

# Detect change points
cpt_result <- cpt.mean(ts_data, method = "PELT")
change_points <- cpts(cpt_result)

cat("\n=== DETECTED CHANGE POINTS ===\n")
if(length(change_points) > 0) {
  change_dates <- unemployment_data$date[change_points]
  change_values <- unemployment_data$unemployment_rate_pct[change_points]
  
  change_point_df <- data.frame(
    Date = change_dates,
    Unemployment_Rate = round(change_values, 2)
  )
  print(change_point_df)
}

# 6. Recent trends (2020-2025)
recent_data <- unemployment_data %>%
  filter(year >= 2020)

p4 <- ggplot(recent_data, aes(x = date, y = unemployment_rate_pct)) +
  geom_line(color = "darkred", size = 1) +
  geom_point(color = "darkred", size = 1.5) +
  labs(
    title = "Recent Unemployment Trends (2020-2025)",
    subtitle = "Showing COVID-19 impact and recovery",
    x = "Date",
    y = "Unemployment Rate (%)"
  ) +
  theme_minimal() +
  scale_x_date(date_breaks = "6 months", date_labels = "%b %Y") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p4)

# 7. Year-over-year changes
yearly_data <- unemployment_data %>%
  group_by(year) %>%
  summarise(
    avg_unemployment = mean(unemployment_rate_pct),
    .groups = 'drop'
  ) %>%
  mutate(
    yoy_change = avg_unemployment - lag(avg_unemployment),
    yoy_change_pct = (yoy_change / lag(avg_unemployment)) * 100
  )

# Find largest increases and decreases
cat("\n=== LARGEST YEAR-OVER-YEAR CHANGES ===\n")
cat("Largest increases:\n")
print(yearly_data %>% 
        filter(!is.na(yoy_change)) %>% 
        arrange(desc(yoy_change)) %>% 
        head(5) %>%
        select(year, avg_unemployment, yoy_change))

cat("\nLargest decreases:\n")
print(yearly_data %>% 
        filter(!is.na(yoy_change)) %>% 
        arrange(yoy_change) %>% 
        head(5) %>%
        select(year, avg_unemployment, yoy_change))

# 8. Correlation with economic cycles
# Create a simple business cycle indicator based on unemployment trends
unemployment_data <- unemployment_data %>%
  arrange(date) %>%
  mutate(
    ma_12 = zoo::rollmean(unemployment_rate_pct, k = 12, fill = NA, align = "right"),
    trend = ifelse(unemployment_rate_pct > lag(ma_12, 6), "Rising", "Falling")
  )

# 9. Statistical summary by presidential terms (rough approximation)
presidential_periods <- data.frame(
  president = c("Eisenhower", "Kennedy/Johnson", "Nixon/Ford", "Carter", 
                "Reagan", "Bush Sr.", "Clinton", "Bush Jr.", "Obama", 
                "Trump", "Biden"),
  start_year = c(1955, 1961, 1969, 1977, 1981, 1989, 1993, 2001, 2009, 2017, 2021),
  end_year = c(1960, 1968, 1976, 1980, 1988, 1992, 2000, 2008, 2016, 2020, 2025)
)

presidential_stats <- unemployment_data %>%
  mutate(
    president = case_when(
      year >= 1955 & year <= 1960 ~ "Eisenhower",
      year >= 1961 & year <= 1968 ~ "Kennedy/Johnson", 
      year >= 1969 & year <= 1976 ~ "Nixon/Ford",
      year >= 1977 & year <= 1980 ~ "Carter",
      year >= 1981 & year <= 1988 ~ "Reagan",
      year >= 1989 & year <= 1992 ~ "Bush Sr.",
      year >= 1993 & year <= 2000 ~ "Clinton",
      year >= 2001 & year <= 2008 ~ "Bush Jr.",
      year >= 2009 & year <= 2016 ~ "Obama",
      year >= 2017 & year <= 2020 ~ "Trump",
      year >= 2021 & year <= 2025 ~ "Biden",
      TRUE ~ "Other"
    )
  ) %>%
  filter(president != "Other") %>%
  group_by(president) %>%
  summarise(
    avg_unemployment = mean(unemployment_rate_pct),
    min_unemployment = min(unemployment_rate_pct),
    max_unemployment = max(unemployment_rate_pct),
    .groups = 'drop'
  ) %>%
  arrange(avg_unemployment)

cat("\n=== UNEMPLOYMENT BY PRESIDENTIAL PERIODS ===\n")
print(presidential_stats)

# 10. Create an interactive plot for exploration
p_interactive <- plot_ly(unemployment_data, x = ~date, y = ~unemployment_rate_pct, 
                         type = 'scatter', mode = 'lines',
                         line = list(color = 'steelblue'),
                         hovertemplate = 'Date: %{x}<br>Unemployment Rate: %{y:.1f}%<extra></extra>') %>%
  layout(
    title = "Interactive US Unemployment Rate (1955-2025)",
    xaxis = list(title = "Date"),
    yaxis = list(title = "Unemployment Rate (%)"),
    hovermode = 'x'
  )

# Display the interactive plot
p_interactive

# Save plots
ggsave("unemployment_timeseries.png", p1, width = 12, height = 6, dpi = 300)
ggsave("unemployment_decades.png", p2, width = 10, height = 6, dpi = 300)
ggsave("unemployment_seasonal.png", p3, width = 10, height = 6, dpi = 300)
ggsave("unemployment_recent.png", p4, width = 10, height = 6, dpi = 300)

cat("\n=== ANALYSIS COMPLETE ===\n")
cat("Plots saved as PNG files in your working directory.\n")
cat("Interactive plot displayed above for exploration.\n")