# Data Analysis for Commercial Auto GLM
library(tidyverse)

# Load cleaned dataset

df <- read.csv("data/cleaned_commercial_auto_dataset.csv")

# 1. CLAIM FREQUENCY DISTRIBUTION

p_freq <- ggplot(df, aes(x = claims)) +
  geom_bar(fill = "steelblue") +
  theme_minimal() +
  labs(
    title = "Distribution of Claim Counts",
    x = "Number of Claims",
    y = "Policy Count"
  )

print(p_freq)
ggsave("plots/claim_frequency_distribution.png", p_freq, width = 7, height = 5)


# 2. SEVERITY DISTRIBUTION (CLAIMS > 0)

p_sev <- df %>%
  filter(claims > 0) %>%
  ggplot(aes(x = severity)) +
  geom_histogram(bins = 40, fill = "tomato") +
  theme_minimal() +
  labs(
    title = "Severity Distribution (for Claims > 0)",
    x = "Severity ($)",
    y = "Count"
  ) +
  coord_cartesian(xlim = c(0, quantile(df$severity, 0.99, na.rm = TRUE)))

print(p_sev)
ggsave("plots/severity_distribution.png", p_sev, width = 7, height = 5)


# 3. PURE PREMIUM BY TERRITORY

pp_territory <- df %>%
  group_by(territory) %>%
  summarise(avg_pp = mean(pure_premium, na.rm = TRUE))

p_pp_territory <- ggplot(pp_territory, aes(x = territory, y = avg_pp, fill = territory)) +
  geom_col() +
  theme_minimal() +
  labs(
    title = "Average Pure Premium by Territory",
    x = "Territory",
    y = "Average Pure Premium ($)"
  )

print(p_pp_territory)
ggsave("plots/pure_premium_by_territory.png", p_pp_territory, width = 7, height = 5)


# 4. PURE PREMIUM BY VEHICLE TYPE

pp_vehicle <- df %>%
  group_by(vehicle_type) %>%
  summarise(avg_pp = mean(pure_premium, na.rm = TRUE))

p_pp_vehicle <- ggplot(pp_vehicle, aes(x = vehicle_type, y = avg_pp, fill = vehicle_type)) +
  geom_col() +
  theme_minimal() +
  labs(
    title = "Average Pure Premium by Vehicle Type",
    x = "Vehicle Type",
    y = "Average Pure Premium ($)"
  ) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

print(p_pp_vehicle)
ggsave("plots/pure_premium_by_vehicle_type.png", p_pp_vehicle, width = 7, height = 5)


# 5. CLAIM FREQUENCY BY INDUSTRY

freq_industry <- df %>%
  group_by(industry) %>%
  summarise(avg_freq = mean(claims / exposure, na.rm = TRUE))

p_freq_industry <- ggplot(freq_industry, aes(x = industry, y = avg_freq, fill = industry)) +
  geom_col() +
  theme_minimal() +
  labs(
    title = "Average Claim Frequency by Industry",
    x = "Industry",
    y = "Average Frequency"
  ) +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))

print(p_freq_industry)
ggsave("plots/frequency_by_industry.png", p_freq_industry, width = 7, height = 5)


# 6. SEVERITY BY INDUSTRY (CLAIMS > 0)

sev_industry <- df %>%
  filter(claims > 0) %>%
  group_by(industry) %>%
  summarise(avg_sev = mean(severity, na.rm = TRUE))

p_sev_industry <- ggplot(sev_industry, aes(x = industry, y = avg_sev, fill = industry)) +
  geom_col() +
  theme_minimal() +
  labs(
    title = "Average Severity by Industry",
    x = "Industry",
    y = "Average Severity ($)"
  ) +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))

print(p_sev_industry)
ggsave("plots/severity_by_industry.png", p_sev_industry, width = 7, height = 5)


# 7. FLEET SIZE VS PURE PREMIUM

p_fleet_pp <- ggplot(df, aes(x = fleet_size, y = pure_premium)) +
  geom_point(alpha = 0.35, color = "#2c3e50") +
  geom_smooth(method = "loess", color = "red") +
  theme_minimal() +
  labs(
    title = "Fleet Size vs Pure Premium",
    x = "Fleet Size",
    y = "Pure Premium ($)"
  )

print(p_fleet_pp)
ggsave("plots/fleet_size_vs_pure_premium.png", p_fleet_pp, width = 7, height = 5)