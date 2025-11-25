# Combine frequency + severity models to predict expected pure premium for each policy

library(tidyverse)
library(broom)

# 1. Load cleaned dataset

df <- read.csv("data/cleaned_commercial_auto_dataset.csv")

# 2. Load models (re-run model code OR source your scripts)

# Easiest method: re-fit the models in this script

# Frequency model
glm_freq <- glm(
  claims ~ industry + vehicle_type + territory +
    fleet_size + years_in_business + driver_age_band,
  offset = log(exposure),
  family = poisson(link = "log"),
  data = df
)

# Severity model (claims > 0)
df_sev <- df %>% filter(claims > 0)

glm_sev <- glm(
  severity ~ industry + vehicle_type + territory +
    fleet_size + years_in_business + driver_age_band,
  family = Gamma(link = "log"),
  data = df_sev
)


# 3. Predict frequency and severity for EVERY policy

df$pred_freq <- predict(glm_freq, type = "response")
df$pred_sev  <- predict(glm_sev, newdata = df, type = "response")

# Calculate predicted pure premium
df$pred_pure_premium <- df$pred_freq * df$pred_sev


# 4. Compare actual vs predicted (portfolio-level)

portfolio_compare <- tibble(
  actual_mean_pure_premium = mean(df$pure_premium, na.rm = TRUE),
  predicted_mean_pure_premium = mean(df$pred_pure_premium, na.rm = TRUE)
)

print(portfolio_compare)

write.csv(portfolio_compare,
          "outputs/portfolio_actual_vs_predicted_pure_premium.csv",
          row.names = FALSE)


# 5. Segment-level comparison (example: by territory)

pp_territory <- df %>%
  group_by(territory) %>%
  summarise(
    actual = mean(pure_premium, na.rm = TRUE),
    predicted = mean(pred_pure_premium, na.rm = TRUE)
  )

print(pp_territory)

write.csv(pp_territory,
          "outputs/pure_premium_by_territory_actual_vs_predicted.csv",
          row.names = FALSE)


# 6. Plot: Actual vs Predicted Pure Premium by Territory

p_pp_territory <- ggplot(pp_territory, aes(x = actual, y = predicted, label = territory)) +
  geom_point(size = 4, color = "darkblue") +
  geom_text(vjust = -0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  theme_minimal() +
  labs(
    title = "Actual vs Predicted Pure Premium by Territory",
    x = "Actual Pure Premium ($)",
    y = "Predicted Pure Premium ($)"
  )

print(p_pp_territory)

ggsave("plots/pure_premium_by_territory_actual_vs_predicted.png",
       p_pp_territory, width = 7, height = 5)


# 7. Save final dataset with predictions

write.csv(df, "data/predicted_pure_premium_dataset.csv", row.names = FALSE)

cat("\nPredicted pure premium dataset saved to data/predicted_pure_premium_dataset.csv\n")