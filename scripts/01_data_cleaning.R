# Clean and prepare the Commercial Auto dataset

library(tidyverse)

# Load dataset

df <- read.csv("commercial_auto_dataset.csv")

# Inspect raw structure

cat("Raw structure:\n")
str(df)
cat("\nMissing values summary:\n")
print(colSums(is.na(df)))

# Convert categorical variables to factors

df <- df %>%
  mutate(
    industry = as.factor(industry),
    vehicle_type = as.factor(vehicle_type),
    territory = as.factor(territory)
  )

# Create driver age bands

df <- df %>%
  mutate(
    driver_age_band = cut(
      driver_age,
      breaks = c(20, 30, 40, 50, 60, 70, 100),
      labels = c("21–30", "31–40", "41–50", "51–60", "61–70", "70+"),
      right = TRUE
    )
  )

# Rearrange columns for readability

df <- df %>%
  select(
    policy_id, industry, vehicle_type, territory,
    fleet_size, years_in_business, driver_age, driver_age_band,
    exposure, claims, total_loss, severity, pure_premium,
    everything()
  )

# Summary after cleaning

cat("\nStructure AFTER cleaning:\n")
str(df)

cat("\nCleaned summary statistics:\n")
summary(df)

# Save cleaned dataset

write.csv(df, "data/cleaned_commercial_auto_dataset.csv", row.names = FALSE)

cat("\nCleaned dataset saved to data/cleaned_commercial_auto_dataset.csv\n")