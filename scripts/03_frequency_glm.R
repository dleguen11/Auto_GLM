# Fit a Poisson GLM for claim frequency

library(tidyverse)

# Load cleaned dataset

df <- read.csv("data/cleaned_commercial_auto_dataset.csv")

# 1. Fit Poisson GLM for claim frequency

glm_freq <- glm(
  claims ~ industry + vehicle_type + territory +
    fleet_size + years_in_business + driver_age_band,
  offset = log(exposure),
  family = poisson(link = "log"),
  data = df
)

# Display model summary
summary(glm_freq)


# 2. Save model summary to a text file (for GitHub)

summary_text <- capture.output(summary(glm_freq))
writeLines(summary_text, "outputs/frequency_glm_summary.txt")


# 3. Calculate model relativities (exponentiated coefficients)

freq_rels <- broom::tidy(glm_freq, exponentiate = TRUE, conf.int = TRUE)

# Save as CSV for portfolio
write.csv(freq_rels, "outputs/frequency_glm_relativities.csv", row.names = FALSE)

print(freq_rels)


# 4. Check for overdispersion 

resid_dev <- sum(residuals(glm_freq, type = "pearson")^2)
df_resid  <- glm_freq$df.residual
overdispersion_ratio <- resid_dev / df_resid

cat("\nOverdispersion ratio:", overdispersion_ratio, "\n")

# Rule of thumb:
# < 1.5  → OK for Poisson
# 1.5–2 → borderline
# > 2   → consider Negative Binomial


# 5. Save overdispersion check

writeLines(
  paste("Overdispersion ratio:", round(overdispersion_ratio, 3)),
  "outputs/frequency_overdispersion.txt"
)