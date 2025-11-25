# Fit a Gamma GLM for claim severity

library(tidyverse)
library(broom)

# Load cleaned dataset

df <- read.csv("data/cleaned_commercial_auto_dataset.csv")

# 1. Filter to claims > 0

df_sev <- df %>% filter(claims > 0)

cat("Number of rows with claims > 0:", nrow(df_sev), "\n\n")


# 2. Fit Gamma GLM for claim severity

glm_sev <- glm(
  severity ~ industry + vehicle_type + territory +
    fleet_size + years_in_business + driver_age_band,
  family = Gamma(link = "log"),
  data = df_sev
)

summary(glm_sev)


# 3. Save model summary to output folder

summary_text <- capture.output(summary(glm_sev))
writeLines(summary_text, "outputs/severity_glm_summary.txt")


# 4. Extract relativities (multiplicative factors)

sev_rels <- broom::tidy(glm_sev, exponentiate = TRUE, conf.int = TRUE)

# Save relativities
write.csv(sev_rels, "outputs/severity_glm_relativities.csv", row.names = FALSE)

print(sev_rels)


# 5. Model diagnostics: Check residual deviance

resid_dev <- sum(residuals(glm_sev, type = "pearson")^2)
df_resid  <- glm_sev$df.residual
dispersion_ratio <- resid_dev / df_resid

cat("\nGamma dispersion ratio:", dispersion_ratio, "\n")


# 6. Save dispersion check

writeLines(
  paste("Gamma dispersion ratio:", round(dispersion_ratio, 3)),
  "outputs/severity_dispersion.txt"
)