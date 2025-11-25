library(tidyverse)

set.seed(123)

# Number of policies to simulate
n <- 10000

# 1. Business / Industry Categories (NAICS-like)
industry_levels <- c("Construction", "Retail", "Logistics", "Healthcare", "Professional")

# 2. Vehicle Type
vehicle_levels <- c("Light_Truck", "Cargo_Van", "Delivery_Truck", "Box_Truck")

# 3. Territory
territory_levels <- c("Rural", "Suburban", "Urban")

# Generate dataset
df <- tibble(
  policy_id = 1:n,
  
  industry = sample(industry_levels, n, replace = TRUE, prob = c(0.25,0.20,0.20,0.15,0.20)),
  vehicle_type = sample(vehicle_levels, n, replace = TRUE, prob = c(0.40,0.30,0.20,0.10)),
  territory = sample(territory_levels, n, replace = TRUE, prob = c(0.20,0.35,0.45)),
  
  fleet_size = sample(1:50, n, replace = TRUE),
  years_in_business = sample(1:40, n, replace = TRUE),
  driver_age = sample(21:70, n, replace = TRUE),
  
  # Exposures between 0.5 and 1.0 (partial-year policies)
  exposure = runif(n, 0.5, 1.0)
)

# --- Frequency model structure (simulate claims) ---
# True underlying relativities for simulation (not visible in real life)
industry_freq_factor <- c(Construction=1.3, Retail=1.0, Logistics=1.4, Healthcare=0.9, Professional=0.7)
vehicle_freq_factor <- c(Light_Truck=0.9, Cargo_Van=1.0, Delivery_Truck=1.3, Box_Truck=1.4)
territory_freq_factor <- c(Rural=0.7, Suburban=1.0, Urban=1.5)

# Baseline frequency
base_freq <- 0.10

df <- df %>%
  mutate(
    freq_lambda = base_freq *
      industry_freq_factor[industry] *
      vehicle_freq_factor[vehicle_type] *
      territory_freq_factor[territory] *
      (1 + (fleet_size - 10) * 0.01) *
      exposure,
    
    claims = rpois(n, lambda = freq_lambda)
  )

# --- Severity model structure (simulate losses for claims > 0) ---
industry_sev_factor <- c(Construction=1.4, Retail=1.0, Logistics=1.5, Healthcare=1.1, Professional=0.9)
vehicle_sev_factor <- c(Light_Truck=0.9, Cargo_Van=1.0, Delivery_Truck=1.3, Box_Truck=1.5)
territory_sev_factor <- c(Rural=0.8, Suburban=1.0, Urban=1.2)

base_severity <- 7000  # typical small-commercial auto severity

df <- df %>%
  mutate(
    sev_mu = base_severity *
      industry_sev_factor[industry] *
      vehicle_sev_factor[vehicle_type] *
      territory_sev_factor[territory],
    
    total_loss = ifelse(
      claims > 0,
      rgamma(n, shape = 2, scale = sev_mu / 2),
      0
    ),
    
    # Derived fields
    severity = ifelse(claims > 0, total_loss / claims, NA_real_),
    pure_premium = total_loss / exposure
  )

# Save dataset
write.csv(df, "commercial_auto_dataset.csv", row.names = FALSE)

df