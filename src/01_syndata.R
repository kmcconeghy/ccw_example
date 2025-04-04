library(here())

source(here('src', 'setup.R'), echo=F)

# Set seed for reproducibility
set.seed(42)

# Number of participants
n <- 10000

# Generate age (normal distribution with mean=75, std=10)
  age <- round(rnorm(n, mean = 75, sd = 10), 1)

# Generate gender (66% female, 34% male)
  female <- sample(c(1, 0), size = n, replace = TRUE, prob = c(0.66, 0.34))

# Generate treatment status (10% treated, 90% control)
  treat <- sample(c(1, 0), size = n, replace = TRUE, prob = c(0.2, 0.9))

# Simulate failure times (exponential distribution for simplicity)
  hr_treatment = 0.8
  hr_gender = 0.9
  hr_age = 0.98
  
  # Calculate linear predictor (log-hazard)
  lp_outc <- log(hr_treatment) * treat +
    log(hr_gender) * female +
    log(hr_age) * age
  
  # Simulate baseline survival times (exponential distribution)
  baseline_hazard <- 0.05
  baseline_survival <- rexp(n, rate = baseline_hazard)
  
  # Adjust survival times based on linear predictor
  failure_time <- round(baseline_survival * exp(-lp_outc))

  lp_treat <- log(0.7) * female +
    log(1.02) * age
  
  baseline_hazard <- 0.1
  treat_time <- ifelse(treat==1, round(rexp(n, rate = baseline_hazard)* exp(-lp_treat)), Inf)
  
# Simulate censoring (e.g., 20% censored)
  censor_status <- sample(c(0, 1), size = n, replace = TRUE, prob = c(0.2, 0.8)) # 0 = censored, 1 = observed

# Simulate binary outcome (failure event)
  outcome <- ifelse(censor_status == 1, 1, 0) # Example: failure events only when not censored

# Create dataset
data <- data.frame(
  X1 = age,
  X2 = female,
  treat = treat,
  treat_time = treat_time, # need a time treatment initiated
  outc_time = failure_time,
  censor_nat = censor_status, #natural censoring, i.e. competing risk or adm end
  outcome = outcome
) %>%
  mutate(id = row_number())

# quick test
survfit(Surv(outc_time, outcome) ~ treat, data = data)
  
saveRDS(data, here('dta', 'survdta.R'))