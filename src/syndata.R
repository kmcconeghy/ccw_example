source("~/R/ccw_example/src/setup.R", echo=F)
# Set seed for reproducibility
set.seed(42)

# Number of participants
n <- 1000

# Generate age (normal distribution with mean=75, std=10)
  age <- round(rnorm(n, mean = 75, sd = 10), 1)

# Generate gender (66% female, 34% male)
  gender <- sample(c("Female", "Male"), size = n, replace = TRUE, prob = c(0.66, 0.34))

# Generate treatment status (10% treated, 90% control)
  treatment_status <- sample(c("Treated", "Control"), size = n, replace = TRUE, prob = c(0.1, 0.9))

# Simulate failure times (exponential distribution for simplicity)
  failure_time <- round(rexp(n, rate = 0.1), 1)
  treat_time <- ifelse(treatment_status=="Treated", round(rexp(n, rate = 0.1), 1), Inf)
  
# Simulate censoring (e.g., 20% censored)
  censor_status <- sample(c(0, 1), size = n, replace = TRUE, prob = c(0.2, 0.8)) # 0 = censored, 1 = observed

# Simulate binary outcome (failure event)
  outcome <- ifelse(censor_status == 1, 1, 0) # Example: failure events only when not censored

# Create dataset
data <- data.frame(
  Age = age,
  Gender = gender,
  Treatment_Status = treatment_status,
  treat_time = treat_time,
  Failure_Time = failure_time,
  Censor_Status = censor_status,
  Outcome = outcome
)

survfit(Surv(Failure_Time, Outcome) ~ Treatment_Status, data = data)
  
saveRDS(data, here('dta', 'survdta.R'))