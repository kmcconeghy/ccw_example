# Define interventions ----

## Three a priori treatments: 
# Assign = 0 (No treatment ever), 
# Assign = 1 (treat by time 12); i.e. grace period of 12

## Modify panel ----
data_cloned = bind_rows(
                    # Assign = 0
                     data %>%
                       mutate(assign = 0,
                              # censor if vaccinated in interval
                              censor_art = if_else(treat_time < failure_time, 1L, 0L),
                              censor_time = if_else(treat_time < failure_time, treat_time, Inf)
                       ),
                     # Assign = 1
                     data %>%
                       mutate(assign = 1,
                              censor_art = if_else(treat_time <= 12, 0L, 1L),
                              censor_time = if_else(treat_time > 12, 12, Inf)
                              )
                       ) %>%
  mutate(clone_time = pmin(time, censor_time),
         outcome = if_else(clone_time<time, 0, outcome))

head(data_cloned)

saveRDS(data_cloned, here('dta', 'survdta_cloned.R'))