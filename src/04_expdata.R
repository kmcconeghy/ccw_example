library(here())

source(here('src', 'setup.R'), echo=F)
# Define interventions ----

## a priori treatments: 
# Assign = 0 (No treatment ever), 
# Assign = 1 (treat by time 12); i.e. grace period of 12

  # start - time 0 (1)
  # end - censor time (event, censor, end of follow-up etc.)
  d_cloned = readRDS(here('dta', 'survdta_cloned.R')) %>%
    mutate(start=1,
           end = clone_time)

  # Expand data so one row per unit of follow-up
  # data.table is faster  
    setDT(d_cloned)
    
    d_panel = d_cloned[rep(seq(.N), clone_time)]
                       
    d_panel[, exit := (seq_len(.N)), by = list(id, assign)]
    d_panel[, enter := exit-1]
    d_panel[, time := seq_len(.N), by = list(id, assign)]
    
    # Outcome is = 1 in row where event occurred
    d_panel[, outcome := if_else(outc_time <= time, 1L, 0L), by = list(id, assign)]
    
    setDF(d_panel)
    
    d_panel_2 = select(d_panel, id, time, outcome, treat_time, assign, enter, exit, end) %>%
      dplyr::filter(time<=60)

saveRDS(d_panel_2, here('dta', 'survdta_cloned_panel.R'))

# TIME-VARYING FOR WEIGHTS ----
  d_treat = readRDS(here('dta', 'survdta_cloned.R')) %>%
    # keep one clone
    dplyr::filter(assign==0) %>%
    mutate(start=1,
           end = treat_time) %>%
    group_by(id) %>%
    mutate(
      time = pmin(outc_time, treat_time),
      outcome = if_else(time==treat_time, 0, 1)
    ) %>%
    ungroup

  # Expand data so one row per unit of follow-up
  # data.table is faster  
  setDT(d_treat)
  
  d_panel = d_treat[rep(seq(.N), time)]
  
  d_panel[, exit := (seq_len(.N)), by = list(id)]
  d_panel[, enter := exit-1]
  d_panel[, time := seq_len(.N), by = list(id)]
  
  # Outcome is = 1 in row where treat occurred
  d_panel[, outcome := if_else(treat_time <= time, 1L, 0L), by = list(id)]
  
  setDF(d_panel)
  
  d_panel_2 = select(d_panel, id, time, outcome, treat, enter, exit, end, X1, X2) %>%
    dplyr::filter(time<=60)

  saveRDS(d_panel_2, here('dta', 'survdta_treat_panel.R'))
  