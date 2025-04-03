source("~/R/ccw_example/src/setup.R", echo=F)

# Define interventions ----

## a priori treatments: 
# Assign = 0 (No treatment ever), 
# Assign = 1 (treat by time 12); i.e. grace period of 12

  d_cloned = readRDS(here('dta', 'survdta_cloned.R')) %>%
    mutate(start=1,
           end = clone_time)

  # data.table is faster  
    setDT(d_cloned)
    
    d_panel = d_cloned[rep(seq(.N), clone_time)]
                       
    d_panel[, exit := (seq_len(.N)), by = list(id)]
    d_panel[, enter := exit-1]
    
    setDF(d_panel)
    
    select(d_panel, id, time, clone_time, censor_time, treat, treat_time, assign, enter, exit, end)
    

saveRDS(d_panel, here('dta', 'survdta_cloned_panel.R'))