# setup -----
  library(here())
  
  source(here('src', 'setup.R'), echo=F)

  #increase size for bootstrapping procedure
  options(future.globals.maxSize = 4000*1024^2) 

  grid.draw.ggsurvplot = function(x) {
    survminer:::print.ggsurvplot(x, newpage=F)
  }
  
  #analysis options stored in list and saved
  d_output = list(runtime = Sys.time(),
                  #params = l_tte_params,
                  runplan = list(boots = 50,
                                 workers = 8,
                                 seed = as.integer(ymd('2024-11-16'))
                                 ))
  
  d_panel =  readRDS(here('dta', 'survdta_cloned_panel.R'))
  setDT(d_panel)
  
# Naive estimator ----

# Run Plan ----
  
  # defined above
  set.seed(d_output$runplan$seed)
  
  d_glm = glm(event_outc ~ poly(time, 2, raw=T)*assign, data=d_panel, family=binomial())
  
  ## Survival probabilities ----
  d_panel$pr_ev = d_glm$fitted.values
  
  d_panel[, `:=`(pr_surv = cumprod(1 - pr_ev)), by=list(id, assign)] 
  
  d_res = d_panel %>%
    group_by(assign, time) %>%
      summarize(pr_ev = mean(1-pr_surv), .groups = 'drop') %>%
    ungroup %>%
    pivot_wider(., id_cols =c('time'), 
                names_from = assign, 
                names_prefix = 'pr_ev_',
                values_from = pr_ev
    ) %>%
    mutate(cid = pr_ev_1 - pr_ev_0,
           cir = pr_ev_1 / pr_ev_0)

  # BOOTSTRAP ----
  
  # defined above
  plan(multisession, workers = d_output$runplan$workers)
  set.seed(d_output$runplan$seed)
  
  # FUNCTION TO RUN ITERATIVELY ###
  d_fun_getplrwt = function(dta, d_ids, ...) {
    
    # For Poisson bootstrap - cluster by person
    # Generate a frequency weight for each person
    # Wt ~ Poisson(Lambda=1)
    d_ids = mutate(d_ids, freqwt = rpois(n(), 1L))
    
    dta_2 = left_join(dta, d_ids, by='id') 
    
    d_glm_wt = glm(event_outc ~ poly(time, 2, raw=T)*assign, data=dta_2, 
                   family=binomial(), weights = freqwt)
    
    ## Survival probabilities ----
    dta_2$pr_ev = d_glm_wt$fitted.values
    
    dta_2[, `:=`(pr_surv = cumprod(1 - pr_ev)), by=list(id, assign)] 
    
    d_res = dta_2 %>%
      group_by(assign, time) %>%
      summarize(pr_ev = weighted.mean(1-pr_surv, w = freqwt), .groups = 'drop') %>%
      ungroup %>%
      pivot_wider(., id_cols =c('time'), 
                  names_from = assign, 
                  names_prefix = 'pr_ev_',
                  values_from = pr_ev
      ) %>%
      mutate(cid = pr_ev_1 - pr_ev_0,
             cir = pr_ev_1 / pr_ev_0)
    
    return(d_res)
  }
  
  d_bs = future_map(.x = 1:d_output$runplan$boots, 
                    .f = ~d_fun_getplrwt(d_panel, d_ids, .x),
                    .options = furrr_options(seed = T))
  
  # Combine and summarize ----
  d_surv = d_bs %>%
    bind_rows(.id = 'boot') %>%
    mutate(boot = as.numeric(boot)) 
  
  d_summ_surv = d_surv %>%
    group_by(time) %>%
    summarize(pr_ev_0_lc = quantile(pr_ev_0, 0.025, na.rm=T),
              pr_ev_1_lc = quantile(pr_ev_1, 0.025, na.rm=T),
              cir_lc = quantile(cir, 0.025, na.rm=T),
              cid_lc = quantile(cid, 0.025, na.rm=T),
              pr_ev_0_uc = quantile(pr_ev_0, 0.975, na.rm=T),
              pr_ev_1_uc = quantile(pr_ev_1, 0.975, na.rm=T),
              cir_uc = quantile(cir, 0.975, na.rm=T),
              cid_uc = quantile(cid, 0.975, na.rm=T)) %>%
    bind_cols(select(d_res, pr_ev_0, pr_ev_1, cir, cid),
              .)
  
  ### Plot ----
    d_gg_ci = d_summ_surv %>%
      ggplot(aes(x=time)) +
      geom_line(aes(y = pr_ev_0), color='red', linewidth=1.2) +
      geom_line(aes(y = pr_ev_1), color='blue', linewidth=1.2, linetype=2) + 
      geom_ribbon(aes(ymin = pr_ev_0_lc, ymax = pr_ev_0_uc), 
                fill='red', alpha=0.2) + 
      geom_ribbon(aes(ymin = pr_ev_1_lc, ymax = pr_ev_1_uc), 
                fill='blue', alpha=0.2) +
    scale_x_continuous(breaks = seq(0, 60, 6),
                         limits = c(0, 60)) +
      theme_bw() +
      labs(x = 'Follow-up', y = 'Cumulative incidence')

    d_gg_rr = d_summ_surv %>%
      ggplot(aes(x=time)) +
      geom_line(aes(y = cir), color='green', linewidth=1.2) + 
      geom_ribbon(aes(ymin = cir_lc, ymax = cir_uc), 
                  fill='green', alpha=0.2) + 
      scale_y_continuous(limits = c(0.5, 1.1)) + 
      scale_x_continuous(breaks = seq(0, 60, 6),
                         limits = c(0, 60)) +
      theme_bw() +
      labs(x = 'Follow-up', y = 'Relative Risk')
    
    d_gg_1 = ggarrange(d_gg_ci, d_gg_rr,
                       nrow=1)
    
    ggsave( here('img', 
                 paste0('survplot_plrunadj', '.jpeg')),
            plot=d_gg_1, width = 12, height=8, dpi=300)

# Save KM est results ----
  write_csv(d_res,
    file = here('out', paste0('plrnaiveoutc.csv'))
  )

d_output$results = d_res

saveRDS(d_output, 
        file = here('out', paste0('plrnaive.Rds')))