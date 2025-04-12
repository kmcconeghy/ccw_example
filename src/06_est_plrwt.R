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
                  runplan = list(boots = 200,
                                 workers = 8,
                                 seed = as.integer(ymd('2024-11-16'))
                                 ))
  
  set.seed(d_output$runplan$seed)
  
  d_panel_treat =  readRDS(here('dta', 'survdta_treat_panel.R'))
  
  d_panel_outcome =  readRDS(here('dta', 'survdta_cloned_panel.R'))
  d_ids = distinct(d_panel_outcome, id) # list of unique person IDs (for bootstrap)
  
# Estimate Weights ----
  # ADD BASELINE AND TIME_VARYING COVARIATES
  d_glm_wt = glm(event_treat ~ poly(time, 2, raw=T) + poly(X1, 2) + X2, data=d_panel_treat, family=binomial())

  # estimate pr(treat==1)
  d_panel_treat$pr_treat = d_glm_wt$fitted.values
  
  # Calculative cumulative probability of non-treatment (treatment-free survival)
  setDT(d_panel_treat)
  d_panel_treat[, cumpr_notreat := cumprod(1-pr_treat), by = .(id)]
  
  # join probability to outcome dataset
  d_panel_outcome = left_join(d_panel_outcome, 
                              select(d_panel_treat, id, time, cumpr_notreat), 
                              by=c('id', 'time'))
  
  # calculate IPW
  setDT(d_panel_outcome)
  
  # PROJECT SPECIFIC!!! MUST CONSIDER CAREFULLY
  d_panel_outcome[, ipw := fcase(
    #cumpr_0; cumulative probability of no vaccination at time t
    assign==0, 1 / cumpr_notreat, 
    assign==1 & time < 12, 1, # trt - cant censor prior to grace
    assign==1 & time == 12 & t_treat < 12, 1, # assign=1 & treat time < grace end, cant censor at grace
    assign==1 & time==12 & t_treat==12, 1 / (1-cumpr_notreat), # treated at grace, then weight
    assign==1 & time==12 & t_treat>12, 0, # assign=1, and not treated before grace, censor
    assign==1 & time > 12, 1 # assign=1, after grace period, assign all weights as 1
  )]
  
  #
  # cumprod of assign==1
  # For those assign=1 above, I need to carry the weight assigned at time 12 forward to end of follow-up
  # I do this by taking cumulative product (only assign=1)
  #
  d_panel_outcome[assign==1, ipw := cumprod(ipw), by=list(id, assign)]
  
  summary(d_panel_outcome$ipw)

# Outcome model ----
  
  # defined above
  # NO TIMEVARYING COVARIATES BUT CAN ADD BASELINE COVARIATES ( STRONG PREDICTORS OF OUTCOME)
  d_glm_pe = glm(event_outc ~ poly(time, 2, raw=T)*assign, data=d_panel_outcome, 
                 family=binomial(), weights = ipw)
  
  ## Survival probabilities 
  d_panel_outcome$pr_ev = d_glm_pe$fitted.values
  
  d_panel_outcome[, pr_surv := cumprod(1 - pr_ev), by=list(id, assign)] 
  
  setDF(d_panel_outcome)
  
  d_res = d_panel_outcome %>%
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
  d_fun_getplrwt = function(dta_outc, dta_treat, ...) {
    
    # For Poisson bootstrap - cluster by person
    # Generate a frequency weight for each person
    # Wt ~ Poisson(Lambda=1)
    d_freqwt = distinct(dta_outc, id) %>% 
      mutate(., freqwt = rpois(n(), 1L))
    
    dta_outc = left_join(dta_outc, d_freqwt, by='id')
    
    dta_2 = left_join(dta_treat, d_freqwt, by='id') 
    
    d_glm_wt = glm(event_treat ~ poly(time, 2, raw=T) + poly(X1, 2) + X2, 
                   data=dta_2, family=binomial(), weights=freqwt)
    
    dta_2$pr_treat = d_glm_wt$fitted.values
    setDT(dta_2)
    dta_2[, cumpr_notreat := cumprod(1-pr_treat), by = .(id)]
    dta_3 = left_join(dta_outc, select(dta_2, id, time, cumpr_notreat), by=join_by(id, time))    
    setDT(dta_3)
      dta_3[, ipw := fcase(
        assign==0, 1 / cumpr_notreat, 
        assign==1 & time < 12, 1, 
        assign==1 & time == 12  & t_treat <  12, 1,
        assign==1 & time == 12  & t_treat == 12, 1 / (1-cumpr_notreat), 
        assign==1 & time == 12  & t_treat >  12, 0, 
        assign==1 & time > 12, 1)]
      dta_3[assign==1, ipw := cumprod(ipw), by=list(id, assign)]
      d_glm_pe = glm(event_outc ~ poly(time, 2, raw=T)*assign, data=dta_3, 
                     family=binomial(), weights = ipw*freqwt)
    dta_3$pr_ev = d_glm_pe$fitted.values
    dta_3[, pr_surv := cumprod(1 - pr_ev), by=list(id, assign)] 
    d_res = dta_3 %>%
      group_by(assign, time) %>%
      summarize(pr_ev = weighted.mean(1-pr_surv, w = freqwt),
                .groups = 'drop') %>%
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
                    .f = ~d_fun_getplrwt(
                      select(d_panel_outcome, -cumpr_notreat),
                      d_panel_treat, d_ids, 
                      .x),
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
                 paste0('survplot_plradj', '.jpeg')),
            plot=d_gg_1, width = 12, height=8, dpi=300)

# Save PLR est results ----
  write_csv(d_res,
    file = here('out', paste0('plradjoutc', '.csv'
    ))
  )

d_output$results = d_res

saveRDS(d_output, 
        file = here('out', paste0('plradj.Rds')))