library(survival)
library(survminer)
library(tidyverse)

set.seed(282716)

# Weibull distribution ----

  # 1000 observations 
  # half to treat = 0, half to treat=1
  # 

d_surv = tibble(treat = c(rep(0, 500), rep(1, 500)))

# Survival data definitions


sdef <- defSurv(varname = "survTime", formula = "1.5*x1", scale = "grp*5000 + (1-grp)*4500",
                shape = "grp*1 + (1-grp)*1.1")

sdef <- defData(sdef, varname = "censorTime", scale = 10000, shape = 1)

sdef

dtSurv <- genData(1000, def)
dtSurv[, censorTime = ]
dtSurv <- genSurv(dtSurv, sdef, timeName = "obsTime", censorName = "censorTime",
                  eventName = "status", keepEvents = TRUE)

dtSurv$censorTime = if_else(dtSurv$censorTime>1080, 1081, dtSurv$censorTime)

head(dtSurv)

survfit(Surv(obsTime, status) ~ grp, data = dtSurv)

ggsurvplot(survfit(Surv(obsTime, status) ~ grp, data = dtSurv),
           risk.table = T, cumevents = T)
