library(tictoc)
library(here)
library(lme4)
library(car)
library(mgcv)
library(ggplot2)
source(here::here("data-wrangling", "read-sanitize-oc-covid-data.R"))
source(here::here("analysis", "helpful-data-analysis-functions.R"))
# Change file path for where you saved the all_ELR_PCR_tests_updated file

all_pcr_and_zip <- read_all_pcr(
  file_path = here("data", "all-elr-pcr-tests-updated-2020-10-05.csv"),
  start_date = "2020-03-01",
  end_date = "2020-08-16"
)
all_pcr <- data.frame(all_pcr_and_zip[["pcr_results_merged"]])
all_zip <- data.frame(all_pcr_and_zip[["zip_data_merged"]])
all_counts <- all_pcr_and_zip[["counts"]]
all_inconsist_ids <- all_pcr_and_zip[["inconsistencies"]]


tic()
fit_time_lin <- glmer(formula = covid_positive ~ age_group + sex + race + 
                        adj_perc_bach_quar + adj_perc_insured_quar +
                          adj_pop_density + adj_med_income +
                          adj_time_days +
                          (1 | zip),              
                        family = binomial, 
                        data = all_pcr,
                        control = glmerControl(optimizer ="bobyqa", optCtrl = list(maxfun = 100000)))
toc()
# 4478.8 sec elapsed
# updated and saved 10-29-2020


# hour and a half to run
tic()
fit_time_quad <- glmer(formula = covid_positive ~ age_group + sex + race + 
                         adj_perc_bach_quar + adj_perc_insured_quar +
                           adj_pop_density + adj_med_income +
                           I(adj_time_days) + I(adj_time_days^2) + 
                           (1 | zip),              
                         family = binomial, 
                         data = all_pcr,
                         control = glmerControl(optimizer ="bobyqa", optCtrl = list(maxfun = 2e6)))
toc()
# Does not converge, 
# not after a second run with bobyqa from the previous run's estimates
# or after runs with all different optimizers starting from the second run's estimates.
# The estimates from the first and third runs are almost identical and off in the hundreths from gam inter estimates
# updated and saved 11-1-2020


# Issure fixes from : 
# https://rstudio-pubs-static.s3.amazonaws.com/33653_57fc7b8e5d484c909b615d8633c01d51.html
# tic()
# ss <- getME(fit_time_quad1, c("theta","fixef"))
# fit_time_quad <- update(
#   fit_time_quad1,
#   start = ss,
#   control = glmerControl(
#     optimizer = "bobyqa",
#     optCtrl = list(maxfun = 2e6)
#   )
# )
# toc()
# # 3153.87
# 
# library(RCurl)
# library(optimx)
# library(dfoptim)
# tic()
# #afurl <- "https://raw.githubusercontent.com/lme4/lme4/master/misc/issues/allFit.R"
# #eval(parse(text = getURL(afurl)))
# aa <- allFit(fit_time_quad)
# is.OK <- sapply(aa, is, "merMod")  
# aa.OK <- aa[is.OK]
# lapply(aa.OK,function(x) x@optinfo$conv$lme4$messages)
# toc()
# 5008.36 sec elapsed
# Warning message:
# In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
# Model failed to converge with max|grad| = 0.00240301 (tol = 0.002, component 1)
# Model still did not converge after the second run starting from the previous run (but estimates do not seem far off)


tic()
fit_time_quad_inter <- glmer(
  formula = covid_positive ~ age_group + sex + race + 
            adj_perc_bach_quar + adj_perc_insured_quar +
            adj_pop_density + adj_med_income +
            adj_time_days + I(adj_time_days^2) + 
            adj_time_days:adj_med_income + I(adj_time_days^2):adj_med_income +
            (1 | zip),              
  family = binomial, 
  data = all_pcr,
  control = glmerControl(optimizer ="bobyqa", optCtrl = list(maxfun = 2e6)))
toc()
# 


tic()
fit_time_gam <- gam(covid_positive ~ age_group + sex + race + 
                        adj_perc_bach_quar + adj_perc_insured_quar +
                        adj_pop_density  + adj_med_income +
                        s(time_days, bs = "ts", k = -1) + 
                        s(zip, bs = "re"),              
                      family = binomial, 
                      data = all_pcr,
                      method = "REML",
                      gamma = 1.5)
toc()
# 392.55 sec elapsed
# updated and saved 10-29-2020



tic()
fit_time_gam_inter <- gam(covid_positive ~ age_group + sex + race + 
                            adj_perc_bach_quar + adj_perc_insured_quar +
                            adj_pop_density  + adj_med_income +
                            s(time_days) +
                            ti(time_days, adj_med_income, bs = "ts") + 
                            s(zip, bs = "re"),              
                          family = binomial, 
                          data = all_pcr,
                          method = "REML",
                          gamma = 1.5)
toc()
# 3925.58 sec elapsed
# updated and saved 10-29-2020



save(fit_time_quad_third_fit_all_optimizers, file = here("analysis/regression-results", "fit_time_quad_third_fit_all_optimizers.Rdata"))
save(fit_time_lin, file = here("analysis/regression-results", "fit_time_lin.Rdata"))
save(fit_time_quad, file = here("analysis/regression-results", "fit_time_quad.Rdata"))
save(fit_time_quad_inter, file = here("analysis/regression-results", "fit_time_quad_inter.Rdata"))
save(fit_time_gam, file = here("analysis/regression-results", "fit_time_gam.Rdata"))
save(fit_time_gam_inter, file = here("analysis/regression-results", "fit_time_gam_inter.Rdata"))
save(all_pcr_and_zip, file = here("data", "cleaned_process_pcr_data.Rdata"))

