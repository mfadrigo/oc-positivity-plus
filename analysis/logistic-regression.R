library(tictoc)
library(here)
library(lme4)
library(car)
library(mgcv)
library(ggplot2)
source(here::here("data-wrangling", "read-sanitize-oc-covid-data.R"))
source(here::here("analysis", "helpful-data-analysis-functions.R"))
# Change file path for where you saved the all_ELR_PCR_tests_updated file

all_pcr_and_zip <- read_all_pcr(file_path = here("data", "all-elr-pcr-tests-updated-2020-10-05.csv"),
                                start_date = "2020-03-01",
                                end_date = "2020-08-16")
all_pcr <- data.frame(all_pcr_and_zip[["pcr_results_merged"]])
all_zip <- data.frame(all_pcr_and_zip[["zip_data_merged"]])
all_counts <- all_pcr_and_zip[["counts"]]
all_inconsist_ids <- all_pcr_and_zip[["inconsistencies"]]


tic()
fit_time_lin <- glmer(formula = covid_positive ~ age_group + sex + race + 
                        adj_perc_bach_quar + adj_perc_insured_quar +
                          adj_pop_density + adj_med_income +
                          I(adj_time_days) +
                          (1 | zip),              
                        family = binomial, 
                        data = all_pcr,
                        control = glmerControl(optimizer ="bobyqa", optCtrl = list(maxfun = 100000)))
toc()

fit_time_lin_info <- compute_ci_logistic(fit_time_lin, 
                                          model_name = "Model with days as linear predictor",
                                          plot_ci = TRUE)
fit_time_lin_summary <- fit_time_lin_info[["model_sum"]]
fit_time_lin_plot <- fit_time_lin_info[["ci_plot"]]


tic()
fit_time_quad <- glmer(formula = covid_positive ~ age_group + sex + race + 
                         adj_perc_bach_quar + adj_perc_insured_quar +
                           adj_pop_density + adj_med_income +
                           I(adj_time_days) + I(adj_time_days^2) + 
                           (1 | zip),              
                         family = binomial, 
                         data = all_pcr,
                         control = glmerControl(optimizer ="bobyqa", optCtrl = list(maxfun = 100000)))
toc()

fit_time_quad_info <- compute_ci_logistic(fit_time_quad, 
                                          model_name = "Model with days as quad predictor")
fit_time_quad_summary <- fit_time_quad_info[["model_sum"]]
fit_time_quad_plot <- fit_time_quad_info[["ci_plot"]]


tic()
fit_time_gam <- gam(covid_positive ~ age_group + sex + race + 
                        adj_perc_bach_quar + adj_perc_insured_quar +
                        adj_pop_density  + adj_med_income +
                        s(adj_time_days, bs = "ts", k = -1) + 
                        s(zip, bs = "re"),              
                      family = binomial, 
                      data = all_pcr,
                      method = "REML",
                      gamma = 1.5)
toc()

fit_time_gam_info <- compute_ci_gam_logistic(model = fit_time_gam, 
                                              model_name = "Model time gam no interaction")
fit_time_gam_summary <- fit_time_quad_info[["model_sum"]]
fit_time_gam_plot <- fit_time_quad_info[["ci_plot"]]
gam_smooth_var_ci <- gam.vcomp(fit_time_gam)


tic()
fit_time_gam_int <- gam(covid_positive ~ age_group + sex + race + 
                            adj_perc_bach_quar + adj_perc_insured_quar +
                            adj_pop_density  + adj_med_income +
                            s(adj_time_days) +
                            ti(adj_time_days, adj_med_income, bs = "ts") + 
                            s(zip, bs = "re"),              
                          family = binomial, 
                          data = all_pcr,
                          method = "REML",
                          gamma = 1.5)
toc()

fit_time_gam_inter_info <- compute_ci_gam_logistic(model = fit_time_gam_int, 
                                             model_name = "Model time gam no interaction")
fit_time_gam_inter_summary <- fit_time_gam_inter_info[["model_sum"]]
fit_time_gam_inter_plot <- fit_time_gam_inter_info[["ci_plot"]]
gam_smooth_var_ci <- gam.vcomp(fit_time_gam_int)





BIC(fit_time_lin, fit_time_quad)
BIC(fit_time_gam, fit_time_gam_int)

fit_time_lin_plot
fit_time_quad_plot
fit_time_gam_plot
fit_time_gam_inter_plot

save(fit_time_lin, file = here("analysis/regression-results", "fit_time_lin.Rdata"))
save(fit_time_quad, file = here("analysis/regression-results", "fit_time_quad.Rdata"))
save(fit_time_gam, file = here("analysis/regression-results", "fit_time_gam.Rdata"))
save(fit_time_gam_int, file = here("analysis/regression-results", "fit_time_gam_inter.Rdata"))
save(all_pcr_and_zip, file = here("data", "cleaned_process_pcr_data.Rdata"))
