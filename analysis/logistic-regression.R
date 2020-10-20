library(tictoc)
library(lme4)
library(car)
library(mgcv)
source(here::here("data-wrangling", "read-sanitize-oc-covid-data.R"))
source(here::here("analysis", "helpful-data-analysis-functions.R"))

# Change file path for where you saved the all_ELR_PCR_tests_updated file
all_pcr_and_zip <- read_all_pcr(file_path = "C:/Users/Catalina Medina/Documents/oc-positivity-plus-outer/All ELR PCR tests updated 10.05.20.csv",
                                start_date = "2020-03-01",
                                end_date = "2020-08-16")
all_pcr <- data.frame(all_pcr_and_zip[["pcr_results_merged"]])
all_zip <- data.frame(all_pcr_and_zip[["zip_data_merged"]])
all_counts <- all_pcr_and_zip[["counts"]]


all_pcr$adj_time_days <- scale(all_pcr$time_days,
                               center = TRUE,
                               scale = TRUE)
all_pcr$adj_pop_density <- scale(all_pcr$pop_density, 
                                        center = TRUE, 
                                        scale = TRUE)
all_pcr$adj_med_income <- scale(all_pcr$med_income, 
                                center = TRUE, 
                                scale = TRUE)
all_pcr$adj_med_income_quartile <- with(all_pcr,
                                        cut(adj_med_income,
                                            breaks = quantile(adj_med_income, 
                                                              probs = seq(0, 1, by = 0.25)),
                                            include.lowest = TRUE,
                                            labels = c("Q1", "Q2", "Q3", "Q4")))

all_pcr$adj_perc_bachelors <- scale(all_pcr$percent_bachelors,
                                    center = TRUE,
                                    scale = TRUE)
all_pcr$adj_perc_bachelors_quartile <- with(all_pcr,
                                            cut(adj_perc_bachelors,
                                                breaks = quantile(adj_perc_bachelors, 
                                                                  probs = seq(0, 1, by = 0.25)),
                                                include.lowest = TRUE,
                                                labels = c("Q1", "Q2", "Q3", "Q4")))

all_pcr$adj_perc_insured <- scale(all_pcr$percent_insured,
                                  center = TRUE,
                                  scale = TRUE)
all_pcr$adj_perc_insured_quartile <- with(all_pcr,
                                          cut(adj_perc_insured,
                                              breaks = quantile(adj_perc_insured, 
                                                                probs = seq(0, 1, by = 0.25)),
                                              include.lowest = TRUE,
                                              labels = c("Q1", "Q2", "Q3", "Q4")))


# ggplot(all_pcr, aes(x = adj_population_density)) + 
#   geom_histogram()
# ggplot(all_pcr, aes(x = adj_med_income)) + 
#   geom_histogram()
# ggplot(all_pcr, aes(x = adj_time_days)) + 
#   geom_histogram()


tic()
model_time_lin <- glmer(formula = covid_positive ~ age_group + sex + race + 
                                  adj_perc_bachelors_quartile + adj_perc_insured_quartile +
                                  adj_pop_density + adj_med_income +
                                  I(adj_time_days) +
                                  (1 | zip),              
                    family = binomial, 
                    data = all_pcr,
                    control = glmerControl(optimizer ="bobyqa", optCtrl = list(maxfun = 100000)))
toc()

(model_time_lin_sum <- compute_exp_ci(model_time_lin, 
                                  model_name = "Model with days as linear predictor"))

tic()
model_time_quad <- glmer(formula = covid_positive ~ age_group + sex + race + 
                                   adj_perc_bachelors_quartile + adj_perc_insured_quartile +
                                   adj_pop_density + adj_med_income +
                                   I(adj_time_days) + I(adj_time_days^2) + 
                                   (1 | zip),              
                        family = binomial, 
                        data = all_pcr,
                        control = glmerControl(optimizer ="bobyqa", optCtrl = list(maxfun = 100000)))
toc()


(model_time_quad_sum <- compute_exp_ci(model_time_quad, 
                                  model_name = "Model with days as quad predictor"))


tic()
model_time_gam <- gam(covid_positive ~ age_group + sex + race + 
                                       adj_perc_bachelors_quartile + adj_perc_insured_quartile +
                                       adj_pop_density  + adj_med_income +
                                       s(adj_time_days, bs = "ts", k = -1) + 
                                       s(zip, bs = "re"),              
                      family = binomial, 
                      data = all_pcr,
                      method = "REML",
                      gamma = 1.5)
toc()

tic()
model_time_gam_int <- gam(covid_positive ~ age_group + sex + race + 
                        adj_perc_bachelors_quartile + adj_perc_insured_quartile +
                        adj_pop_density  + adj_med_income +
                        s(adj_time_days, adj_med_income, bs = "ts", k = -1) + 
                        s(zip, bs = "re"),              
                      family = binomial, 
                      data = all_pcr,
                      method = "REML",
                      gamma = 1.5)
toc()