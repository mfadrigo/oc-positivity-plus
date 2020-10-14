library(tictoc)
library(lme4)
library(car)
library(mgcv)
source(here::here("data-wrangling", "read-sanitize-oc-covid-data.R"))
source(here::here("analysis", "helpful-data-analysis-functions.R"))

# Change file path for where you saved the all_ELR_PCR_tests_updated file
all_pcr <- read_all_pcr(file_path = "C:/Users/Catalina Medina/Documents/oc-positivity-plus-outer/All ELR PCR tests updated 10.05.20.csv")
pcr_march_to_june <- all_pcr[all_pcr$posted_month %in% c("3", "4", "5", "6"), ]


pcr_march_to_june$adj_population_density <- scale(pcr_march_to_june$population_density, 
                                   center = TRUE, 
                                   scale = TRUE)
pcr_march_to_june$adj_med_income <- scale(pcr_march_to_june$med_income, 
                                    center = TRUE, 
                                    scale = TRUE)
pcr_march_to_june$adj_percent_bachelors <- scale(pcr_march_to_june$percent_bachelors,
                                   center = TRUE,
                                   scale = TRUE)
pcr_march_to_june$adj_percent_insured <- scale(pcr_march_to_june$percent_insured,
                                 center = TRUE,
                                 scale = TRUE)
pcr_march_to_june$adj_time_days <- scale(pcr_march_to_june$time_days,
                                 center = TRUE,
                                 scale = FALSE)

pcr_march_to_june$adj_med_income_quartile <- with(pcr_march_to_june,
                                 cut(adj_med_income,
                                     breaks = quantile(adj_med_income, 
                                                       probs = seq(0, 1, by = 0.25)),
                                     include.lowest = TRUE,
                                     labels = c("Q1", "Q2", "Q3", "Q4")))
pcr_march_to_june$adj_per_bachelors_quartile <- with(pcr_march_to_june,
                                        cut(adj_percent_bachelors,
                                            breaks = quantile(adj_percent_bachelors, 
                                                              probs = seq(0, 1, by = 0.25)),
                                            include.lowest = TRUE,
                                            labels = c("Q1", "Q2", "Q3", "Q4")))
pcr_march_to_june$adj_per_insured_quartile <- with(pcr_march_to_june,
                                      cut(adj_percent_insured,
                                          breaks = quantile(adj_percent_insured, 
                                                            probs = seq(0, 1, by = 0.25)),
                                          include.lowest = TRUE,
                                          labels = c("Q1", "Q2", "Q3", "Q4")))


# Time discretized
tic()
model_month <- glmer(formula = covid_positive ~ age_group + sex + race + 
                               adj_per_bachelors_quartile + adj_per_insured_quartile +
                               adj_population_density + adj_med_income +
                               posted_month +
                               (1 | zip),              
                     family = binomial, 
                     data = pcr_march_to_june,
                     control = glmerControl(optimizer ="bobyqa", optCtrl=list(maxfun=100000)))
toc()

# Time discretized with interaction with income
tic()
model_month_int <- glmer(formula = covid_positive ~ age_group + sex + race + 
                                   adj_per_bachelors_quartile + adj_per_insured_quartile +
                                   adj_population_density + adj_med_income +
                                   posted_month + posted_month * adj_med_income +
                                   (1 | zip),              
                         family = binomial, 
                         data = pcr_march_to_june,
                         control = glmerControl(optimizer ="bobyqa", optCtrl=list(maxfun=100000)))
toc()

# Time continuous
tic()
model_time <- glmer(formula = covid_positive ~ age_group + sex + race + 
                              adj_per_bachelors_quartile + adj_per_insured_quartile +
                              adj_population_density + adj_med_income +
                              time_days + 
                              (1 | zip),              
                    family = binomial, 
                    data = pcr_march_to_june,
                    control = glmerControl(optimizer ="bobyqa", optCtrl=list(maxfun=100000)))
toc()

# Time continuous with interaction with income
tic()
model_time_int <- glmer(formula = covid_positive ~ age_group + sex + race + 
                                  adj_per_bachelors_quartile + adj_per_insured_quartile +
                                  adj_population_density + adj_med_income +
                                  time_days + time_days * adj_med_income +
                                  (1 | zip),              
                        family = binomial, 
                        data = pcr_march_to_june,
                        control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
toc()


(model_month_sum <- compute_exp_ci(model_month, 
                                   model_name = "Model with month as predictor"))
(model_month_int_sum <- compute_exp_ci(model_month_int, 
                                       model_name = "Model with month as predictor and interaction with income"))
(model_time_sum <- compute_exp_ci(model_time, 
                                  model_name = "Model with days as predictor"))
(model_time_int_sum <- compute_exp_ci(model_time_int, 
                                      model_name = "Model with days as predictor and interaction with income"))

ggplot(pcr_march_to_june, aes(x = adj_population_density)) + 
  geom_histogram()
ggplot(pcr_march_to_june, aes(x = adj_med_income)) + 
  geom_histogram()
ggplot(pcr_march_to_june, aes(x = time_days)) + 
  geom_histogram()
ggplot(pcr_march_to_june, aes(x = adj_time_days)) + 
  geom_histogram()


###########################################not working yet###################################################
# Time continuous gam model with thin plate regression spline
tic()
model_gam_time <- gam(covid_positive ~ age_groups_2 + sex + race + 
                      adj_per_bachelors_quartile + adj_per_insured_quartile +
                      adj_population_density + adj_med_income +
                      s(time_days, bs = "tp") + 
                      (1 | zip),              
                    family = binomial, 
                    data = pcr_march_to_june)
toc()