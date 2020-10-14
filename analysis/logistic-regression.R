library(tictoc)
library(lme4)
library(car)
source(here::here("data-wrangling", "read-sanitize-oc-covid-data.R"))

# Change file path for where you saved the data
all_pcr <- read_all_pcr(file_path = "C:/Users/Catalina Medina/Documents/oc-positivity-plus-outer/All ELR PCR tests updated 10.05.20.csv")
all_pcr_1 <- all_pcr

all_pcr_1$adj_population_density <- scale(all_pcr_1$population_density, 
                                   center = TRUE, 
                                   scale = TRUE)
all_pcr_1$adj_med_income <- scale(all_pcr_1$med_income, 
                                    center = TRUE, 
                                    scale = TRUE)
all_pcr_1$adj_percent_bachelors <- scale(all_pcr_1$percent_bachelors,
                                   center = TRUE,
                                   scale = TRUE)
all_pcr_1$adj_percent_insured <- scale(all_pcr_1$percent_insured,
                                 center = TRUE,
                                 scale = TRUE)

all_pcr_1$adj_med_income_quartile <- with(all_pcr_1,
                                 cut(adj_med_income,
                                     breaks = quantile(adj_med_income, 
                                                       probs = seq(0, 1, by = 0.25)),
                                     include.lowest = TRUE,
                                     labels = c("Q1", "Q2", "Q3", "Q4")))
all_pcr_1$adj_per_bachelors_quartile <- with(all_pcr_1,
                                        cut(adj_percent_bachelors,
                                            breaks = quantile(adj_percent_bachelors, 
                                                              probs = seq(0, 1, by = 0.25)),
                                            include.lowest = TRUE,
                                            labels = c("Q1", "Q2", "Q3", "Q4")))
all_pcr_1$adj_per_insured_quartile <- with(all_pcr_1,
                                      cut(adj_percent_insured,
                                          breaks = quantile(adj_percent_insured, 
                                                            probs = seq(0, 1, by = 0.25)),
                                          include.lowest = TRUE,
                                          labels = c("Q1", "Q2", "Q3", "Q4")))


pcr_march_to_june <- all_pcr_1[all_pcr_1$posted_month %in% c("3", "4", "5", "6"), ]

tic()
model_month <- glmer(formula = covid_positive ~ age_groups_2 + sex + race + 
                               adj_per_bachelors_quartile + adj_per_insured_quartile
                               adj_population_density + adj_med_income +
                               posted_month +
                               (1 | zip),              
                     family = binomial, 
                     data = pcr_march_to_june,
                     control = glmerControl(optimizer ="bobyqa", optCtrl=list(maxfun=100000)))
toc()
summary_model_month <- summary(model_month)

tic()
model_month_int <- glmer(formula = covid_positive ~ age_groups_2 + sex + race + 
                                   adj_per_bachelors_quartile + adj_per_insured_quartile
                                   adj_population_density + adj_med_income +
                                   posted_month + posted_month * adj_med_income +
                                   (1 | zip),              
                         family = binomial, 
                         data = pcr_march_to_june,
                         control = glmerControl(optimizer ="bobyqa", optCtrl=list(maxfun=100000)))
toc()
summary_model_month_int <- summary(model_month_int)



