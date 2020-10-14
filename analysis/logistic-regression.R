library(lme4)
source(here::here("data-wrangling", "read-sanitize-oc-covid-data.R"))
all_pcr <- read_all_pcr()
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

all_pcr_1$adj_med_income <- with(all_pcr_1,
                                 cut(adj_med_income,
                                     breaks = quantile(adj_med_income, 
                                                       probs = seq(0, 1, by = 0.25)),
                                     include.lowest = TRUE,
                                     labels = c("Q1", "Q2", "Q3", "Q4")))
all_pcr_1$adj_percent_bachelors <- with(all_pcr_1,
                                        cut(adj_percent_bachelors,
                                            breaks = quantile(adj_percent_bachelors, 
                                                              probs = seq(0, 1, by = 0.25)),
                                            include.lowest = TRUE,
                                            labels = c("Q1", "Q2", "Q3", "Q4")))
all_pcr_1$adj_percent_insured <- with(all_pcr_1,
                                      cut(adj_percent_insured,
                                          breaks = quantile(adj_percent_insured, 
                                                            probs = seq(0, 1, by = 0.25)),
                                          include.lowest = TRUE,
                                          labels = c("Q1", "Q2", "Q3", "Q4")))


pcr_march_to_june <- all_pcr_1[all_pcr_1$posted_month %in% c("3", "4", "5", "6"), ]
model1 <- glmer(formula = covid_positive ~ age_groups + sex + race + 
                          med_adj_income + posted_month + population_density + 
                          (1|zip), 
              family = binomial, 
              data = pcr_march_to_june,
              control = glmerControl(optimizer ="bobyqa", optCtrl=list(maxfun=100000)))


source(here::here("analysis", "dans-glm-transformed-ci-function.R"))
m1_sum <- glmCI(model = model1, transform = TRUE, robust = TRUE)
m1_sum
############################################################################################################################################################

###Model diagnostics
###Worried about multicolinearity due to variables based on zipcode
#Check vif
library(car)
vif(model1)