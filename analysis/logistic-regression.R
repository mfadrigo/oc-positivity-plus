library(lme4)
source(here::here("data-wrangling", "read-sanitize-oc-covid-data.R"))
all_pcr <- read_all_pcr()


all_pcr$population_density <- scale(all_pcr$population_density, 
                                   center = TRUE, 
                                   scale = TRUE)
all_pcr$med_adj_income <- scale(all_pcr$med_adj_income, 
                                    center = TRUE, 
                                    scale = TRUE)
all_pcr$percent_bachelors <- scale(all_pcr$percent_bachelors,
                                   center = TRUE,
                                   scale = TRUE)
all_pcr$percent_insured <- scale(all_pcr$percent_insured,
                                 center = TRUE,
                                 scale = TRUE)



pcr_march_to_june <- all_pcr[all_pcr$posted_month %in% c("3", "4", "5", "6"), ]
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