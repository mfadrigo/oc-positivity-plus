library(lme4)
source(here::here("data-wrangling", "read-sanatize-oc-covid-data.R"))
all_pcr <- read_all_pcr()

all_pcr$population_density <- scale(all_pcr$population_density, 
                                   center = TRUE, 
                                   scale = TRUE)
all_pcr$med_adj_income <- scale(all_pcr$med_adj_income, 
                                    center = TRUE, 
                                    scale = TRUE)

model1 <- glmer(formula = covid_positive ~ age_groups + sex + race + 
                          med_adj_income + month + population_density + 
                          (1|zip), 
              family = binomial, 
              data = all_pcr, 
              subset = (month %in% c("3", "4", "5", "6")))


source(here::here("analysis", "dans-glm-transformed-ci-function.R"))
m1_sum <- glmCI(model = model1, transform = TRUE, robust = TRUE)
m1_sum
############################################################################################################################################################

###Model diagnostics
###Worried about multicolinearity due to variables based on zipcode
#Check vif
library(car)
vif(model1)