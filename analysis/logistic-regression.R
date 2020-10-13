source(here::here("data-wrangling", "read-sanatize-oc-covid-data.R"))
read_line_list()



model1 <- glm(formula = Positive ~ factor(AgeGroup) + Male + factor(Race) + Income + 
                                   factor(Month) + PopDen, 
              family = binomial(logistic), data = all_PCR1)