library(tictoc)
library(lme4)
library(car)
library(mgcv)
library(ggplot2)
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
model_time_lin_sum <- data.frame(model_time_lin_sum, par_names = c(par_names, "days"))

glmer_lin_ci_plot <- ggplot(model_time_lin_sum, 
                      aes(x = par_names, y = odds),
                      size = 5, colour = "red") +
               ylim(min(model_time_lin_sum$lower_bound), max(model_time_lin_sum$upper_bound)) +
               geom_point() +
               geom_errorbar(aes(ymin = lower_bound, ymax = upper_bound), 
                            colour = "black") +
               geom_hline(yintercept = 1, linetype = "dashed", colour = "gray") +
               theme(axis.text.x = element_text(face = "bold", 
                                               size = 12, angle = 45)) +
               ggtitle("GLMER model with time linear")
  


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


model_time_quad_sum <- compute_exp_ci(model_time_quad, 
                                  model_name = "Model with days as quad predictor")
model_time_quad_sum <- data.frame(model_time_quad_sum, par_names = c(par_names, "days", "days^2"))

glmer_quad_ci_plot <- ggplot(model_time_quad_sum, 
                            aes(x = par_names, y = odds),
                            size = 5, colour = "red") +
                      ylim(min(model_time_quad_sum$lower_bound), max(model_time_quad_sum$upper_bound)) +
                      geom_point() +
                      geom_errorbar(aes(ymin = lower_bound, ymax = upper_bound), 
                                    colour = "black") +
                      geom_hline(yintercept = 1, linetype = "dashed", colour = "gray") +
                      theme(axis.text.x = element_text(face = "bold", 
                                                       size = 12, angle = 45)) +
                      ggtitle("GLMER model with time quadratic")

tic()
#abut 8 minutes to run
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

#Note:here I use the variance-covariance matrix that is adjusted to account for 
#the selection of the smoothness parameter for the smooth in the model
#This is a ci for the variance components
gam_smooth_var_ci <- gam.vcomp(model_time_gam)

model_time_gam_sum <- summary(model_time_gam)
coeffs <- model_time_gam_sum$p.coeff
se <- sqrt(diag(vcov(model_time_gam, unconditional = TRUE)))[1:length(coeffs)]
model_gam_time_sum <- data.frame("odds" = exp(coeffs),
                         "lower_bound" = exp(coeffs - qnorm(0.975) * se),
                         "upper_bound" = exp(coeffs + qnorm(0.975) * se),
                         "par_names" = factor(par_names, levels = par_names))

#need to plot each separately
gam_ci_plot <- ggplot(model_gam_time_sum, 
                      aes(x = par_names, y = odds),
                      size = 5, colour = "red") +
  ylim(min(model_gam_time_sum$lower_bound), max(model_gam_time_sum$upper_bound)) +
  geom_point() +
  geom_errorbar(aes(ymin = lower_bound, ymax = upper_bound), 
                colour = "black") +
  geom_hline(yintercept = 1, linetype = "dashed", colour = "gray") +
  theme(axis.text.x = element_text(face = "bold", 
                                   size = 12, angle = 45)) +
  ggtitle("GAM model")

tic()
#abut 6 minutes to run
#Warning message:
#In mgcv::te(adj_time_days, adj_med_income, bs = "ts", k = -1) :
#  one or more supplied k too small - reset to default
model_time_gam_int <- gam(covid_positive ~ age_group + sex + race + 
                                           adj_perc_bachelors_quartile + adj_perc_insured_quartile +
                                           adj_pop_density  + adj_med_income +
                                           s(adj_time_days) +
                                           ti(adj_time_days, adj_med_income, bs = "ts") + 
                                           s(zip, bs = "re"),              
                      family = binomial, 
                      data = all_pcr,
                      method = "REML",
                      gamma = 1.5)
toc()

#Note:here I use the variance-covariance matrix that is adjusted to account for 
#the selection of the smoothness parameter for the smooth in the model
#This is a ci for the variance components
gam_inter_smooth_var_ci <- gam.vcomp(model_time_gam_int)

model_time_inter_gam_sum <- summary(model_time_gam_int)
coeffs <- model_time_inter_gam_sum$p.coeff
se <- sqrt(diag(vcov(model_time_gam_int, unconditional = TRUE)))[1:length(coeffs)]
model_gam_time_inter_sum <- data.frame("odds" = exp(coeffs),
                         "lower_bound" = exp(coeffs - qnorm(0.975) * se),
                         "upper_bound" = exp(coeffs + qnorm(0.975) * se),
                         "par_names" = factor(par_names, levels = par_names))

#need to plot each separately
gam_inter_ci_plot <- ggplot(model_gam_time_inter_sum, 
                      aes(x = par_names, y = odds),
                      size = 5, colour = "red") +
  ylim(min(model_gam_time_inter_sum$lower_bound), max(model_gam_time_inter_sum$upper_bound)) +
  geom_point() +
  geom_errorbar(aes(ymin = lower_bound, ymax = upper_bound), 
                colour = "black") +
  geom_hline(yintercept = 1, linetype = "dashed", colour = "gray") +
  theme(axis.text.x = element_text(face = "bold", 
                                   size = 12, angle = 45)) +
  ggtitle("GAM model with time income interaction")



AIC(model_time_gam, model_time_gam_int)

glmer_lin_ci_plot
glmer_quad_ci_plot
gam_ci_plot
gam_inter_ci_plot
