
compute_ci_logistic <- function(model, alpha = 0.05, model_name = "model", plot_ci = TRUE){
  model_summary <- summary(model)
  coeffs <- model_summary$coefficients[, 1]
  std_err <- model_summary$coefficients[, 2]
  ci_lower_bound <- coeffs - qnorm(1 - alpha / 2) * std_err
  ci_upper_bound <- coeffs + qnorm(1 - alpha / 2) * std_err
  
  model_sum <- data.frame("odds" = exp(coeffs), 
                          "lower_bound" = exp(ci_lower_bound),
                          "upper_bound" = exp(ci_upper_bound),
                          "se" = std_err,
                          "p-value" = model_summary$coefficients[, 4],
                          "par_names" = names(coeffs))
  
  if (plot_ci) {
    ci_plot <- ggplot(model_sum, 
                      aes(x = par_names, y = odds),
                      size = 5, colour = "red") +
      ylim(min(model_sum$lower_bound), max(model_sum$upper_bound)) +
      geom_point() +
      geom_errorbar(aes(ymin = lower_bound, ymax = upper_bound), 
                    colour = "black") +
      geom_hline(yintercept = 1, linetype = "dashed", colour = "gray") +
      theme(axis.text.x = element_text(face = "bold", 
                                       size = 12, angle = 90)) +
      ggtitle(model_name)
    
    return(list("model_sum" = model_sum, "ci_plot" = ci_plot))
  }
  
  model_sum
}


compute_ci_gam_logistic <- function(model, alpha = 0.05, model_name, plot_estimates = TRUE) {
  model_summary <- summary(model)
  coeffs <- model_summary$p.coeff
  se <- sqrt(diag(vcov(model, unconditional = TRUE)))[1:length(coeffs)]
  model_sum <- data.frame("odds" = exp(coeffs),
                          "lower_bound" = exp(coeffs - qnorm(1 - alpha / 2) * se),
                          "upper_bound" = exp(coeffs + qnorm(1 - alpha / 2) * se),
                          "p-value" =  model_summary$p.pv,
                          "par_names" = factor(names(coeffs)), levels = names(coeffs))
  
  if (plot_estimates) {
    ci_plot <- ggplot(model_sum, 
                        aes(x = par_names, y = odds),
                        size = 5, colour = "red") +
    ylim(min(model_sum$lower_bound), max(model_sum$upper_bound)) +
    geom_point() +
    geom_errorbar(aes(ymin = lower_bound, ymax = upper_bound), 
                  colour = "black") +
    geom_hline(yintercept = 1, linetype = "dashed", colour = "gray") +
    theme(axis.text.x = element_text(face = "bold", 
                                     size = 12, angle = 45)) +
    ggtitle(model_name)
    
    return(list("model_sum" = model_sum, "ci_plot" = ci_plot))
  }
  
  return("model_sum" = model_sum)
}


# par_names <- c("Intercept",
#                "5-9",
#                "10-14",
#                "15-19",
#                "20-24",
#                "25-29",
#                "30-34",
#                "35-39",
#                "40-49",
#                "50-59",
#                "60-69",
#                "70-79",
#                "80plus",
#                "male",
#                "native_amer",
#                "asian",
#                "black",
#                "mult_races",
#                "pacific_isl",
#                "other_race",
#                "per_bach_Q2",
#                "per_bach_Q3",
#                "per_bach_Q4",
#                "per_insur_Q2",
#                "per_insur_Q3",
#                "per_insur_Q4",
#                "pop_density",
#                "med_income")
# 
# par_names2 <- c("Intercept",
#                "5-9",
#                "10-14",
#                "15-19",
#                "20-24",
#                "25-29",
#                "30-34",
#                "35-39",
#                "40-49",
#                "50-59",
#                "60-69",
#                "70-79",
#                "80plus",
#                "male",
#                "native_amer",
#                "asian",
#                "black",
#                "pacific_isl",
#                "other_race",
#                "per_bach_Q2",
#                "per_bach_Q3",
#                "per_bach_Q4",
#                "per_insur_Q2",
#                "per_insur_Q3",
#                "per_insur_Q4",
#                "pop_density",
#                "med_income")
