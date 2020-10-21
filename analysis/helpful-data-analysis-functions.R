compute_exp_ci <- function(model, alpha = 0.05, model_name = "model"){
  model_summary <- summary(model)
  coeffs <- model_summary$coefficients[, 1]
  std_err <- model_summary$coefficients[, 2]
  ci_lower_bound <- coeffs - qnorm(1 - alpha / 2) * std_err
  ci_upper_bound <- coeffs + qnorm(1 - alpha / 2) * std_err
  cat(paste(model_name, "\n", 
            "with ", (1 - alpha) * 100, "% Exponentiated Confidence Intervals (not robust std error) \n", 
            collapse = ""))
  cbind("exp_est" = exp(coeffs), 
        "exp_ci_lb" = exp(ci_lower_bound),
        "exp_ci_ub" = exp(ci_upper_bound),
        "std_err" = std_err,
        "p-value" = model_summary$coefficients[, 4])
}

par_names <- c("Intercept",
               "5-9",
               "10-14",
               "15-19",
               "20-24",
               "25-29",
               "30-34",
               "35-39",
               "40-49",
               "50-59",
               "60-69",
               "70-79",
               "80plus",
               "male",
               "native_amer",
               "asian",
               "black",
               "mult_races",
               "pacific_isl",
               "other_race",
               "per_bach_Q2",
               "per_bach_Q3",
               "per_bach_Q4",
               "per_insur_Q2",
               "per_insur_Q3",
               "per_insur_Q4",
               "pop_density",
               "med_income")