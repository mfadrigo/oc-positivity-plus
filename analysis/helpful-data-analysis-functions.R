compute_exp_ci <- function(model, alpha = 0.05, model_name = "model"){
  model_summary <- summary(model)
  coeffs <- model_summary$coefficients[, 1]
  std_err <- model_summary$coefficients[, 2]
  ci_lower_bound <- exp(coeffs - qnorm(1 - alpha / 2) * std_err)
  ci_upper_bound <- exp(coeffs + qnorm(1 - alpha / 2) * std_err)
  cat(paste(model_name, "\n", 
            "with ", (1 - alpha) * 100, "% Exponentiated Confidence Intervals (not robust std error) \n", 
            collapse = ""))
  cbind("exp_est" = exp(coeffs), 
        "exp_ci_lb" = ci_lower_bound,
        "exp_ci_ub" = ci_upper_bound,
        "std_err" = std_err,
        "p-value" = model_summary$coefficients[, 4])
}