library(gtsummary)
library(magrittr)
library(dplyr)

model.data <- function(dataset){
  
  ## regression model
  model <-
    glm(
      OFI_delay ~ Total_GCS + Gender + Highest_care_level + Respiratory_rate + Systolic_blood_pressure + Intubated_prehospitaly + ISS + Age + weekday + work_hours,
      data = factors.data,
      family = binomial
    )

 # Your predictor variables
  predictor_variables <- c("Total_GCS", "Gender", "Highest_care_level", "Respiratory_rate", "Systolic_blood_pressure", "Intubated_prehospitaly", "ISS", "Age", "weekday", "work_hours")
  
  # Loop through predictor variables and store regression results
  results_list <- lapply(predictor_variables, function(var) {
    formula <- as.formula(paste("OFI_delay ~", var))
    res.logist <- glm(formula, data = factors.data, family = binomial)
    
    # Store the results summary in the list
    summary(res.logist)
  })
  
  result_data <- do.call(rbind, lapply(results_list, function(res) {
    cbind(
      variable = rownames(coef(res)),
      coef(res),
      p_values <- res$coefficients[, "Pr(>|z|)"]
    )
  }))
  
  fancy_table <-
    tbl_merge(
      tbls        = list(model, result_data),
      tab_spanner = c("Adjusted", "Unadjusted")
    )
  
  fancy_table
  
  
  
  ## view model
  model %>%
    tbl_regression(exponentiate = TRUE,
        pvalue_fun = ~ style_pvalue(.x, digits = 2),) %>%
    bold_p() %>%
    bold_labels()
    add_p(digits = c(p = 3))
    


  factors.data %>%
    tbl_summary(  # Create summary statistics for different groups
      by = OFI_delay,
      label = list(
        Age = "Age (years)",
        Gender = "Gender",
        Total_GCS = "GCS"),
        pvalue_fun = ~ style_pvalue(.x, digits = 2),
      
      statistic = list(all_continuous() ~ "{mean} ± {sd}"),
      digits = list(all_continuous() ~ c(2, 2))
    ) %>%
    

  
  return(model)
}
