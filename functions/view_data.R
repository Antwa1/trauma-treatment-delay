library(gtsummary)

model.data <- function(dataset){
  
  ## regression model
  model <-
    glm(
      OFI_delay ~ Total_GCS + Gender + survival_after_30_days + Highest_care_level + Respiratory_rate + Systolic_blood_pressure + Time_until_first_CT + Intubated_prehospitaly + ISS + Age + weekday + work_hours,
      data = factors.data,
      family = binomial
    )
  
  
  ## view model
  tbl_regression(model, exponentiate = TRUE)

  factors.data %>%
    tbl_summary(  # Create summary statistics for different groups
      by = OFI_delay,
      label = list(
        Age = "Age (years)",
        Gender = "Gender",
        Total_GCS = "GCS"
      ),
      statistic = list(all_continuous() ~ "{mean} ± {sd}"),
      digits = list(all_continuous() ~ c(2, 2))
    ) %>%
    add_p()  # Add p-values
  
  
  return(model)
}
