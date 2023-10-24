library(gtsummary)

model.data <- function(dataset){
  
  ## regression model
  model <-
    glm(
      OFI_delay ~ Total_GCS + Gender + survival_after_30_days + Highest_care_level + Respiratory_rate + Systolic_blood_pressure + Time_until_first_CT + Intubated_prehospitaly + ISS + Age + weekday + work_hours,
      data = dataset,
      family = binomial
    )
  
  
  ## view model
  tbl_regression(model, exponentiate = TRUE)

  return(model)
}
