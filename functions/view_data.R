library(gtsummary)

view_data <- function(dataset){
  
  ## regression model
  model <- glm(OFI_delay ~ Total_GCS + Gender + survival_after_30_days + Highest_care_level + Respiratory_rate + Systolic_blood_pressure + dt_ed_first_ct + Intubated_prehospitaly + ISS + Age, data = New.subset, family = binomial)
  
  
  ## view model
  tbl_regression(model, exponentiate = TRUE)

  return(model)
}
