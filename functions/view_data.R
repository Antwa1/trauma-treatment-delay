library(rofi, gtsummary)

view_data <- function(dataset){
  
  ## regression model
  model <- glm(OFI_delay ~ ed_gcs_sum + Gender + res_survival + host_care_level + ed_rr_value + ed_sbp_value + dt_ed_first_ct + pre_intubated + ISS + pt_age_yrs, data = New.subset, family = binomial)
  
  ## view model
  tbl_regression(model, exponentiate = TRUE)

  return(model)
}
