library(rofi)

New_subset <- function(dataset){
  
  ## create new colum for outcome to be observed
  prepared.data$OFI_delay <- as.factor(ifelse(prepared.data$Problemomrade_.FMP == "Handläggning", "Delay to treatment", "No delay to treatment"))
  
  ## remove all columns except the analyzed ones
  prepare.data <- prepared.data[, c("Gender", "res_survival", "host_care_level", "ed_gcs_sum", "ed_rr_value", "ed_sbp_value", "dt_ed_first_ct", "pre_intubated", "pt_age_yrs", "ISS", "ofi", "OFI_delay", "pre_gcs_sum", "ed_rr_rtscat", "DateTime_ArrivalAtHospital")]
  
  ## remove rows without OFI
  cleaned_data <- subset(prepare.data, !is.na(ofi))

  return(cleaned_data)
}
