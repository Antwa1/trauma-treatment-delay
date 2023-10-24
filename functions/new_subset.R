library(rofi)

clean.dataset <- function(dataset){
  ## create new colum for outcome to be observed
  dataset$OFI_delay <- as.factor(ifelse(dataset$Problemomrade_.FMP == "Handläggning", "Delay to treatment", "No delay to treatment"))
  
  ## remove all columns except the analyzed ones
  dataset <-
    dataset[, c(
      "Gender",
      "res_survival",
      "host_care_level",
      "ed_gcs_sum",
      "ed_rr_value",
      "ed_sbp_value",
      "dt_ed_first_ct",
      "pre_intubated",
      "pt_age_yrs",
      "ISS",
      "ofi",
      "OFI_delay",
      "pre_gcs_sum",
      "ed_rr_rtscat",
      "ed_sbp_rtscat",
      "DateTime_ArrivalAtHospital"
    )]
  
  ## remove rows without OFI
  cleaned.data <- subset(dataset, !is.na(ofi))

  return(cleaned.data)
}
