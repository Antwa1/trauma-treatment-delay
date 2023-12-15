library(rofi)

clean.dataset <- function(dataset){
  ## create new colum for outcome to be observed
  dataset$OFI_delay <- as.factor(ifelse(dataset$Problemomrade_.FMP == "Lång tid till DT", "Delay to treatment",
                                               ifelse(dataset$Problemomrade_.FMP == "Lång tid till op", "Delay to treatment",
                                                      "No delay to treatment")))
  
  ## remove all columns except the analyzed ones
  dataset <-
    dataset[, c(
      "Gender",
      "ed_gcs_sum",
      "ed_rr_value",
      "ed_sbp_value",
      "pt_age_yrs",
      "ISS",
      "ofi",
      "OFI_delay",
      "pre_gcs_sum",
      "pre_rr_value",
      "pre_sbp_value",
      "NumberOfInjuries"
    )]
  
  ## remove rows without OFI
  cleaned.data <- subset(dataset, !is.na(ofi))

  return(cleaned.data)
}
