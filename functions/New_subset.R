library(rofi)

New_subset <- function(dataset){
  
  ## create new colum for outcome to be observed
  prepared.data$OFI_delay <- as.factor(ifelse(prepared.data$Problemomrade_.FMP == "Handläggning", "Delay to treatment", "No delay to treatment"))
  
  ## remove rows without OFI
  cleaned_data <- subset(prepared.data, !is.na(ofi))

  return(cleaned_data)
}