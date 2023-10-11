library(rofi)

New_subset <- function(dataset){
  
  prepared.data$OFI_delay <- ifelse(prepared.data$Problemomrade_.FMP == "Handläggning", "1", "0")

  cleaned_data <- subset(prepared.data, !is.na(OFI_delay))

  return(cleaned_data)
}