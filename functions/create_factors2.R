library(stringr)

## Creating function
create.factors2 <- function(dataset){
 

  ## Making gender into a factor
  dataset <- subset(dataset, !is.na(Gender))
  
  
  dataset$Gender <- as.factor(dataset$Gender)

  ##Fixing years
  dataset <- subset(dataset, !(is.na(pt_age_yrs) | pt_age_yrs == 999))
  
  dataset$Age <- as.numeric(dataset$pt_age_yrs)
  
  ##Fixing ISS
  dataset <- subset(dataset, !(is.na(ISS) | ISS == 999))
  
  dataset$ISS <- as.numeric(dataset$ISS)
  
  ##Number of injuries removing missing values and making it continuous
  dataset <- subset(dataset, !(is.na(NumberOfInjuries) | NumberOfInjuries == 999))
  
  dataset$Number_of_injuries <- as.numeric(dataset$NumberOfInjuries)
  
  ## SBP into rts and replacing missing values
  
  dataset <- dataset %>%
    mutate(ed_sbp_value = if_else(is.na(ed_sbp_value) | ed_sbp_value == 999 | ed_sbp_value == 99, pre_sbp_value, ed_sbp_value))
  
  dataset <- dataset %>%
    mutate(ed_sbp_value = na_if(ed_sbp_value, 99)) %>%
    mutate(ed_sbp_value = na_if(ed_sbp_value, 999))
  
  dataset <- subset(dataset, !(is.na(ed_sbp_value)))
  
  dataset$Systolic_blood_pressure <- as.numeric(dataset$ed_sbp_value)
  
  ## Respiratory rate into rts and replacing missing values
  dataset <- dataset %>%
    mutate(ed_rr_value = if_else(is.na(ed_rr_value) | ed_rr_value == 999 | ed_rr_value == 99, pre_rr_value, ed_rr_value))
  
  dataset <- dataset %>%
    mutate(ed_rr_value = na_if(ed_rr_value, 99)) %>%
    mutate(ed_rr_value = na_if(ed_rr_value, 999))
  
  dataset <- subset(dataset, !(is.na(ed_rr_value)))
  
  dataset$Respiratory_rate <- as.numeric(dataset$ed_rr_value)
  
  ## Making gcs into rts and replacing missing values
  
  dataset <- dataset %>%
    mutate(ed_gcs_sum = if_else(is.na(ed_gcs_sum) | ed_gcs_sum == 999 | ed_gcs_sum == 99, pre_gcs_sum, ed_gcs_sum))
  
  dataset <- dataset %>%
    mutate(ed_gcs_sum = na_if(ed_gcs_sum, 99)) %>%
    mutate(ed_gcs_sum = na_if(ed_gcs_sum, 999))
  
  dataset <- subset(dataset, !(is.na(ed_gcs_sum)))
  
  dataset$Total_GCS <- as.numeric(dataset$ed_gcs_sum)
  
  
  ##Replacing NA in OFI_dealy with no delay
  dataset$OFI_delay[is.na(dataset$OFI_delay)] <- "No delay to treatment"
  
  dataset$OFI_delay <- as.factor(dataset$OFI_delay)
  
  dataset$ofi <- as.factor(dataset$ofi)
 
  ##removing redundant columns
  dataset <-
    dataset[,!(
      names(dataset) %in% c(


        "ed_gcs_sum",
        "ed_rr_value",
        "ed_sbp_value",
        "pt_age_yrs",
        "pre_gcs_sum",
        "pre_rr_value",
        "pre_sbp_value",
        "ofi",
        "NumberOfInjuries"
      )
    )]

  
  return(dataset)
}

