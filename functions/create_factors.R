library(rofi)
## Creating function
Create_factors <- function(dataset){
 
  ## Making gcs into rts and replacing missing values
  New.subset$ed_gcs_sum <- ifelse(New.subset$ed_gcs_sum >= 3 & New.subset$ed_gcs_sum <= 3, 0,
                     ifelse(New.subset$ed_gcs_sum >= 4 & New.subset$ed_gcs_sum <= 5, 1,
                            ifelse(New.subset$ed_gcs_sum >= 6 & New.subset$ed_gcs_sum <= 8, 2,
                                   ifelse(New.subset$ed_gcs_sum >= 9 & New.subset$ed_gcs_sum <= 12, 3,
                                          ifelse(New.subset$ed_gcs_sum >= 13 & New.subset$ed_gcs_sum <= 15, 4,  New.subset$ed_gcs_sum)))))
  
  New.subset$pre_gcs_sum <- ifelse(New.subset$pre_gcs_sum >= 3 & New.subset$pre_gcs_sum <= 3, 0,
                                  ifelse(New.subset$pre_gcs_sum >= 4 & New.subset$pre_gcs_sum <= 5, 1,
                                         ifelse(New.subset$pre_gcs_sum >= 6 & New.subset$pre_gcs_sum <= 8, 2,
                                                ifelse(New.subset$pre_gcs_sum >= 9 & New.subset$pre_gcs_sum <= 12, 3,
                                                       ifelse(New.subset$pre_gcs_sum >= 13 & New.subset$pre_gcs_sum <= 15, 4,  New.subset$pre_gcs_sum)))))
                                  
  New.subset$ed_gcs_sum <- ifelse(is.na(New.subset$ed_gcs_sum) | New.subset$ed_gcs_sum == 999 | New.subset$ed_gcs_sum == 99, New.subset$pre_gcs_sum, New.subset$ed_gcs_sum)  
  
  New.subset <- subset(New.subset, !(is.na(ed_gcs_sum) | ed_gcs_sum == 999 | ed_gcs_sum == 99))
  
  New.subset$Total_GCS <- as.factor(New.subset$ed_gcs_sum)
  
  ## Making gender into a factor
  New.subset <- subset(New.subset, !is.na(Gender))
  
  
  New.subset$Gender <- as.factor(New.subset$Gender)
  
  
  ## Survival after 30 days
  New.subset <- subset(New.subset, !(is.na(res_survival) | res_survival == 999 | res_survival == 99))

  New.subset$res_survival <- ifelse(New.subset$res_survival == 1, "Dead", "Alive")
   
  New.subset$survival_after_30_days <- as.factor(New.subset$res_survival)
  

  ## Highest hospital care level
  New.subset$host_care_level <- ifelse(New.subset$host_care_level == 1, "ER",
                               ifelse(New.subset$host_care_level == 2, "General care department",
                                      ifelse(New.subset$host_care_level == 3, "OR",
                                             ifelse(New.subset$host_care_level == 4, "Specialized care department",
                                                    ifelse(New.subset$host_care_level == 5, "ICU", NA)))))
  
  New.subset <- subset(New.subset, !(is.na(host_care_level) | host_care_level == 999 | host_care_level == 99))
  
  New.subset$Highest_care_level <- as.factor(New.subset$host_care_level)
  
  ## Respiratory rate into rts and replacing missing values
  New.subset$ed_rr_value <- ifelse(New.subset$ed_rr_value >= 0 & New.subset$ed_rr_value <= 0, 0,
                                  ifelse(New.subset$ed_rr_value >= 1 & New.subset$ed_rr_value <= 5, 1,
                                         ifelse(New.subset$ed_rr_value >= 6 & New.subset$ed_rr_value <= 9, 2,
                                                ifelse(New.subset$ed_rr_value >= 30 & New.subset$ed_rr_value <= 98, 3,
                                                       ifelse(New.subset$ed_rr_value >= 10 & New.subset$ed_rr_value <= 29, 4,  New.subset$ed_rr_value)))))
  
  New.subset$ed_rr_value <- ifelse(is.na(New.subset$ed_rr_value) | New.subset$ed_rr_value == 999 | New.subset$ed_rr_value == 99, New.subset$ed_rr_rtscat, New.subset$ed_rr_value)  
  
  New.subset <- subset(New.subset, !(is.na(ed_rr_value) | ed_rr_value == 999 | ed_rr_value == 99))
  
  New.subset$Respiratory_rate <- as.factor(New.subset$ed_rr_value)
  
  ## SBP into rts and replacing missing values
  New.subset$ed_sbp_value <- ifelse(New.subset$ed_sbp_value >= 0 & New.subset$ed_sbp_value <= 0, 0,
                                   ifelse(New.subset$ed_sbp_value >= 1 & New.subset$ed_sbp_value <= 49, 1,
                                          ifelse(New.subset$ed_sbp_value >= 50 & New.subset$ed_sbp_value <= 75, 2,
                                                 ifelse(New.subset$ed_sbp_value >= 76 & New.subset$ed_sbp_value <= 89, 3,
                                                        ifelse(New.subset$ed_sbp_value >= 89 & New.subset$ed_sbp_value <= 300, 4,  New.subset$ed_sbp_value)))))
  
  New.subset$ed_sbp_value <- ifelse(is.na(New.subset$ed_sbp_value) | New.subset$ed_sbp_value == 999, New.subset$ed_sbp_rtscat, New.subset$ed_sbp_value) 
  
  New.subset <- subset(New.subset, !(is.na(ed_sbp_value) | ed_sbp_value == 999))
  
  New.subset$Systolic_blood_pressure <- as.factor(New.subset$ed_sbp_value)
  
  ##
   New.subset$pre_intubated <- as.factor(New.subset$pre_intubated)
  

 
    New.subset$dt_ed_first_ct <- as.factor(New.subset$dt_ed_first_ct)
  
  return(factors.data)
}

