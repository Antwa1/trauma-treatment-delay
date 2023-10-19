library(rofi)
## Creating function
Create_factors <- function(dataset){
 
  ## Making gcs into rts and replacing missing values
  New.subset$ed_gcs_sum <- ifelse(New.subset$ed_gcs_sum >= 3 & New.subset$ed_gcs_sum <= 3, 0,
                     ifelse(New.subset$ed_gcs_sum >= 4 & New.subset$ed_gcs_sum <= 5, 1,
                            ifelse(New.subset$ed_gcs_sum >= 6 & New.subset$ed_gcs_sum <= 8, 2,
                                   ifelse(New.subset$ed_gcs_sum >= 9 & New.subset$ed_gcs_sum <= 12, 3,
                                          ifelse(New.subset$ed_gcs_sum >= 13 & New.subset$ed_gcs_sum <= 15, 4,
                                                 ifelse(New.subset$ed_gcs_sum == 99, "intubated", New.subset$ed_gcs_sum))))))
  
  New.subset$pre_gcs_sum <- ifelse(New.subset$pre_gcs_sum >= 3 & New.subset$pre_gcs_sum <= 3, 0,
                                  ifelse(New.subset$pre_gcs_sum >= 4 & New.subset$pre_gcs_sum <= 5, 1,
                                         ifelse(New.subset$pre_gcs_sum >= 6 & New.subset$pre_gcs_sum <= 8, 2,
                                                ifelse(New.subset$pre_gcs_sum >= 9 & New.subset$pre_gcs_sum <= 12, 3,
                                                       ifelse(New.subset$pre_gcs_sum >= 13 & New.subset$pre_gcs_sum <= 15, 4,
                                                              ifelse(New.subset$ed_gcs_sum == 99, "intubated", New.subset$pre_gcs_sum))))))
                                  
  New.subset$ed_gcs_sum <- ifelse(is.na(New.subset$ed_gcs_sum) | New.subset$ed_gcs_sum == 999, New.subset$pre_gcs_sum, New.subset$ed_gcs_sum)  
  
  New.subset <- subset(New.subset, !(is.na(ed_gcs_sum) | ed_gcs_sum == 999))
  
  New.subset$Total_GCS <- as.factor(New.subset$ed_gcs_sum)
  
  ## Making gender into a factor
  New.subset <- subset(New.subset, !is.na(Gender))
  
  
  New.subset$Gender <- as.factor(New.subset$Gender)
  
  
  ## Survival after 30 days
  New.subset <- subset(New.subset, !(is.na(res_survival) | res_survival == 999))

  New.subset$res_survival <- ifelse(New.subset$res_survival == 1, "Dead", "Alive")
   
  New.subset$survival_after_30_days <- as.factor(New.subset$res_survival)
  

  ## Highest hospital care level
  New.subset$host_care_level <- ifelse(New.subset$host_care_level == 1, "ER",
                               ifelse(New.subset$host_care_level == 2, "General care department",
                                      ifelse(New.subset$host_care_level == 3, "OR",
                                             ifelse(New.subset$host_care_level == 4, "Specialized care department",
                                                    ifelse(New.subset$host_care_level == 5, "ICU", NA)))))
  
  New.subset <- subset(New.subset, !(is.na(host_care_level) | host_care_level == 999))
  
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
                                                        ifelse(New.subset$ed_sbp_value >= 89 & New.subset$ed_sbp_value <= 500, 4,  New.subset$ed_sbp_value)))))
  
  
  
  New.subset$ed_sbp_value <- ifelse(is.na(New.subset$ed_sbp_value) | New.subset$ed_sbp_value == 999, New.subset$ed_sbp_rtscat, New.subset$ed_sbp_value) 
  
  New.subset <- subset(New.subset, !(is.na(ed_sbp_value) | ed_sbp_value == 999))
  
  New.subset$Systolic_blood_pressure <- as.factor(New.subset$ed_sbp_value)
  
  ##Intubation sorting and missing values
  New.subset <- subset(New.subset, !(is.na(pre_intubated) | pre_intubated == 999))
  
  New.subset$pre_intubated <- ifelse(New.subset$pre_intubated == 1, "Yes", "No")
  
  New.subset$Intubated_prehospitaly <- as.factor(New.subset$pre_intubated)
  
  ##CT scan categorizing
  New.subset$dt_ed_first_ct <- ifelse(New.subset$dt_ed_first_ct >= 1 & New.subset$dt_ed_first_ct <= 30, "1-30 mins",
                                  ifelse(New.subset$dt_ed_first_ct >= 31 & New.subset$dt_ed_first_ct <= 60, "31-60 mins",
                                         ifelse(New.subset$dt_ed_first_ct >= 61 & New.subset$dt_ed_first_ct <= 120, "61-120 mins",
                                                ifelse(New.subset$dt_ed_first_ct >= 121 & New.subset$dt_ed_first_ct <= 9999, "120+ mins", New.subset$dt_ed_first_ct))))
  
  New.subset <- subset(New.subset, !is.na(dt_ed_first_ct))
  
  
  New.subset$Time_until_first_CT <- as.factor(New.subset$dt_ed_first_ct)
  
  
  ##Fixing years
  New.subset <- subset(New.subset, !(is.na(pt_age_yrs) | pt_age_yrs == 999))
  
  New.subset$Age <- as.numeric(New.subset$pt_age_yrs)
  
  ##Fixing ISS
  New.subset <- subset(New.subset, !(is.na(ISS) | ISS == 999))
  
  New.subset$ISS <- as.numeric(New.subset$ISS)
  
  ##Dates
  library(stringr)
  New.subset[c('Date', 'Time')] <- str_split_fixed(New.subset$DateTime_ArrivalAtHospital, ' ', 2)
  New.subset$work_hours <- with(New.subset, ifelse(New.subset$Time >= "08:00" & New.subset$Time <= "16:59", "Yes", "No")) 

  ##Weekdays
  New.subset$Date <- as.Date(New.subset$Date)
 
  New.subset$weekday <- weekdays(New.subset$Date)
  
  New.subset$weekday <- gsub("Monday|Tuesday|Wednesday|Thursday|Friday", "weekday", New.subset$weekday)
  New.subset$weekday <- gsub("Saturday|Sunday", "weekend", New.subset$weekday)
  
  ##Replacing NA in OFI_dealy with no delay
  New.subset$OFI_delay[is.na(New.subset$OFI_delay)] <- "No delay to treatment"
 
  ##removing redundant columns
  New.subset$res_survival <- NULL
  New.subset$host_care_level <- NULL
  New.subset$ed_gcs_sum <- NULL
  New.subset$ed_rr_value <- NULL
  New.subset$ed_sbp_value <- NULL
  New.subset$dt_ed_first_ct <- NULL
  New.subset$pre_intubated <- NULL
  New.subset$pt_age_yrs <- NULL
  New.subset$pre_gcs_sum <- NULL
  New.subset$ed_rr_rtscat <- NULL
  New.subset$DateTime_ArrivalAtHospital <- NULL
  New.subset$Date <- NULL
  New.subset$Time <- NULL
  
  return(factors.data)
}

