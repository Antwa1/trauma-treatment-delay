library(stringr)

## Creating function
create.factors <- function(dataset){
 
  ## Making gcs into rts and replacing missing values

  dataset$ed_gcs_sum <- ifelse(dataset$ed_gcs_sum >= 3 & dataset$ed_gcs_sum <= 3, 0,
                               ifelse(dataset$ed_gcs_sum >= 4 & dataset$ed_gcs_sum <= 5, 1,
                                      ifelse(dataset$ed_gcs_sum >= 6 & dataset$ed_gcs_sum <= 8, 2,
                                             ifelse(dataset$ed_gcs_sum >= 9 & dataset$ed_gcs_sum <= 12, 3,
                                                    ifelse(dataset$ed_gcs_sum >= 13 & dataset$ed_gcs_sum <= 15, 4, dataset$ed_gcs_sum)))))
  
  dataset$pre_gcs_sum <- ifelse(dataset$pre_gcs_sum >= 3 & dataset$pre_gcs_sum <= 3, 0,
                                ifelse(dataset$pre_gcs_sum >= 4 & dataset$pre_gcs_sum <= 5, 1,
                                       ifelse(dataset$pre_gcs_sum >= 6 & dataset$pre_gcs_sum <= 8, 2,
                                              ifelse(dataset$pre_gcs_sum >= 9 & dataset$pre_gcs_sum <= 12, 3,
                                                     ifelse(dataset$pre_gcs_sum >= 13 & dataset$pre_gcs_sum <= 15, 4, dataset$ed_gcs_sum)))))
  
                                  
  dataset$ed_gcs_sum <- ifelse(is.na(dataset$ed_gcs_sum) | dataset$ed_gcs_sum == 999, dataset$pre_gcs_sum, dataset$ed_gcs_sum)  
  
  dataset <- subset(dataset, !(is.na(ed_gcs_sum) | ed_gcs_sum == 999))
  
  dataset$Total_GCS <- as.factor(dataset$ed_gcs_sum)
  
  ## Making gender into a factor
  dataset <- subset(dataset, !is.na(Gender))
  
  
  dataset$Gender <- as.factor(dataset$Gender)
  
  
  ## Survival after 30 days
  dataset <- subset(dataset, !(is.na(res_survival) | res_survival == 999))

  dataset$res_survival <- ifelse(dataset$res_survival == 1, "Dead", "Alive")
   
  dataset$survival_after_30_days <- as.factor(dataset$res_survival)
  

  ## Highest hospital care level
  dataset$host_care_level <- ifelse(dataset$host_care_level == 1, "ER",
                               ifelse(dataset$host_care_level == 2, "General care department",
                                      ifelse(dataset$host_care_level == 3, "OR",
                                             ifelse(dataset$host_care_level == 4, "Specialized care department",
                                                    ifelse(dataset$host_care_level == 5, "ICU", NA)))))
  
  dataset <- subset(dataset, !(is.na(host_care_level) | host_care_level == 999))
  
  dataset$Highest_care_level <- as.factor(dataset$host_care_level)
  
  ## Respiratory rate into rts and replacing missing values
  dataset$ed_rr_value <- ifelse(dataset$ed_rr_value >= 0 & dataset$ed_rr_value <= 0, 0,
                                  ifelse(dataset$ed_rr_value >= 1 & dataset$ed_rr_value <= 5, 1,
                                         ifelse(dataset$ed_rr_value >= 6 & dataset$ed_rr_value <= 9, 2,
                                                ifelse(dataset$ed_rr_value >= 30 & dataset$ed_rr_value <= 98, 3,
                                                       ifelse(dataset$ed_rr_value >= 10 & dataset$ed_rr_value <= 29, 4,  dataset$ed_rr_value)))))
  
  dataset$ed_rr_value <- ifelse(is.na(dataset$ed_rr_value) | dataset$ed_rr_value == 999 | dataset$ed_rr_value == 99, dataset$ed_rr_rtscat, dataset$ed_rr_value)  
  
  dataset <- subset(dataset, !(is.na(ed_rr_value) | ed_rr_value == 999 | ed_rr_value == 99))
  
  dataset$Respiratory_rate <- as.factor(dataset$ed_rr_value)
  
  ## SBP into rts and replacing missing values
  dataset$ed_sbp_value <- ifelse(dataset$ed_sbp_value >= 0 & dataset$ed_sbp_value <= 0, 0,
                                   ifelse(dataset$ed_sbp_value >= 1 & dataset$ed_sbp_value <= 49, 1,
                                          ifelse(dataset$ed_sbp_value >= 50 & dataset$ed_sbp_value <= 75, 2,
                                                 ifelse(dataset$ed_sbp_value >= 76 & dataset$ed_sbp_value <= 89, 3,
                                                        ifelse(dataset$ed_sbp_value >= 89 & dataset$ed_sbp_value <= 500, 4,  dataset$ed_sbp_value)))))
  
  dataset$ed_sbp_value <- ifelse(is.na(dataset$ed_sbp_value) | dataset$ed_sbp_value == 999, dataset$ed_sbp_rtscat, dataset$ed_sbp_value)
  
  dataset <- subset(dataset, !(is.na(ed_sbp_value) | ed_sbp_value == 999))
  
  dataset$Systolic_blood_pressure <- as.factor(dataset$ed_sbp_value)
  
  ##Intubation sorting and missing values
  dataset <- subset(dataset, !(is.na(pre_intubated) | pre_intubated == 999))
  
  dataset$pre_intubated <- ifelse(dataset$pre_intubated == 1, "Yes", "No")
  
  dataset$Intubated_prehospitaly <- as.factor(dataset$pre_intubated)
  
  ##CT scan categorizing
  dataset$dt_ed_first_ct <- ifelse(dataset$dt_ed_first_ct >= 1 & dataset$dt_ed_first_ct <= 30, "1-30 mins",
                                  ifelse(dataset$dt_ed_first_ct >= 31 & dataset$dt_ed_first_ct <= 60, "31-60 mins",
                                         ifelse(dataset$dt_ed_first_ct >= 61 & dataset$dt_ed_first_ct <= 120, "61-120 mins",
                                                ifelse(dataset$dt_ed_first_ct >= 121 & dataset$dt_ed_first_ct <= 9999, "120+ mins", dataset$dt_ed_first_ct))))
  
  dataset <- subset(dataset, !is.na(dt_ed_first_ct))
  
  
  dataset$Time_until_first_CT <- as.factor(dataset$dt_ed_first_ct)
  
  
  ##Fixing years
  dataset <- subset(dataset, !(is.na(pt_age_yrs) | pt_age_yrs == 999))
  
  dataset$Age <- as.numeric(dataset$pt_age_yrs)
  
  ##Fixing ISS
  dataset <- subset(dataset, !(is.na(ISS) | ISS == 999))
  
  dataset$ISS <- as.numeric(dataset$ISS)
  
  ##Dates
  dataset[c('Date', 'Time')] <- str_split_fixed(dataset$DateTime_ArrivalAtHospital, ' ', 2)
  dataset$work_hours <- with(dataset, ifelse(dataset$Time >= "08:00" & dataset$Time <= "16:59", "Yes", "No")) 

  ##Weekdays
  dataset$Date <- as.Date(dataset$Date)
 
  dataset$weekday <- weekdays(dataset$Date)
  
  dataset$weekday <- gsub("Monday|Tuesday|Wednesday|Thursday|Friday", "weekday", dataset$weekday)
  dataset$weekday <- gsub("Saturday|Sunday", "weekend", dataset$weekday)
  
  ##Replacing NA in OFI_dealy with no delay
  dataset$OFI_delay[is.na(dataset$OFI_delay)] <- "No delay to treatment"
  
  dataset$OFI_delay <- as.factor(dataset$OFI_delay)
  
  dataset$ofi <- as.factor(dataset$ofi)
 
  ##removing redundant columns
  dataset <-
    dataset[,!(
      names(dataset) %in% c(
        "res_survival",
        "host_care_level",
        "ed_gcs_sum",
        "ed_rr_value",
        "ed_sbp_value",
        "dt_ed_first_ct",
        "pre_intubated",
        "pt_age_yrs",
        "pre_gcs_sum",
        "ed_rr_rtscat",
        "ed_sbp_rtscat",
        "DateTime_ArrivalAtHospital",
        "Date",
        "Time"
      )
    )]

  
  return(dataset)
}

