library(stringr)

## Creating function
create.factors <- function(dataset){
 

  ## Making gender into a factor
  dataset <- subset(dataset, !is.na(Gender))
  
  
  dataset$Gender <- as.factor(dataset$Gender)
  

  ## Highest hospital care level
  dataset$host_care_level <- ifelse(dataset$host_care_level == 1, "ER",
                               ifelse(dataset$host_care_level == 2, "General care department",
                                      ifelse(dataset$host_care_level == 3, "OR",
                                             ifelse(dataset$host_care_level == 4, "Specialized care department",
                                                    ifelse(dataset$host_care_level == 5, "ICU", NA)))))
  
  dataset <- subset(dataset, !(is.na(host_care_level) | host_care_level == 999))
  
  dataset$Highest_care_level <- as.factor(dataset$host_care_level)
  

  ##Intubation sorting and missing values
  dataset <- subset(dataset, !(pre_intubated == 999))
  
  dataset$ed_intubated <- ifelse(dataset$ed_intubated == 1, 3, 2)
  
  dataset <- dataset %>%
    mutate(pre_intubated = if_else(is.na(pre_intubated) | pre_intubated == 2, ed_intubated, pre_intubated))
  
  dataset$Intubated <- as.factor(ifelse(dataset$pre_intubated == 1, "Intubated prehospitaly",
                                        ifelse(dataset$pre_intubated == 2, "Not intubated",
                                               ifelse(dataset$pre_intubated == 3, "Intubated at hosptital",
                                               "unknown"))))
  dataset <- subset(dataset, !(is.na(Intubated)))
                               

  ##Fixing years
  dataset <- subset(dataset, !(is.na(pt_age_yrs) | pt_age_yrs == 999))
  
  dataset$Age <- as.numeric(dataset$pt_age_yrs)
  
  ##Fixing ISS
  dataset <- subset(dataset, !(is.na(ISS) | ISS == 999))
  
  dataset$ISS <- as.numeric(dataset$ISS)
  
  ##Dates
  dataset[c('Date', 'Time')] <- str_split_fixed(dataset$DateTime_ArrivalAtHospital, ' ', 2)
  dataset$work_hours <- with(dataset, ifelse(dataset$Time >= "08:00" & dataset$Time <= "16:59", "Work-hours", "On-call")) 

  ##Weekdays
  dataset$Date <- as.Date(dataset$Date)
 
  dataset$Weekday <- weekdays(dataset$Date)
  
  dataset$Weekday <- gsub("Monday|Tuesday|Wednesday|Thursday|Friday", "weekday", dataset$Weekday)
  dataset$Weekday <- gsub("Saturday|Sunday", "weekend", dataset$Weekday)
  
  dataset <- subset(dataset, !(is.na(Weekday)))
  
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
        "Time",
        "pre_rr_value",
        "pre_sbp_value",
        "ofi",
        "survival_after_30_days",
        "Time_until_first_CT",
        "ed_intubated",
        "ed_emerg_proc"
      )
    )]

  
  return(dataset)
}

