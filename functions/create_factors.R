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
  
  New.subset$ed_gcs_sum <- as.factor(New.subset$ed_gcs_sum)
  
  ## Making gender into a factor
  New.subset <- subset(New.subset, !is.na(Gender))
  
  
  New.subset$Gender <- as.factor(New.subset$Gender)
  
  
  
  New.subset$res_survival <- as.factor(New.subset$res_survival)
  
  New.subset$host_care_level <- as.factor(New.subset$host_care_level)
  
  New.subset$ed_rr_value <- as.factor(New.subset$ed_rr_value)
  
  New.subset$ed_sbp_value <- as.factor(New.subset$ed_sbp_value)
  
  New.subset$dt_ed_first_ct <- as.factor(New.subset$dt_ed_first_ct)
  
  New.subset$pre_intubated <- as.factor(New.subset$pre_intubated)
  


  return(factors.data)
}

