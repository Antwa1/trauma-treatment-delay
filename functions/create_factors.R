library(rofi)
## Creating function
Create_factors <- function(dataset){
 
  ## Making my variables into factors
  New.subset$ed_gcs_sum <- as.factor(New.subset$ed_gcs_sum)
  New.subset$Gender <- as.factor(New.subset$Gender)
  New.subset$res_survival <- as.factor(New.subset$res_survival)
  New.subset$host_care_level <- as.factor(New.subset$host_care_level)
  New.subset$ed_rr_value <- as.factor(New.subset$ed_rr_value)
  New.subset$ed_sbp_value <- as.factor(New.subset$ed_sbp_value)
  New.subset$dt_ed_first_ct <- as.factor(New.subset$dt_ed_first_ct)
  New.subset$pre_intubated <- as.factor(New.subset$pre_intubated)


  return(newsubset.data)
}

