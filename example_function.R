library(rofi)

subset.datas <- prepared.data[, c("ofi", "Problemomrade_.FMP")]

table(prepared.data$Problemomrade_.FMP)


prepared.data$ofi <- ifelse(prepared.data$ofi == "success", 1, 0)
cleaned_data$Gender <- ifelse(cleaned_data$Gender == "M", 1, 0)



  model <- glm(OFI_delay ~ ed_gcs_sum + Gender + res_survival + host_care_level + ed_rr_value + ed_sbp_value + dt_ed_first_ct + pre_intubated + ISS + pt_age_yrs, data = New.subset, family = binomial)
  

