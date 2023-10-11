library(rofi)

subset.datas <- prepared.data[, c("ofi", "Problemomrade_.FMP")]

table(prepared.data$Problemomrade_.FMP)


prepared.data$ofi <- ifelse(prepared.data$ofi == "success", 1, 0)
cleaned_data$Gender <- ifelse(cleaned_data$Gender == "M", 1, 0)



  model <- glm(OFI_delay ~ pt_age_yrs + ISS + inj_mechanism + pt_Gender, data = cleaned_data, family = binomial)
  
  filtered.data <- subset(New.subset, "pt_age_yrs" + "ISS" + "inj_mechanism" + "pt_Gender", data != "999")
  
  prepared.data$OFI_delay <- as.factor(ifelse(prepared.data$Problemomrade_.FMP == "Handläggning", "1", "0"))
  