library(gtsummary)
library(magrittr)
library(dplyr)
library(car)

model.data <- function(dataset){
  
  ## regression model
  model <-
    glm(
      OFI_delay ~ Total_GCS + Gender + survival_after_30_days + Highest_care_level + Respiratory_rate + Systolic_blood_pressure + Time_until_first_CT + Intubated_prehospitaly + ISS + Age + weekday + work_hours,
      data = factors.data,
      family = binomial
    )
  
  
  ## view model
  model %>%
    tbl_regression(exponentiate = TRUE) %>%
    bold_p() %>%
    bold_labels()
    add_p(digits = c(p = 3))


  factors.data %>%
    tbl_summary(  # Create summary statistics for different groups
      by = OFI_delay,
      label = list(
        Age = "Age (years)",
        Gender = "Gender",
        Total_GCS = "GCS"
      ),
      statistic = list(all_continuous() ~ "{mean} ± {sd}"),
      digits = list(all_continuous() ~ c(2, 2))
    ) %>%
    
library(ggplot2)
  ggplot(factors.data, aes(x = Age)) +
    geom_density(alpha = .2, fill = "#FF6666")
  
  
  return(model)
}
