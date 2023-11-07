library(gtsummary)
library(magrittr)
library(dplyr)

model.data <- function(dataset){
  
  ## regression model
  model <-
    glm(
      OFI_delay ~ Total_GCS + Gender + Highest_care_level + Respiratory_rate + Systolic_blood_pressure + Time_until_first_CT + Intubated_prehospitaly + ISS + Age + weekday + work_hours,
      data = factors.data,
      family = binomial
    )

  model1 <-
    glm(
      OFI_delay ~Gender + ISS + Age + work_hours + Highest_care_level,
      data = factors.data,
      family = binomial
    )
  
  model2 <-
    glm(
      OFI_delay ~ Intubated_prehospitaly,
      data = factors.data,
      family = binomial
    )
  
  ## view model
  model2 %>%
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
    

  
  return(model)
}
