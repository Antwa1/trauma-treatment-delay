library(gtsummary)
library(magrittr)
library(dplyr)

model.data <- function(dataset){
  
  ## regression model
  model <-
    glm(
      OFI_delay ~ Total_GCS + Gender + Highest_care_level + Respiratory_rate + Systolic_blood_pressure + Intubated + ISS + Age + weekday + work_hours,
      data = factors.data,
      family = binomial)
      
      adjusted <-
        tbl_regression(model, exponentiate = TRUE,
                       pvalue_fun = ~ style_pvalue(.x, digits = 2),) %>%
        bold_p() %>%
        bold_labels()
      add_p(digits = c(p = 3))
}
  
  unadjust2 <- tbl_uvregression(data = factors.data,
                                exponentiate = TRUE,
                                method = glm,
                                y = OFI_delay,
                                method.args = list(family = binomial),
                                pvalue_fun = ~ style_pvalue(.x, digits = 2),
                                hide_n = TRUE
  ) %>%
    bold_p(t = 0.05) %>%
    bold_labels()
    

fancy_table <-
    tbl_merge(
      tbls        = list(adjusted, unadjust2),
      tab_spanner = c("Adjusted", "Unadjusted")
    )
  
  
  
  
  
  ## view model

    


  factors.data %>%
    tbl_summary(  # Create summary statistics for different groups
      by = OFI_delay,
      label = list(
        Age = "Age (years)",
        Gender = "Gender",
        Total_GCS = "GCS"),
      
      statistic = list(all_continuous() ~ "{mean} ± {sd}"),
      digits = list(all_continuous() ~ c(2, 2))
    ) %>%
    

  
  return(model)

