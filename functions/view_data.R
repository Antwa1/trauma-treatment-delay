library(gtsummary)
library(magrittr)
library(dplyr)

model.data <- function(dataset){
  
  ## regression model nr 1
  model1 <-
    glm(
      OFI_delay ~ Gender + Highest_care_level + Intubated + ISS + Age + weekday + work_hours,
      data = factors.data,
      family = binomial)
      
      adjusted1 <-
        tbl_regression(model1, exponentiate = TRUE,
                       pvalue_fun = ~ style_pvalue(.x, digits = 2),) %>%
        bold_p() %>%
        bold_labels()
      add_p(digits = c(p = 3))
}
  
  unadjust1 <- tbl_uvregression(data = factors.data1,
                                exponentiate = TRUE,
                                method = glm,
                                y = OFI_delay,
                                method.args = list(family = binomial),
                                pvalue_fun = ~ style_pvalue(.x, digits = 2),
                                hide_n = TRUE
  ) %>%
    bold_p(t = 0.05) %>%
    bold_labels()
    

fancy_table1 <-
    tbl_merge(
      tbls        = list(adjusted1, unadjust1),
      tab_spanner = c("Adjusted", "Unadjusted")
    )
  
  
  
  
## regression model nr 2
model2 <-
  glm(
    OFI_delay ~ Gender + ISS + Age + Emergency_procedure + Systolic_blood_pressure + Respiratory_rate + Total_GCS,
    data = factors.data2,
    family = binomial)

adjusted2 <-
  tbl_regression(model2, exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),) %>%
  bold_p() %>%
  bold_labels()
add_p(digits = c(p = 3))
}

unadjust2 <- tbl_uvregression(data = factors.data2,
                              exponentiate = TRUE,
                              method = glm,
                              y = OFI_delay,
                              method.args = list(family = binomial),
                              pvalue_fun = ~ style_pvalue(.x, digits = 2),
                              hide_n = TRUE
) %>%
  bold_p(t = 0.05) %>%
  bold_labels()


fancy_table2 <-
  tbl_merge(
    tbls        = list(adjusted2, unadjust2),
    tab_spanner = c("Adjusted", "Unadjusted")
  )
  ## view model

    

##table1
factors.data %>%
  tbl_summary(  
    by = OFI_delay,
    label = list(
      Age = "Age (years)",
      Gender = "Gender",
      work_hours = "Work hours"),
    statistic = list(all_continuous() ~ "{mean} ± {sd}"),
    digits = list(all_continuous() ~ c(2, 2))
  ) %>%
  add_overall("**Overall (N = {N})**") %>%
  bold_labels()


##table2
factors.data2 %>%
  tbl_summary(  
    by = OFI_delay,
    label = list(
      Age = "Age (years)",
      Systolic_blood_pressure = "Systolic blood pressure",
      Emergency_procedure = "Emergency procedure",
      Respiratory_rate = "Respiratory rate",
      Total_GCS = "GCS",
      work_hours = "Work hours"),
    statistic = list(all_continuous() ~ "{mean} ± {sd}"),
    digits = list(all_continuous() ~ c(2, 2))
  ) %>%
  add_overall("**Overall (N = {N})**") %>%
  bold_labels()
  
  return(model)

