library(gtsummary)
library(magrittr)
library(dplyr)
library(purrr)
  

  
## regression model nr 2
model2 <-
  glm(
    OFI_delay ~ Gender + ISS + Age + Number_of_injuries + Systolic_blood_pressure + Respiratory_rate + Total_GCS,
    data = factors.data2,
    family = binomial)

adjusted2 <-
  tbl_regression(model2, exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),) %>%
  
  bold_p() %>%
  bold_labels()


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
    tbls        = list(unadjust2, adjusted2),
    tab_spanner = c("Unadjusted", "Adjusted")
  )
  ## view model




##table2
table2 <-
factors.data2 %>%
  tbl_summary(  
    by = OFI_delay,
    label = list(
      Number_of_injuries = "Number of injuries",
      Age = "Age (years)",
      Systolic_blood_pressure = "Systolic blood pressure",
      Respiratory_rate = "Respiratory rate",
      Total_GCS = "GCS"),
    statistic = list(all_continuous() ~ "{mean} ± {sd}"),
    digits = list(all_continuous() ~ c(2, 2))
  ) %>%
  add_p(list(all_continuous() ~ "t.test",
             all_categorical() ~ "fisher.test")) %>%
add_overall("**Overall (N = {N})**") %>%
  bold_labels()
  
  return(model)


