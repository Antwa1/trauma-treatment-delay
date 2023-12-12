library(gtsummary)
library(magrittr)
library(dplyr)
library(purrr)

result.1 <- function(factors.data2){

## regression model nr 2
model1 <-
  glm(
    OFI_delay ~ Gender + ISS + Age + Number_of_injuries + Systolic_blood_pressure + Respiratory_rate + Total_GCS,
    data = factors.data2,
    family = binomial)

adjusted <-
  tbl_regression(model1, exponentiate = TRUE,
                 pvalue_fun = ~ style_pvalue(.x, digits = 2),) %>%
  
  bold_p() %>%
  bold_labels()


unadjust <- tbl_uvregression(data = factors.data2,
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
    tbls        = list(unadjust, adjusted),
    tab_spanner = c("Unadjusted", "Adjusted")
  )
return(fancy_table)

}