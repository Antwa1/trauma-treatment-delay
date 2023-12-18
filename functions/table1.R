table1 <- function(dataset){

  factors.data2 %>%
    tbl_summary(
      by = OFI_delay,
      label = list(
        Age = "Age (years)",
        Gender = "Gender",
        Total_GCS = "GCS",
        ISS = "ISS",
        Number_of_injuries = "Number of injuries",
        Systolic_blood_pressure = "Systolic blood pressure",
        Respiratory_rate = "Respiratory rate"
      ),
      statistic = list(
        Age ~ "{mean} ± {sd}",
        Gender ~ "{mean} ± {sd}",
        Total_GCS ~ "{median} [IQR]",
        ISS ~ "{mean} ± {sd}",
        Number_of_injuries ~ "{mean} ± {sd}",
        Systolic_blood_pressure ~ "{mean} ± {sd}",
        Respiratory_rate ~ "{mean} ± {sd}"
      ),
      digits = list(all_continuous() ~ c(2, 2))
    ) %>%
    add_p()
  
  
  return(table1)
}

