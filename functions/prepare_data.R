library(rofi)

prepare.data <- function(){
  
  ## Import data
  data <- rofi::import_data(
    user = "antonw",
    password = "2-parts-Attack-applied")
    
  ## Create variable combined datasets by merging
  combined.datasets <- rofi::merge_data(data)

    ## Adding ofi as variable
  combined.datasets$ofi <- rofi::create_ofi(combined.datasets)
  
  ## output
  return (combined.datasets)
}

