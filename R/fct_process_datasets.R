# Set of functions to: 
# Check, clean, and process the uploaded datasets


#' Process the consumption dataset 
#'
#' @description A fct function
#' @param dta Tibble. The uploaded consumption file
#' @param foodex string. Indicate the foodex categorisation system.One of c('foodex2', 'foodex1')
#' @return Tibble. Processed
#'
#' @noRd
process_consumption <- function(dta, foodex){
  
 foodex <- match.arg(foodex,  c("foodex1", "foodex2"))
  
  #1. Lowercase
  dta <- rename_all(dta, tolower)
  
  if(foodex == "foodex2"){
    check_valid_vars(dta, vars_needed_consumptionFdx2)
    
    # 2. Missing values
    # Important vars for exposure
    cols <- c("serial", "subjectid", "day", "foodexcode","amountfood") #"amountfcooked" will defintely have NAs
    
    dta <- dplyr::filter(dta, across(all_of(cols), ~ !is.na(.x)))
    
    
  } else {
    
    check_valid_vars(dta, vars_valid_consumption_foodex1)
    
    cols <- c("serial", "subjectid", "day", "amountfood", "foodex1")
    
    dta <- dplyr::filter(dta, across(all_of(cols), ~ !is.na(.x)))
  }
  
 
  dta
  
  
  #cat("all good\n")  
  
}


check_valid_vars <- function(dta, vars_valid){
  
  if(!all(vars_valid %in% names(dta))){
    stop("Invalid column names for the dataset", call. = FALSE)    
  } else {
    return(NULL)
  }
  
  
}
