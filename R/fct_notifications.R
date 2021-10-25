#  This holds the notifications and error messages throughout the app


#' Show success message
#' @noRd
toast_success <- function(.message, .duration = 400, ...){
  
  shinyFeedback::showToast(
    
    type = "success",
    message = .message,
    ...,
    .options = list(
      
      showDuration = .duration,
      positionClass = "toast-top-center"
    )
    
  )
}

toast_error <- function(.message, .duration = 100, ...){
  
  shinyFeedback::showToast(
    
    type = "error",
    message = .message,
    ...,
    .options = list(
      preventDuplicates = TRUE,
      showDuration = .duration,
      positionClass = "toast-top-center"
    )
    
  )
}

toast_info <- function(.message, .duration = 400, ...){
  
  shinyFeedback::showToast(
    
    type = "info", message = .message, ...,
    .options = list(
      showDuration = .duration,
      positionClass = "toast-top-left"
    )
    
  )
}



#' Check if  there is an error in the ggplot creation and shows a message via validate()
#' @noRd
catch_plotError <- function(plot){
  
  x <- 
    tryCatch(
      print(plot),
      error  = function(e) e
    ) 
  
  not_error <- !(inherits(x, "error"))
  
  # a message to the console
  if(!not_error) print(x)
  
  errorMessage_plot <-
    paste0("Sorry, something went wrong in creating this plot!
      \nCheck your data and if the problem persists contact info@improvast.com
  \n and supply the following error message ", x)
  
  # A message to the user
  validate(
    need(not_error, message = errorMessage_plot)
  )
  
  
}

# Unused ------------------------------------------------------------------

#'Show success alert for uploading and checking the data
#'@noRd
show_success_alert <- function(message){

  shinyFeedback::showToast(
    type =  "success",
    title = "Successful Upload and Checks",
    message = message,
    #keepVisible = FALSE,
    .options = list(
      positionClass = "toast-top-center",
      timeOut  = 6000
    )
  )

}


#'Show Error toastr for not uploading .xlsx files
#'@noRd
error_notExcel <- function(){
  
  shinyFeedback::showToast(
    "error",
    title = "Please upload an .xlsx file",
    message = "Currently we only accept .xlsx file format",
    #keepVisible = FALSE,
    .options = list(
      positionClass = "toast-top-center",
      timeOut  = 6000
    )
  )
  
}


# TODO The below three functions (checks) are all the same. Create a single function
# What were you thinking malaka?

check_varsConsumptionFdx2 <- function(data, vars_needed){
  
  #vars_needed <- vars_needed_consumptionFdx2
  #names_dt    <- tolower(names(data))
  names_dt    <- names(data)
  
  dt_name     <- deparse(substitute(data))
  
  if(all(vars_needed %in% names_dt)){
    
    NULL
    
  } else {
    
    missing_vars <- setdiff(vars_needed, names_dt)
    
    error_consumption("Missing column names in your dataset")
    
    validate(
      
      glue::glue(
        "Missing columns in {dt_name}: \n{paste(missing_vars,collapse=', ')}
        \n Have you uploaded the correct data?"
      )
      , errorClass = "col_names"
    )
    
  }
  
}

check_varsRawOccurrence  <- function(data, vars_needed){
  
  names_dt    <- names(data) #tolower(names(data))
  dt_name     <- deparse(substitute(data))
  
  if(all(vars_needed %in% names_dt)){
    
    NULL
    
  } else {
    
    missing_vars <- setdiff(vars_needed, names_dt)
    
    error_occurrence("Missing column names in your dataset")
    
    validate(
      
      glue::glue(
        "Missing columns in {dt_name}: \n{paste(missing_vars,collapse=', ')}
        \nHave you uploaded the correct file?"
      )
      , errorClass = "col_names"
    )
    
  }
  
}

check_varsOccurrenceFdx2 <- function(data, vars_needed){
  
  #vars_needed <- vars_needed_occurrenceFdx2
  names_dt    <- names(data) #tolower(names(data))
  dt_name     <- deparse(substitute(data))
  
  if(all(vars_needed %in% names_dt)){
    
    NULL
    
  } else {
    
    missing_vars <- setdiff(vars_needed, names_dt)
    
    error_occurrence("Missing column names in your dataset")
    
    validate(
      
      glue::glue(
        "Missing columns in {dt_name}: \n{paste(missing_vars,collapse=', ')}
        \nHave you uploaded the correct file?"
      )
      , errorClass = "col_names"
    )
    
  }
  
}




#'Show Error toastr for errors in the consumptionfile
#'@noRd
error_consumption <- function(message){
  
  shinyFeedback::showToast(
    "error",
    title = "Problem with your consumption file",
    message = message,
    #keepVisible = FALSE,
    .options = list(
      positionClass = "toast-top-center",
      timeOut  = 6000
    )
  )
  
}

#'Show Error toastr for errors in the consumptionfile
#'@noRd
error_occurrence <- function(message){
  
  shinyFeedback::showToast(
    "error",
    title = "Problem with your occurrence file",
    message = message,
    #keepVisible = FALSE,
    .options = list(
      positionClass = "toast-top-center",
      timeOut  = 6000
    )
  )
  
}


#' Check the Level 4 foodex codes (not the descr) in the Consunption data
#' @param data The uploaded consumption data
check_fdx1_coding <- function(data){
  
  cons_l4_codes <- data[["foodex_l4_code"]]
  
  valid <- sum(cons_l4_codes %in% fdx1_l4_code)/ length(cons_l4_codes)
  
  if(valid < 0.95){
    
    error_consumption("Wrong Foodex codes in the data")
    
    validate("More than 5% of your data do not have the correct FoodEx Level 4 coding
             \n Please check the column 'foodex_l4_code' in your consumption data that holds the foodex code")
  } else {
    
    NULL
  }
  
}


#' Check the Level 2, 3 or 4 foodex DESCR (not the codes) in Occurrence
#' @param level String. One of 'level2',  'level3'  or 'level4'
#' @param data The occurence data.Either level 2 or level3
#' @noRd
check_fdx1_descr <- function(data, level){
  
  dt_name <- deparse(substitute(data))
  
  if(level == "level2"){
    
    occur_l2_desc <- data[["foodex_l2_desc"]]
    
    valid <- sum(occur_l2_desc %in% fdx1_l2_desc)/ length(occur_l2_desc)
    
  } 
  
  if(level == "level3"){
    
    occur_l3_desc <- data[["foodex_l3_desc"]]
    
    valid <- sum(occur_l3_desc %in% fdx1_l3_desc)/ length(occur_l3_desc)
    
  } 
  
  if(level == "level4"){
    
    occur_l4_desc <- unique(data[["foodex_l4_desc"]])
    
    valid <- sum(occur_l4_desc %in% fdx1_l4_desc)/ length(occur_l4_desc)
    
  } 
  
  if(valid < 0.95){
    error_occurrence(glue::glue("Wrong Foodex descriptions in sheet {dt_name}"))
    
    validate(glue::glue("More than 5% of your data in sheet {dt_name} do not have the correct FoodEx {level} description
    \n Please check the column that holds {level} in your occurrence data in sheet {dt_name}")
    )
  } else {
    
    NULL
  }
  
}


#
check_fewRows <- function(data){
  
  if(nrow(data)<50){
    
    error_consumption("Very few rows in the dataset")
    
    validate("There are very few rows in the dataset (<50)
             \n Perhaps you have uploaded the wrong dataset?")
  } else {
    
    NULL
  }
  
}
