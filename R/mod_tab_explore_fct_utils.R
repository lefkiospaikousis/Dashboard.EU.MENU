#' Functions for the explore TAB
#' 

#' Makes the filter UI for a variables
#' 
#' @param var Vector. The variable
#' @param id String. Length 1. The id of the filter
#' @param label String length 1. The label of the filter
#' 
#' @details Taken from https://mastering-shiny.org/action-dynamic.html#dynamic-filter
#' @noRd
make_filter <- function(var, id, label = id, session) {
  
  ns <- session$ns
  
  if (is.numeric(var)) {
    
    rng <- range(var, na.rm = TRUE)
    sliderInput(ns(id), label, min = rng[1], max = rng[2], value = rng)
    
  } else if (is.factor(var)) {
    
    levs <- levels(var)
    selectInput(ns(id), label, choices = levs, selected = "", multiple = TRUE)
    
  } else if (is.character(var)) {
    
    levs <- unique(var)
    selectInput(ns(id), label, choices = levs, selected = "", multiple = TRUE)
    
  } else {
    # Not supported
    NULL
  }
}

#' Makes a logical vector for filtering
#' 
#' @param var Vector. The variable
#' @param value The selected value. in case of bumeric variable, then this is a range
#' 
#' @details Taken from https://mastering-shiny.org/action-dynamic.html#dynamic-filter
#' @noRd
#' 
filter_var <- function(var, value) {
  
  if (is.numeric(var)) {
    
    !is.na(var) & var >= value[1] & var <= value[2]
    
  } else if (is.factor(var)) {
    
    var %in% value
    
  } else if (is.character(var)) {
    
    var %in% value
    
  } else {
    
    # No control, so don't filter
    TRUE
  }
}




