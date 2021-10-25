# Global functions

#'Turn a vector of numerics into percentages
#'@param ...  Arguments passed to scales::percent()
#'@param x A numeric vector
#'@noRd
#'@return A vector of percentages same length as x
#'@export
percent <- function(x, ...){
  scales::label_percent(...)(x)
  
}


#' Check if the need columns are in the dataset
#' @noRd
check_varsNeeded  <- function(data, vars_needed){
  
  names_dt    <- tolower(names(data))
  
  if(all(vars_needed %in% names_dt)){
    
    return(NULL)
    
  } else {
    
    setdiff(vars_needed, names_dt)
  }
  
}

#' Get the icon  as a character vector to pass to the DT tables
#' @noRd
get_icon <- function(x) as.character(shiny::icon(x))


#' Excel vlookup 
#' This comes from the `Goddess`
#' See https://twitter.com/JennyBryan/status/980978609794895872
#' https://www.r-bloggers.com/2018/04/an-r-vlookup-not-so-silly-idea/
#' @export
#' @noRd
vlookup <- function(this, df, key, value) {
  m <- match(this, df[[key]])
  df[[value]][m]
}

# spinner wrapper 
with_spinner <- function(obj, size = 0.5, ...) {
  
  shinycssloaders::withSpinner(ui_element = obj, size = size, ...)
  
}

