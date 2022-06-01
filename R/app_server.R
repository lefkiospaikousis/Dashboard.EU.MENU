#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @import dplyr
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom purrr keep map imap map2 reduce walk iwalk walk2
#' @importFrom rlang .data 
#' @importFrom stats na.omit
#' @import DT
#' @import shinydashboard
#' @importFrom reactable renderReactable reactable reactableOutput colDef colFormat colGroup
#' @importFrom waiter waiter_show waiter_hide
#' @noRd
app_server <- function( input, output, session ) {
  # Your application server logic 
  
  #dta <- readxl::read_xlsx("")
  rv <- rv(
    dta = consumption_sample
  )
  
  dta <- mod_tab_data_server("tab_data_ui_1", reactive(rv$dta))
  
  observeEvent(dta$trigger, {
    
    rv$dta <- dta$dta
    
  })
  
  # observe({
  #   shinyjs::toggleState("explore", isTruthy(rv$dta))
  # })
  
  mod_tab_explore_server("tab_explore_ui_1", reactive(rv$dta))
  
  
  
}
