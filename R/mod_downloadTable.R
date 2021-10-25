#' downloadTable UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param buttonLabel String. Text for the Button.
#' 
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_downloadTable_ui <- function(id, buttonLabel  = "Download .xlsx"){
  ns <- NS(id)
  tagList(
    downloadLink(ns("downloadTable"), label = buttonLabel, class = "downExcel"),
    tags$head(tags$style(".downExcel{background-color:transparent;} .downExcel{color: #337ab7;}  .downExcel{border:0px;}
   .downExcel{outline:0px;} .downExcel{font-size:10px;"))
  )
}
    
#' downloadTable Server Function
#' @param table_name String. A name to be used in the filename when downloading
#' @param the_table A reactive element that leads to a datframe. Put it without the ()
#' @details This function saves only tables And specifically reactive tables. Havent tested it for 
#' reactivevalues. Note the () I use in the server part. Not sure why I need this.
#' @noRd 
mod_downloadTable_server <- function(input, output, session, table_name, the_table){
  ns <- session$ns
 
  # this will create the download handler
  # Remember that the handler's output$_name_ has to be the same as the `downloadLink`'s id
  # in this case its download_plot
  output$downloadTable <- downloadHandler(
    
    filename = function() {
      paste(table_name,".xlsx", sep="")
    },
    
    # See here that i wrapped the `the_plot` with `()`
    content = function(file) {
      writexl::write_xlsx(path = file, x = the_table())
    }
  )
  
}
