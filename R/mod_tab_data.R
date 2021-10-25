#' data UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' 
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_tab_data_ui <- function(id){
  ns <- NS(id)
  tagList(
    mod_import_consumption_ui(ns("import_consumption_ui_1")),
    tabBox(id = ns("data_tabset"), width = 12,
           
           tabPanel(title = "FULL DATASET", 
                    h3("Full consumption table"), 
                    reactableOutput(ns("consumption"))
           ),
           tabPanel(title = "Participants", 
                    h3("Participants in the food survey"), 
                    selectInput(ns("row_var"), "Row var", choices = c("gender", "area", "pop_class")),
                    selectInput(ns("col_var"), "Row var", choices =  c("gender", "area", "pop_class")),
                    DT::dataTableOutput(ns("participants")),
                    div(id = ns("down_participants"))
           ),
           
           # tabPanel(title = "FoodEx1",
           #          h3("The FoodEx1 food classification system"),
           #          DT::dataTableOutput(ns("foodex1"))
           # ),
           
           tabPanel(title = "Survey Samples",
                    h3("The FoodSurvey sample sizes"),
                    p("The table shows the sample size [% (N)] of participants"),
                    tableOutput(ns("freq_gender_age")),
                    div(id=ns("freq1")),
                    p(" "),
                    tableOutput(ns("freq_district_area")),
                    div(id=ns("freq2"))
                    
           ),
           tabPanel(title = "Data Description",
                    h3("A description of the columns in the dataset"),
                    #p("The table shows what each column in the data represents"),
                    mod_downloadTable_ui(ns("tbl_data_description")),
                    DT::dataTableOutput(ns("tbl_data_description"))
           )
    )
  )
}

#' data Server Functions
#'
#' @noRd 
mod_tab_data_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    rv <- rv(
      dta = NULL,
      trigger = 0
    )
    
    
    # Data --------------------------------------------------------------------
    
    dta <- mod_import_consumption_server("import_consumption_ui_1")
    
    observeEvent(dta$trigger, {
      
      rv$dta <- dta$new_consumption
      rv$trigger <- rv$trigger + 1
    })
    
    
    output$consumption <- renderReactable({
      
      validate(need(rv$dta, message = "No dataset is uploaded"))
      
      reactable(rv$dta)
      
      
    })
    
    # helper datasets
    
    tbl_n_days      <- reactive(rv$dta %>% group_by(subjectid) %>% summarise(n_days = max(day))) 
    tbl_weight      <- reactive(rv$dta %>% distinct(subjectid, weight))
    tbl_n_pop_class <- reactive(rv$dta %>% distinct(subjectid, .keep_all = TRUE) %>% count(pop_class)) 
    
    
    # Participants ------------------------------------------------------------
    
    participants   <- reactive({
      
      validate(need(rv$dta, message = "No dataset is uploaded"))
      
      rv$dta %>% 
        distinct(subjectid, .keep_all = TRUE) %>%
        select(subjectid, gender, pop_class, age, weight, area, wcoeff)
    })
    
    output$participants <- DT::renderDataTable({
      
      participants() %>% 
        DT::datatable(
          filter = "top"
        ) %>% 
        DT::formatRound ( c("weight", "age"), 1) 
      #DT::formatRound ( c("gr_day", "gr_kbw_day"), 2) 
      
    })
    
    
    # Survey Samples ----------------------------------------------------------
    
    
    freq_gender_age <- reactive({
      
      validate(need(rv$dta, message = "No dataset is uploaded"))
      
      rv$dta %>% 
        distinct(subjectid, pop_class, gender) %>%
        janitor::tabyl(gender, pop_class, show_missing_levels = TRUE) %>% 
        janitor::adorn_totals(c("row", "col")) %>% 
        janitor::adorn_percentages() %>% 
        janitor::adorn_pct_formatting() %>% 
        janitor::adorn_ns() %>% 
        janitor::untabyl() 
    })
    
    output$freq_gender_age <- renderTable({freq_gender_age()})
    
    
    freq_district_area <- reactive({
      
      validate(need(rv$dta, message = "No dataset is uploaded"))
      
      rv$dta %>% 
        distinct(subjectid, area, pop_class) %>%
        janitor::tabyl(area, pop_class, show_missing_levels = TRUE) %>% 
        janitor::adorn_totals(c("row", "col")) %>% 
        janitor::adorn_percentages() %>% 
        janitor::adorn_pct_formatting() %>% 
        janitor::adorn_ns() %>% 
        janitor::untabyl() 
    })    
    
    output$freq_district_area <- renderTable({freq_district_area()})
    
    
    
    
    
    
    
    
    
    
    # Return ------------------------------------------------------------------
    
    
    return(rv)
  })
  
}

## To be copied in the UI
# mod_tab_data_ui("tab_data_ui_1")

## To be copied in the server
# mod_tab_data_server("tab_data_ui_1")
