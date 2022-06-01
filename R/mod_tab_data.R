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
           
           tabPanel(title = "Data", 
                    h3("Full consumption table"), 
                    mod_downloadTable_ui(ns("dataset")),
                    reactableOutput(ns("consumption")) %>% with_spinner()
           ),
           tabPanel(title = "Participants", 
                    h3("Participants in the food survey"), 
                    mod_downloadTable_ui(ns("participants")),
                    DT::dataTableOutput(ns("participants")) %>% with_spinner(),
           ),
           tabPanel(title = "Survey Samples",
                    h3("The FoodSurvey sample sizes"),
                    p("The tables show the sample size [% (N)] of participants"),
                    hr(),
                    h4("Gender by Population class"),
                    mod_downloadTable_ui(ns("freq_gender_age")),
                    tableOutput(ns("freq_gender_age")),
                    div(id=ns("freq1")),
                    p(" "),
                    hr(),
                    h4("Area by Population class"),
                    mod_downloadTable_ui(ns("freq_popClass_area")),
                    tableOutput(ns("freq_popClass_area")),
                    div(id=ns("freq2"))
                    
           )
           # tabPanel(title = "Data Description",
           #          h3("A description of the columns in the dataset"),
           #          #p("The table shows what each column in the data represents"),
           #          mod_downloadTable_ui(ns("tbl_data_description")),
           #          DT::dataTableOutput(ns("tbl_data_description"))
           # )
    )
  )
}

#' data Server Functions
#'
#' @noRd 
mod_tab_data_server <- function(id, dta){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    rv <- rv(
      dta = NULL,
      trigger = 0
    )
    
    # initialise
    observe({ rv$dta <- dta() })
    
    callModule(mod_downloadTable_server, "dataset",
               table_name = "dataset",
               the_table = reactive(rv$dta))
    
    callModule(mod_downloadTable_server, "participants",
               table_name = "Participants",
               the_table = participants)
    
    callModule(mod_downloadTable_server, "freq_popClass_area",
               table_name = "AreaByPopClass",
               the_table = freq_popClass_area)
    
    callModule(mod_downloadTable_server, "freq_gender_age",
               table_name = "GenderByAge",
               the_table = freq_gender_age)
    
    
    # Data --------------------------------------------------------------------
    
    dta <- mod_import_consumption_server("import_consumption_ui_1")
    
    observeEvent(dta$trigger, {
      
      rv$dta <- dta$new_consumption
      rv$trigger <- rv$trigger + 1
    }, ignoreInit = TRUE)
    
    
    output$consumption <- renderReactable({
      
      validate(need(rv$dta, message = "No dataset is uploaded"))
      
      rv$dta %>% 
        rename(!!!unlist(keep(labels_list, ~ .x %in% names(rv$dta)))) %>% 
        reactable(
          searchable = TRUE
        )
      
      
    })
    
    # helper datasets
    
    tbl_n_days      <- reactive(rv$dta %>% group_by(subjectid) %>% summarise(n_days = max(day))) 
    tbl_weight      <- reactive(rv$dta %>% distinct(subjectid, weight))
    tbl_n_pop_class <- reactive(rv$dta %>% distinct(subjectid, .keep_all = TRUE) %>% count(pop_class)) 
    
    
    # Participants ------------------------------------------------------------
    
    participants   <- reactive({
      
      validate(need(rv$dta, message = "No dataset is uploaded"))
      
      tbl <- 
        rv$dta %>% 
        distinct(subjectid, .keep_all = TRUE) %>%
        select(subjectid, gender, pop_class, age, weight, area, wcoeff) 
      
      tbl %>% 
        rename(!!!unlist(keep(labels_list, ~ .x %in% names(tbl)))) %>% 
        mutate(across(where(is.character), as.factor))
    })
    
    output$participants <- DT::renderDataTable({
      
      participants() %>% 
        DT::datatable(
          filter = "top"
        ) %>% 
        DT::formatRound ( c("Weight", "Age"), 1) 
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
        janitor::untabyl() %>% 
        rename(Gender = gender)
    })
    
    output$freq_gender_age <- renderTable({freq_gender_age()})
    
    freq_popClass_area <- reactive({
      
      validate(need(rv$dta, message = "No dataset is uploaded"))
      
      rv$dta %>% 
        distinct(subjectid, area, pop_class) %>%
        janitor::tabyl(area, pop_class, show_missing_levels = TRUE) %>% 
        janitor::adorn_totals(c("row", "col")) %>% 
        janitor::adorn_percentages() %>% 
        janitor::adorn_pct_formatting() %>% 
        janitor::adorn_ns() %>% 
        janitor::untabyl()  %>% 
        rename(Area = area)
    })    
    
    output$freq_popClass_area <- renderTable({freq_popClass_area()})
    
    
    # Return ------------------------------------------------------------------
    
    
    return(rv)
  })
  
}

## To be copied in the UI
# mod_tab_data_ui("tab_data_ui_1")

## To be copied in the server
# mod_tab_data_server("tab_data_ui_1")
