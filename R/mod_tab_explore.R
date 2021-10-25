#' tab_explore UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_tab_explore_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      box(
        col_4(
          radioButtons(ns("aggregation_type"), "Aggregation Type", choices = c("Consumers", "Population"))
        ),
        col_4(
          radioButtons(ns("exposure_type"), "Exposure Type", choices = c("Chronic", "Acute"))
        ),
        col_4(
          radioButtons(ns("per_day_type"), "Per weight?", choices = c("Grams/day" = "gr_day", 
                                                                      "Grams/day/Kg.bw"= "gr_day_kg_bw"))
        ),
        selectInput(ns("group_var"), "Select demographic", 
                    multiple = TRUE,  
                    choices = c("gender", "pop_class", "area"),
                    selected = "gender"
        ),
      )
    ),
    
    fluidRow(
      tabBox(id = ns("tabs_explore"), width = 12,
             tabPanel(title = "Statistics",
                      h4("Aggregated statistics"),
                      #mod_downloadTable_ui(ns("tbl_data_description")),
                      reactableOutput(ns("tbl_by_demo"))
             ),
             tabPanel(title = "Individual",
                      h4("Individual mean consumption"),
                      #p("The table shows what each column in the data represents"),
                      #mod_downloadTable_ui(ns("tbl_data_description")),
                      reactableOutput(ns("individual"))
             )
      )
    )
    
    #DT::DTOutput(ns("tbl_consumption"))
  )
}

#' tab_explore Server Functions
#'
#' @param consumption The consumption file
#' @noRd 
mod_tab_explore_server <- function(id, consumption){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    output$tbl_consumption <- renderDT({
      
      DT::datatable(
        consumption()
      )
      
    })
    
    
    food_items <- reactive({
      
      # map all food selections to the foodname variable
      
      c("Cheese, edam")
    })
    
    
    # Helper datasets ---------------------------------------------------------
    
    
    tbl_n_days      <- reactive({
      
      consumption_sample %>% group_by(subjectid) %>% summarise(n_days = max(day))
      
    }) 
    
    tbl_weight      <- reactive({
      
      consumption_sample %>% 
        distinct(subjectid, .keep_all = TRUE) %>% select(subjectid, weight)
      
    })
    
    
    tbl_population <- reactive({
      
      consumption_sample %>% 
        distinct(subjectid, .keep_all = TRUE) %>% 
        count(across(any_of(input$group_var)), name = "population")
    }) 
    
    
    tbl_consumers <- reactive({
      
      # need to filter it for the food selected
      consumption_sample %>% 
        filter(foodname %in% food_items()) %>% 
        distinct(subjectid, .keep_all = TRUE) %>% 
        count(across(any_of(input$group_var)), name = "consumers")
      
      
    })
    
    
    # Aggregation table -------------------------------------------------------
    
    
    # Per subject grams per day
    
    chronic <- reactive({
      
      consumption_sample %>% 
        {if(input$aggregation_type == "Consumers"){
          filter(., foodname %in% food_items())
        } else {
          mutate(., amountfood = if_else(foodname %in% food_items(), amountfood, 0))
        }} %>% 
        group_by(subjectid, across(any_of(input$group_var))) %>% 
        summarise(total = sum(amountfood, na.rm = TRUE)) %>% 
        ungroup() %>% 
        left_join(tbl_n_days(), by = "subjectid") %>% 
        left_join(tbl_weight(), by = "subjectid") %>% 
        mutate(
          gr_day = total / n_days,
          gr_day_kg_bw = total / n_days / weight
        )
      
    })
    
    
    acute <- reactive({
      
      consumption_sample %>% 
        {if(input$aggregation_type == "Consumers"){
          filter(., foodname %in% food_items())
        } else {
          mutate(., amountfood = if_else(foodname %in% food_items(), amountfood, 0))
        }} %>% 
        # group also by day for the acute
        group_by(across(c(subjectid, any_of(input$group_var), day))) %>% 
        summarise(total = sum(amountfood, na.rm = TRUE)) %>% 
        ungroup() %>% 
        #left_join(tbl_n_days(), by = "subjectid") %>% 
        left_join(tbl_weight(), by = "subjectid") %>% 
        # mutate(
        #   total_kg_bw = total / weight
        # ) %>% 
        mutate(
          gr_day = total,
          gr_day_kg_bw = total / weight
        )
      
    })
    
    output$individual <- renderReactable({
      
      if(input$exposure_type == "Chronic") {
        
        reactable(chronic())
        
      } else {
        
        reactable(acute())
        
      }
      
    })
    
    
    
    tbl_by_demo <- reactive({
      
      
      if(input$exposure_type == "Chronic"){
        
        dta <- chronic ()
        
      } else {
        
        dta <- acute()
      }
      
      aggregation <- 
        dta %>% 
        group_by(across(any_of(input$group_var))) %>% 
        summarise(across(any_of(input$per_day_type), aggregate_summary, .names = "{fn}")) %>% 
        mutate(
          across(-any_of(input$group_var), ~round(., 2))
        ) %>% 
        ungroup()
      
      
      if(!isTruthy(input$group_var)){
        
        bind_cols(tbl_population(), tbl_consumers(), aggregation) 
        
      } else {
        
        purrr::reduce(
          list(
            tbl_population(),
            tbl_consumers(),
            aggregation
          ),
          left_join,
          by = input$group_var
          
        ) 
      }
      
    })
    
    output$tbl_by_demo <- renderReactable({
      
      reactable(tbl_by_demo()
                
      )
    })
    
    
  })
}

## To be copied in the UI
# mod_tab_explore_ui("tab_explore_ui_1")

## To be copied in the server
# mod_tab_explore_server("tab_explore_ui_1")
