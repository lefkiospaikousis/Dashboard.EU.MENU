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
      box(width = 5,
          col_4(
            radioButtons(ns("aggregation_type"), "Aggregation Type", 
                         choices = c("Consumers", "Population"))
          ),
          col_4(
            radioButtons(ns("exposure_type"), "Exposure Type", 
                         choices = c("Chronic", "Acute"))
          ),
          col_4(
            radioButtons(ns("per_day_type"), "Per weight?", 
                         choices = c("grams/day" = "gr_day", 
                                     "grams/day/Kg bw"= "gr_day_kg_bw"))
          ),
          selectInput(ns("group_var"), "Select demographic", 
                      multiple = TRUE,  
                      choices = c("Gender" = "gender", 
                                  "Population Class" = "pop_class", 
                                  "Area" = "area"
                                  
                      ),
                      selected = ""
          ),
      ),
      box(width = 7,
          # col_4(
          #   selectInput(ns("foodname"), "FoodEx Name", choices = NULL, multiple = TRUE)
          # ),
          uiOutput(ns("filter_ui"))
      )
    ),
    
    fluidRow(
      tabBox(id = ns("tabs_explore"), width = 12,
             tabPanel(title = "Statistics",
                      #h4("Aggregated statistics"),
                      uiOutput(ns("title_table")),
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
    
    
    labels_list <- list(
      
      "Gender" = "gender", 
      "Population Class" = "pop_class", 
      "Area" = "area",
      
      
      "grams/day" = "gr_day", 
      "grams/day/Kg bw"= "gr_day_kg_bw",
      
      "FoodEx Name" = "foodname", 
      "FoodEx1 Name" = "foodex1_name", 
      "FoodEx1" = "foodex1",
      
      "Participants" = "population",
      "Consumers" = "consumers",
      "% of consumers" = "pct_cons",
      "Mean" = "mean",
      "Median" = "median",
      "95th percentile" = "p95",
      "Max"  = "max",
      "Min" = "min"
    )
    
    
    
    # Name is the Label of the filter. Value must be the actual variable name
    filter_vars <- keep(labels_list, ~ .x %in% c("foodname", "foodex1_name", "foodex1"))
    # "FoodEx1 Name" = 
    # "FoodEx1" = "foodex1"
    
    
    output$filter_ui <- renderUI({
      
      purrr::imap(filter_vars, ~ col_4(
        make_filter(
          var = consumption_sample[[.x]], id = .x, label = .y,  session = session
        )
      )
      )
      
    })
    
    
    observe({
      
      purrr::map(filter_vars, ~ updateSelectInput(inputId = .x, 
                                                  choices = unique(consumption_sample[[.x]]), 
                                                  selected = "")
      )
    })
    
    food_items <- reactive({
      
      # map all food selections to the foodname variable
      
      each_var <- purrr::map(filter_vars, ~ filter_var(consumption_sample[[.x]], input[[.x]]))
      
      selected <- purrr::reduce(each_var, `|`)
      
      consumption_sample[selected,] %>% pull(foodname)
      
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
        left_join(tbl_weight(), by = "subjectid") %>% 
        mutate(
          gr_day = total,
          gr_day_kg_bw = total / weight
        )
      
    })
    
    output$individual <- renderReactable({
      
      dta <- if(input$exposure_type == "Chronic") chronic() else acute()
      
      reactable(dta)
      
    })
    
    
    tbl_by_demo <- reactive({
      
      if(length(food_items()) == 0) {
        
        validate( "Filter your consumption data with food items")
      }
      
      dta <- if(input$exposure_type == "Chronic") chronic() else acute()
      
      aggregation <- 
        dta %>% 
        group_by(across(any_of(input$group_var))) %>% 
        summarise(across(any_of(input$per_day_type), aggregate_summary, .names = "{fn}")) %>% 
        mutate(
          across(-any_of(input$group_var), ~round(., 2))
        ) %>% 
        ungroup()
      
      
      if(!isTruthy(input$group_var)){
        
        tbl <- bind_cols(tbl_population(), tbl_consumers(), aggregation) 
        
      } else {
        
        tbl <- purrr::reduce(
          list(
            tbl_population(),
            tbl_consumers(),
            aggregation
          ),
          left_join,
          by = input$group_var
        ) 
        
      }
      
      tbl <- 
        tbl %>% 
        mutate(pct_cons = consumers / population, .after = consumers) %>% 
        mutate(pct_cons = percent(pct_cons, 0.01)) %>% 
        mutate(
          #across(c(-any_of(input$group_var), pct_cons), ~round(., 2))
          across(where(is.numeric), ~round(., 2))
        ) 
      
      # rename the columns for better presentation
      # what a messy code here.. 
      tbl %>% 
        rename(!!!unlist(keep(labels_list, ~ .x %in% names(tbl))))
      
    })
    
    output$tbl_by_demo <- renderReactable({
      
      reactable(tbl_by_demo())
      
      
    })
    
    
    
    output$title_table <- renderUI({
      
      
      if(!isTruthy(input$group_var)) {
        demo <- "Overall"
        
      } else {
        
        demo <- 
          paste0("per ",
                 names(keep(labels_list, ~.x %in% input$group_var)) %>% 
                   glue::glue_collapse(sep = ", ", last = " and ")
          )
      }
      
      grams_type <- names(keep(labels_list, ~ .x %in% input$per_day_type))
      
      tbl_title <- 
        glue::glue(
          "Descriptives ({grams_type}) - {input$exposure_type} consumption - {input$aggregation_type} based"
        )
      
      
      tagList(
        h3(tbl_title),
        h4(demo),
        br()
      )
      
    })
    
  })
}

## To be copied in the UI
# mod_tab_explore_ui("tab_explore_ui_1")

## To be copied in the server
# mod_tab_explore_server("tab_explore_ui_1")
