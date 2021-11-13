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
            radioButtons(ns("per_day_type"), "Per Kg bw?", 
                         choices = c("grams/day" = "gr_day", 
                                     "grams/day/Kg bw"= "gr_day_kg_bw"))
          ),
          selectInput(ns("group_var"), "Select demographic", 
                      multiple = TRUE,  
                      choices = labels_list[c("Gender", "Population Class", "Area")],
                      selected = ""
          )
      ),
      box(width = 6, 
          uiOutput(ns("filter_ui"))
      ),
      shinyWidgets::dropdownButton(inputId = ns("options"), label = "",
                                   p(strong("Customise")),
                                   numericInput(ns("digits"), "Decimals", 2, 0, 10, 1),
                                   shinyjs::hidden(
                                   radioButtons(ns("amount_food"), "Amount Food",
                                                choices = c("Raw" = "amountfood", "Cooked" = "amountfcooked")
                                   )),
                                   circle = FALSE, status = "primary", 
                                   icon = icon("gear"), width = "100px",
                                   right = TRUE,
                                   tooltip = shinyWidgets::tooltipOptions(title = "Click for options!"),
                                   size = "xs"
      )
    ),
    
    fluidRow(
      tabBox(id = ns("tabs_explore"), width = 12,
             tabPanel(title = "Statistics",
                      uiOutput(ns("title_table")),
                      mod_downloadTable_ui(ns("tbl_by_demo")),
                      reactableOutput(ns("tbl_by_demo")) %>% with_spinner()
             ),
             tabPanel(title = "Individual",
                      uiOutput(ns("title_individual")),
                      mod_downloadTable_ui(ns("individual")),
                      reactableOutput(ns("individual")) %>% with_spinner()
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
    
    callModule(mod_downloadTable_server, "tbl_by_demo",
               table_name = "DESCRIPTIVES",
               the_table = tbl_by_demo)
    
    callModule(mod_downloadTable_server, "individual",
               table_name = "Individual",
               the_table = individual)
    
    
    
    
    # Name is the Label of the filter. Value must be the actual variable name
    filter_vars <- keep(labels_list, ~ .x %in% c("foodname", "orfoodname", "enfoodname", "enrecipedesc"))
    
    
    output$filter_ui <- renderUI({
      
      tagList(
        h4('FIlter your dataset. Your selections will "add up"'),
        p('Use this section to COMBINE food items, eg.select "Cow milk" and "Goat milk" to get the total milk consumption considering both of these items')
        
        , purrr::imap(filter_vars, ~ col_4(
          make_filter(
            var = consumption()[[.x]], id = .x, label = .y,  session = session
          )
        )
        )
      )
    })
    
    
    observe({
      
      purrr::map(filter_vars, ~ updateSelectInput(inputId = .x, 
                                                  choices = unique(consumption()[[.x]]), 
                                                  selected = "")
      )
    })
    
    food_items <- reactive({
      
      # map all food selections to the foodname variable
      
      each_var <- purrr::map(filter_vars, ~ filter_var(consumption()[[.x]], input[[.x]]))
      
      selected <- purrr::reduce(each_var, `|`)
      
      consumption()[selected,] %>% pull(foodname) %>% unique()
      
    })
    
    
    
    # Helper datasets ---------------------------------------------------------
    
    
    tbl_n_days      <- reactive({
      
      consumption() %>% group_by(subjectid) %>% summarise(n_days = max(day))
      
    }) 
    
    tbl_weight      <- reactive({
      
      consumption() %>% 
        distinct(subjectid, .keep_all = TRUE) %>% select(subjectid, weight)
      
    })
    
    
    tbl_population <- reactive({
      
      consumption() %>% 
        distinct(subjectid, .keep_all = TRUE) %>% 
        count(across(any_of(input$group_var)), name = "population")
    }) 
    
    
    tbl_consumers <- reactive({
      
      # need to filter it for the food selected
      consumption() %>% 
        filter(foodname %in% food_items()) %>% 
        distinct(subjectid, .keep_all = TRUE) %>% 
        count(across(any_of(input$group_var)), name = "consumers")
      
      
    })
    
    
    # Aggregation table -------------------------------------------------------
    
    
    # Per subject grams per day
    
    acute <- reactive({
      
      dta <- switch (input$aggregation_type,
                     
                     Consumers  = {
                       
                       if(input$amount_food == "amountfood") {
                         
                         filter(consumption(), foodname %in% food_items())
                         
                       } else if (input$amount_food == "amountfcooked") {
                         # COOKED - Rows with missing `amountfcooked` values need to be removed
                         # Summing them later will result in total consumption = 0
                         # and the consumers of the cooked food will be the same as all the consumers of the raw food
                         # This way we distinguish the consumers of the cooked food
                         filter(consumption(), !is.na(amountfcooked), foodname %in% food_items())
                         
                       } else {
                         validate("Wrong variables")
                       }
                       
                       
                     },
                     
                     Population = {
                       
                       if(input$amount_food == "amountfood") {
                         
                         # Easy. I don;t filter the food combination, rather I keep the consumption for that
                         # food combination and all other consumers set as consumption = 0
                         consumption() %>% 
                           mutate(across(all_of(input$amount_food), ~ if_else(foodname %in% food_items(), .x, 0)))
                         
                       } else if (input$amount_food == "amountfcooked") {
                         # COOKED - 
                         # Those who a) consumed the food and b) as cooked, will keep the amountfcooked. 
                         # Rest get a zero consumption on the amountfcooked. 
                         # Like all had the chance to consume the food as cooked but didn;t
                         
                         consumption() %>% 
                           mutate(
                             across(all_of(input$amount_food), ~ if_else(foodname %in% food_items() & !is.na(amountfcooked), .x, 0))
                           )
                         
                       } else {
                         validate("Wrong variables")
                       }
                       
                       
                       
                     },
                     
                     validate("Something went wrong. Contact the app author. Wrong Aggregation Type")
      )
      
      if(nrow(dta) == 0) validate("No consumers of this food combination")
      
      dta %>% 
        # group also by `day` for the acute
        group_by(across(c(subjectid, any_of(input$group_var), day))) %>% 
        summarise(total = sum(.data[[input$amount_food]], na.rm = TRUE)) %>% 
        ungroup() %>% 
        left_join(tbl_weight(), by = "subjectid") %>% 
        mutate(
          gr_day = total,
          gr_day_kg_bw = total / weight
        ) %>% 
        select(-total)
      
    })
    
    
    chronic <- reactive({
      
      
      dta <- switch (input$aggregation_type,
                     
                     Consumers  = {
                       
                       if(input$amount_food == "amountfood") {
                         
                         filter(consumption(), foodname %in% food_items())
                         
                       } else if (input$amount_food == "amountfcooked") {
                         # COOKED - Rows with missing `amountfcooked` values need to be removed
                         # Summing them later will result in total consumption = 0
                         # and the consumers of the cooked food will be the same as all the consumers of the raw food
                         # This way we distinguish the consumers of the cooked food
                         filter(consumption(), !is.na(amountfcooked), foodname %in% food_items())
                         
                       } else {
                         validate("Wrong variables")
                       }
                       
                       
                     },
                     
                     Population = {
                       
                       if(input$amount_food == "amountfood") {
                         
                         # Easy. I don;t filter the food combination, rather I keep the consumption for that
                         # food combination and all other consumers set as consumption = 0
                         consumption() %>% 
                           mutate(across(all_of(input$amount_food), ~ if_else(foodname %in% food_items(), .x, 0)))
                         
                       } else if (input$amount_food == "amountfcooked") {
                         # COOKED - 
                         # Those who a) consumed the food and b) as cooked, will keep the amountfcooked. 
                         # Rest get a zero consumption on the amountfcooked. 
                         # Like all had the chance to consume the food as cooked but didn;t
                         
                         consumption() %>% 
                           mutate(
                             across(all_of(input$amount_food), ~ if_else(foodname %in% food_items() & !is.na(amountfcooked), .x, 0))
                           )
                         
                       } else {
                         validate("Wrong variables")
                       }
                       
                       
                       
                     },
                     
                     validate("Something went wrong. Contact the app author. Wrong Aggregation Type")
      )
      
      if(nrow(dta) == 0) validate("No consumers of this food combination")
      
      
      # Return
      
      dta %>% 
        group_by(subjectid, across(any_of(input$group_var))) %>% 
        summarise(total = sum(.data[[input$amount_food]], na.rm = TRUE)) %>% 
        ungroup() %>% 
        left_join(tbl_n_days(), by = "subjectid") %>% 
        left_join(tbl_weight(), by = "subjectid") %>% 
        mutate(
          gr_day = total / n_days,
          gr_day_kg_bw = total / n_days / weight
        )%>% 
        select(-total)
    })
    
    
    
    individual <- reactive({
      
      validate(need(consumption(), "No data uploaded"))
      
      dta <- if(input$exposure_type == "Chronic") chronic() else acute()
      
      tbl <- 
        dta %>% 
        mutate(
          across(where(is.numeric), ~round(., input$digits))
        ) 
      
      tbl %>% 
        rename(!!!unlist(keep(labels_list, ~ .x %in% names(tbl))))
      
      
    })
    
    output$individual <- renderReactable({
      
      individual() %>% 
        reactable(
          searchable = TRUE
        )
      
    })
    
    
    
    
    tbl_by_demo <- reactive({
      
      validate(need(consumption(), "No data uploaded"))
      
      if(length(food_items()) == 0) {
        
        validate( "Filter your consumption data with food items")
      }
      
      dta <- if(input$exposure_type == "Chronic") chronic() else acute()
      
      aggregation <- 
        dta %>% 
        group_by(across(any_of(input$group_var))) %>% 
        summarise(across(any_of(input$per_day_type), aggregate_summary, .names = "{fn}")) %>% 
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
        mutate(pct_cons = percent(pct_cons, 0.01)) 
      
      # rename the columns for better presentation
      # what a messy code here.. 
      tbl %>% 
        rename(!!!unlist(keep(labels_list, ~ .x %in% names(tbl))))
      
    })
    
    output$tbl_by_demo <- renderReactable({
      
      tbl_by_demo() %>% 
        mutate(
          across(where(is.numeric), ~round(., input$digits))
        ) %>% 
        reactable(
          searchable = TRUE
        )
      
      
    })
    
    
    output$title_table <- renderUI({
      
      
      if(!isTruthy(input$group_var)) {
        demo <- "-Overall-"
        
      } else {
        
        demo <- 
          paste0("- by ",
                 names(keep(labels_list, ~.x %in% input$group_var)) %>% 
                   glue::glue_collapse(sep = ", ", last = " and ")
                 , " -"
          )
      }
      
      grams_type <- names(keep(labels_list, ~ .x %in% input$per_day_type))
      food_var <- if(input$amount_food == "amountfood") "Raw" else "Cooked"
      
      tbl_title <- 
        glue::glue(
          "Descriptives ({grams_type}) - {input$exposure_type} consumption - {food_var} food - {input$aggregation_type} based"
        )
      
      
      tagList(
        h3(tbl_title),
        h4(demo),
        br()
      )
      
    })
    
    output$title_individual <- renderUI({
      
      tbl_title <- 
        glue::glue(
          "Individual Consumption - {input$exposure_type}" # - {input$aggregation_type} based
        )
      
      tagList(
        h3(tbl_title),
        br()
      )
      
    })
    
    
    
    
    
    
    
  })
}

## To be copied in the UI
# mod_tab_explore_ui("tab_explore_ui_1")

## To be copied in the server
# mod_tab_explore_server("tab_explore_ui_1")


# acute <- reactive({
#   
#   consumption() %>% 
#     {if(input$aggregation_type == "Consumers"){
#       filter(., foodname %in% food_items())
#     } else {
#       mutate(., .data[[input$amount_food]] := if_else(foodname %in% food_items(), .data[[input$amount_food]], 0))
#     }} %>% 
#     # group also by day for the acute
#     group_by(across(c(subjectid, any_of(input$group_var), day))) %>% 
#     summarise(total = sum(.data[[input$amount_food]], na.rm = TRUE)) %>% 
#     ungroup() %>% 
#     left_join(tbl_weight(), by = "subjectid") %>% 
#     mutate(
#       gr_day = total,
#       gr_day_kg_bw = total / weight
#     ) %>% 
#     select(-total)
#   
# })


# chronic <- reactive({
#   
#   consumption() %>% 
#     {if(input$aggregation_type == "Consumers"){
#       filter(., foodname %in% food_items())
#     } else {
#       mutate(., .data[[input$amount_food]] := if_else(foodname %in% food_items(), .data[[input$amount_food]], 0))
#     }} %>% 
#     group_by(subjectid, across(any_of(input$group_var))) %>% 
#     summarise(total = sum(.data[[input$amount_food]], na.rm = TRUE)) %>% 
#     ungroup() %>% 
#     left_join(tbl_n_days(), by = "subjectid") %>% 
#     left_join(tbl_weight(), by = "subjectid") %>% 
#     mutate(
#       gr_day = total / n_days,
#       gr_day_kg_bw = total / n_days / weight
#     )%>% 
#     select(-total)
#   
# })