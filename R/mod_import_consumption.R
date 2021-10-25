#' import_consumption UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_import_consumption_ui <- function(id){
  ns <- NS(id)
  tagList(
    actionButton(ns("import"), "Upload a new consumption file", width = "100%", 
                 style = "white-space: normal; background:#8A2BE2; color:white ;margin-top: 8px;margin-bottom: 8px;"
    )
  )
}

#' import_consumption Server Functions
#'
#' @noRd 
mod_import_consumption_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    WaiterVerify<- div(
      style="color:black;", waiter::spin_2(), h3("Verifying dataset")
    ) 
    
    WaiterRead <- div(
      style = "color:black", waiter::spin_2(), h3("Reading Excel file...")
      
    )
    
    checks <- rv(
      
      is_excel  = FALSE,
      dup_names = FALSE,
      col_names = FALSE,
      all_good  = FALSE,
      temp_data = NULL,
      new_consumption = NULL
    )
    
    to_return <- rv(new_consumption = NULL,
                    new_consumption_name = NULL,
                    trigger = 0
    )
    
    
    initialise_checks <- function(){
      
      checks$is_excel  <- FALSE
      checks$dup_names <- FALSE
      checks$col_names <- FALSE
      checks$all_good  <- FALSE
      checks$temp_data <- NULL
      checks$new_consumption <- NULL
    }
    
    
    observeEvent(input$consumption_file$name ,{
      
      initialise_checks()
      
      file_type <- tools::file_ext(input$consumption_file$name)
      
      # Excel file check --------------------------------------------------------
      
      excel_ok <- file_type %in% c("xlsx",  "xls")
      
      checks$is_excel <- excel_ok
      
      if(!excel_ok){
        shinyjs::hide("show_dta", anim = TRUE)
        shinyjs::hide("check_content_UI", anim = TRUE)
        checks$all_good <- FALSE
        shinyFeedback::hideFeedback("consumption_file")
        shinyFeedback::showFeedbackDanger("consumption_file", "This is not an excel file")
        
      } else {
        
        # ok Read it
        waiter::waiter_show(color = "#EBE2E231", html = WaiterRead)
        temp_data <- readxl::read_excel(input$consumption_file$datapath, .name_repair = tolower) 
        waiter::waiter_hide()
        Sys.sleep(1)
      }
      
      # DUPLICATE COLUMN NAMES check
      req(checks$is_excel)
      
      dup_names <- names(temp_data)[duplicated(names(temp_data))]
      
      
      if(length(dup_names)>0){
        
        cols <- paste(dup_names, collapse = ", ")
        
        waiter::waiter_hide()
        shinyjs::hide("show_dta", anim = TRUE)
        shinyjs::hide("check_content_UI", anim = TRUE)
        shinyFeedback::hideFeedback("consumption_file")
        shinyFeedback::showFeedbackDanger("consumption_file", paste0("Error! Duplicated column names in the dataset: ", cols)
                                          
        )
        
      } else {
        
        checks$dup_names <- TRUE
      }
      
      # NAMES VALID CHECK
      req(checks$dup_names)
      
      names_check <- check_varsNeeded(temp_data, tolower(vars_needed_consumptionFdx2))
      
      
      if(!is.null(names_check)) {
        
        cols <- paste(names_check, collapse = ", ")
        
        waiter::waiter_hide()
        
        shinyjs::hide("show_dta", anim = TRUE)
        shinyjs::hide("check_content_UI", anim = TRUE)
        shinyFeedback::hideFeedback("consumption_file")
        shinyFeedback::showFeedbackDanger("consumption_file",paste0("Missing column names: ",cols))
        
      } else {
        
        checks$col_names <- TRUE
      }
      
      all_good <- checks$is_excel & checks$dup_names & checks$col_names
      
      if(all_good) {
        checks$new_consumption <- temp_data %>% 
          # keep only the nessescary columns & force type
          select(all_of(vars_needed_consumptionFdx2)) %>% 
          mutate(
            across(c(day, amountfood, amountfcooked, age, weight, wcoeff), as.numeric),
            across(c(serial, subjectid, foodexcode, gender, area, pop_class), as.character)
          )
        waiter::waiter_hide()
      }
      waiter::waiter_hide()
      
    }, priority = 10)
    
    
    
    # Validation checks -------------------------------------------------------
    
    
    validation_checks <- eventReactive(checks$new_consumption, {
      
      # All Good presumably :)
      
      waiter::waiter_show(color = "#EBE2E231", html = WaiterVerify)
      Sys.sleep(1)
      
      dataset <- checks$new_consumption 
      
      # Now check the type and content
      result     <- check_consumption_dataset(dataset)
      
      types_ok   <- all(result$col_type$is_type_ok)
      content_ok <- all(result$col_content$is_content_ok)
      # cooked food can be missing
      missing    <- result$missing %>% filter(!Column %in% c("amountfcooked", "enrecipedesc")) %>% .$missing
      missing_ok <- all(missing == 0)
      
      Sys.sleep(1)
      waiter_hide()
      
      
      return(
        list(
          nrow            = result$n_row,
          type            = dplyr::lst(types_ok, col_type = result$col_type),
          content         = dplyr::lst(content_ok, col_content = result$col_content),
          missing         = dplyr::lst(missing_ok, missing = result$missing),
          problem_cols    = result$problem_cols,
          tbl_problem_ids = result$tbl_problem_ids,
          problem_ids     = result$problem_ids
        )
      )
      
    })
    
    
    observeEvent(validation_checks(),{
      
      checks$all_good <- all(validation_checks()$content$content_ok,  validation_checks()$missing$missing_ok )
      #validation_checks()$type$types_ok, )
      
      shinyjs::show("check_content_UI", anim = TRUE)
      shinyjs::show("show_dta", anim = TRUE)
      
      if(!checks$all_good){
        
        shinyFeedback::hideFeedback("consumption_file")
        shinyFeedback::showFeedbackDanger("consumption_file", "There are some problems with the dataset. Please verify before proceeding")
        
      } else {
        
        shinyFeedback::hideFeedback("consumption_file")
        shinyFeedback::showFeedbackSuccess("consumption_file", "Success! Data have passed all validation checks")
      }
    })
    
    
    tbls_problems <- reactive({
      
      req(validation_checks())
      req(checks$all_good == FALSE)
      
      
      missing <- checks$new_consumption %>% filter(if_any(-c(amountfcooked), ~is.na(.))) 
      
      out <- 
        validation_checks()$tbl_problem_ids %>% 
        tibble::deframe() %>% 
        purrr::keep(~length(.)>0) %>% 
        purrr::map(~ checks$new_consumption[.,])
      
      out$missing <- missing
      
      out
      
    })
    
    
    # Outputs -----------------------------------------------------------------
    
    
    output$inform_loss <- renderUI({
      
      req(validation_checks())
      req(checks$all_good == FALSE)
      
      old_rows <- validation_checks()$nrow
      
      
      if(length(validation_checks()$problem_ids) ==0 ) {
        new_rows <- nrow(checks$new_consumption)
      } else {
        new_rows <- nrow(
          checks$new_consumption[-validation_checks()$problem_ids,] %>% 
            filter(across(c(amountfood, area, serial, subjectid, foodexcode, age, pop_class, weight, wcoeff), ~!is.na(.)))
        )
        
      }
      
      tagList(
        p(
          glue::glue("Your dataset has some errors and/or missing values. ImpoRisk can exclude these resulting in the loss of 
               {old_rows - new_rows} ({percent(1 - new_rows/old_rows,accuracy = 0.01)}) cases."), br(),
          "Note: Missing values from the ", strong("amountfcooked")," column will not be excluded", br(),
          "You can ",downloadLink(ns("down_problems"),"Download an .xlsx workbook with the errors found", style = "padding-bottom: 10px; padding-top: 10px" )
          )
      )
      
    })
    
    
    
    output$down_problems <- downloadHandler(
      filename = function() {
        paste("errorsConsumptionFile-", Sys.Date(), ".xlsx", sep="")
      },
      content = function(file) {
        
        writexl::write_xlsx(tbls_problems(), file)
      }
    )
    
    output$check_col_type <- DT::renderDT({
      # we mught not need it anymore. I force the types and present the issues that appear
      req(validation_checks())
      req(FALSE) 
      
      validation_checks()$type$col_type %>% 
        mutate(
          Check = ifelse(is_type_ok, get_icon("check"), get_icon("times"))
        ) %>% 
        DT::datatable(
          options = list(
            dom = 't',
            pageLength = ncol(isolate(checks$new_consumption)),
            processing = FALSE
          ),
          rownames = FALSE,
          selection = "none",
          caption = "Check for the type of the columns",
          escape = FALSE
        ) %>% 
        DT::formatStyle(
          'is_type_ok',
          target = 'row',
          Color = DT::styleEqual(c(0, 1), c('red', 'green'))
        )
      
      
    })
    
    output$check_missing <- DT::renderDT({
      
      req(validation_checks())
      
      
      # | Column =="amountfcooked"
      validation_checks()$missing$missing %>% 
        mutate(
          Check = ifelse(missing==0, get_icon("check"), get_icon("times"))
        ) %>% 
        DT::datatable(
          options = list(
            dom = 't',
            pageLength = ncol(isolate(checks$new_consumption)),
            processing = FALSE
          ),
          rownames = FALSE,
          selection = "none",
          caption = "Checks for the missing values",
          escape = FALSE
        ) %>% 
        DT::formatStyle(
          'missing',
          target = 'row',
          Color = DT::styleEqual(c(0), c('green'), default = "red")
        )
      
    })
    
    
    output$check_col_content <- DT::renderDT({
      
      req(validation_checks())
      
      validation_checks()$content$col_content %>% 
        mutate(
          Check = ifelse(is_content_ok, get_icon("check"), get_icon("times"))
        ) %>% 
        DT::datatable(
          options = list(
            dom = 't',
            pageLength = ncol(isolate(checks$new_consumption)),
            processing = FALSE
          ),
          rownames = FALSE,
          selection = "none",
          caption = "Checks for the Content of the columns",
          escape = FALSE
        ) %>% 
        DT::formatStyle(
          'is_content_ok',
          target = 'row',
          Color = DT::styleEqual(c(0, 1), c('red', 'green'))
        )
      
    })
    
    
    output$consumption <- renderReactable({ 
      
      req(checks$new_consumption)
      req(checks$col_names)
      req(checks$is_excel)
      
      
      checks$new_consumption %>% 
        reactable(
          striped = TRUE,
          highlight = TRUE,
          searchable = TRUE,
          resizable = TRUE,
          showPageSizeOptions = TRUE
        )
    })
    
    
    observeEvent(input$confirm_import,{
      
      removeModal()
      
      if(length(validation_checks()$problem_ids) == 0){
        to_return$new_consumption <- checks$new_consumption
      } else {
        checks$new_consumption[-validation_checks()$problem_ids,]
      }
      
      
      to_return$new_consumption <- 
        # remove all empty
        checks$new_consumption %>% 
        filter(across(c(amountfood, area, serial, subjectid, foodexcode, age, pop_class, weight, wcoeff), ~!is.na(.))) %>% 
        # add the  foodname
        mutate(termCode = stringr::str_extract(foodexcode,"^.{5}")) %>% 
        left_join(
          mtx_levels %>% select(termCode, foodname = termExtendedName)
          , by = "termCode"
        ) %>% 
        select(-termCode) %>% 
        relocate(foodname, .after =  foodexcode)
      
      to_return$new_consumption_name <- input$consumption_file$name
      
      to_return$trigger <- isolate(to_return$trigger) + 1
      
      initialise_checks()
      
    })
    
    
    
    # Load  the Dialog Window for importing ----
    observeEvent(input$import, {
      
      load_modal <- function(){
        ns <- session$ns
        modalDialog(
          tagList(
            #h3("The Consumption file specifications:"),
            shinydashboard::box(
              solidHeader = TRUE,width = NULL, collapsible = TRUE, collapsed = TRUE, status = "warning",
              title = "The Consumption file specifications:",
              tags$ul(
                tags$li(strong("Input data"), ": Consumption data with food consumption occasions at the participant level"),
                tags$li(strong("Data file format"), ": Excel (.xlsx/ .xls)"),
                tags$li(strong("Mandatory data fields")),
                tags$ul(
                  tags$li(strong("SERIAL"), ": Unique record identifier for each consumption occassion"),
                  tags$li(strong("SUBJECTID"), ": A unique participant id"),
                  tags$li(strong("DAY"), ": Day of the consumption in numeric format. Positive Integer value up to 5"),
                  tags$li(strong("AMOUNTFOOD"), ": Raw quantity of the food consumed (grams or ml)"),
                  tags$li(strong("AMOUNTFCOOKED"), ": Cooked quantity of the food consumed (grams or ml) [missing values are allowed]"),
                  tags$li(strong("FOODEXCODE"), ": Full FoodEx2 code of the consumed food"),
                  tags$li(strong("GENDER"), ": Gender of the participant [Valid values: MALE, FEMALE, Other]"),
                  tags$li(strong("AGE"), ": Age of the participant in 'years'"),
                  tags$li(strong("WEIGHT"), ": Weight of the participant in 'Kg'"),
                  tags$li(strong("AREA"), ": Area of participant. Free text"),
                  tags$li(strong("POP_CLASS"), ": The particpants's population class", a(href= "https://www.efsa.europa.eu/en/efsajournal/pub/3944", " according to (EFSA,2014) guidance."),  
                          " [Valid values: Infants, Toddlers, Other children, Adolescents, Adults, Elderly, Pregnant Women]"),
                  tags$li(strong("WCOEFF"), ": The weighting cooefficient of the participant for the representativeness in the population",
                          a(href = "https://en.wikipedia.org/wiki/Weighted_arithmetic_mean#Frequency_weights", "See Frequency Weights"))
                )
              ),
            )
            ,
            br(),
            fileInput(ns("consumption_file"), "Upload Consumption data", accept = ".xlsx", width = "100%"),
            uiOutput(ns("inform_loss")),
            br(),
            hr(),
            shinyjs::hidden(
              div(id = ns("check_content_UI"),
                  shinydashboard::tabBox(id = ns("check_tables"),width = NULL,
                                         tabPanel("Column content",
                                                  DT::DTOutput(ns("check_col_content"))
                                         ),
                                         tabPanel("Missing Values",
                                                  DT::DTOutput(ns("check_missing"))
                                         ),
                                         tabPanel("Uploaded Data",
                                                  reactableOutput(ns("consumption"))
                                         )
                                         
                  )
              )
            ),
            shinyjs::hidden(
              div(id= ns("show_dta"),
                  actionButton(ns("confirm_import"), "Import the data to ImproRisk", width = "100%", 
                               class = "btn btn-success",style = "margin-bottom: 8px; margin-top: 6px")
              )
            )
            
          ), size = "l"
        )
      }
      
      showModal(load_modal())
    })
    
    
    
    # Return ----
    
    return(to_return)
    
    
    
    
    
  })
}

## To be copied in the UI
# mod_import_consumption_ui("import_consumption_ui_1")

## To be copied in the server
# mod_import_consumption_server("import_consumption_ui_1")
