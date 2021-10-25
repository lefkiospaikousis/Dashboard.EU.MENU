#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic 
    dashboardPage(
      dashboardHeader(title = "EU MENU dashboard"),
      dashboardSidebar(
        sidebarMenu(id = "tabs",
                    
                    # selectInput("bank", "Select the BANK", "", selected = NULL),
                    # div(
                    #   valueBoxOutput("rec_month", width = NULL),
                    #   style = "margin: 13px"
                    # ),
                    tags$hr(style = "border-color: white; width:80%"),
                    menuItem("Data", tabName = "data", icon = icon("university")),
                    menuItem("Explore", tabName = "explore", icon = icon("magic")),
                    # menuItem("Load files", tabName = "load", icon = icon("file-upload")),
                    tags$hr(style = "border-color: white; width:80%"),
                    p(paste0("Version: ", golem::get_golem_version()), style = "margin-left:25px")
                    
        )
      ),
      dashboardBody(
        tabItems(
          
          tabItem(tabName = "data",
                  mod_tab_data_ui("tab_data_ui_1")
          ),
          
          tabItem(tabName = "explore",
                  mod_tab_explore_ui("tab_explore_ui_1")
          )
        )
      )
    )
    
  )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
 
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'Dashboard-EU_MENU'
    ),
    shinyjs::useShinyjs(),
    waiter::use_waiter(),
    shinyFeedback::useShinyFeedback()
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

