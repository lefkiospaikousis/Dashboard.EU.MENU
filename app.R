# Launch the ShinyApp (Do not remove this comment)
# To deploy, run: rsconnect::deployApp()
# Or use the blue button on top of this file

pkgload::load_all(export_all = FALSE,helpers = FALSE,attach_testthat = FALSE)
options( "golem.app.prod" = TRUE, 
         dplyr.summarise.inform = FALSE,
         shiny.maxRequestSize = 100*1024^2)
Dashboard.EU.MENU::run_app() # add parameters here (if any)
