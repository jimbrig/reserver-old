#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {

  val_tbl <- shiny::reactive({
    shiny::req(input$val_date)
    create_lossrun(input$val_date)
  })

  mod_dashboard_module_server("dashboard_module_ui_1", val_tbl)

}
