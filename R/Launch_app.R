#' Launch App function
#'
#' Function launching the PDX app..;
#'
#'
#' @return None
#' @examples
#' if (interactive()) PDX_app()
#' @export
#'

#PDX_app <- function() {shinyApp(ui = ui_PDX_app, server = server_PDX_app) }

PDX_app <- function() {

#if (interactive()) {

  ### Functions ###

library(BioshinyModules)
library(shinydashboardPlus)
shinyApp(ui = ui_PDX_app, server = server_PDX_app)
}
#}


