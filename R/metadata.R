
#' Meta-Data Plot Input UI
#'
#' @import shiny
#'
#' @param id Input id for use in server functions.
#'
#' @export

metadataPlotInput <- function(id) {

  ## Namespace function using ID.
  ns <- NS(id)

  ## Sidebar panel of inputs.
  sidebarPanel(
    selectInput(ns("test"), "test", c("yes", "no"))   
  )

}

#' Meta_data Plot Input Server
#'
#' @param input input
#' @param output output
#' @param session session
#'
#' @export

metadataPlot <- function(input, output, session) {

  ## Get the answer.
  answer <- reactive({input$test})
  return(answer)

}
