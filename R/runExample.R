
#' @import data.table
#' @import shiny
#' @import shinythemes
#' @import pool
#' @import dplyr
#' @import stringr
#' @import ggplot2
#' @import shinyWidgets
#' @importFrom magrittr %>%
#' @importFrom DBI dbConnect
#' @importFrom RSQLite SQLite
#'
#' @export

runExample <- function() {

  ## Open up a connection to the SQLite database.
  con <<- dbPool(SQLite(), dbname = "data/murach.sqlite")

  ## Run the app.
  app_dir <- system.file("myapp", package = "scRNAseqShiny")
  runApp(app_dir, display.mode = "normal")

}
