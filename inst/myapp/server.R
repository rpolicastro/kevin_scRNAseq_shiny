
function(input, output) {

  ## Meta-data plot output.
  metadata_dimplot <- callModule(metadataPlot, "metadataPlotInput")
  output$metadataPlotOutput <- renderPlot({metadata_dimplot()}, height = 750)

  ## Meta-data table output.
  metadata_table <- callModule(metadataTable, "metadataTableInput")
  output$metadataTableOutput <- DT::renderDataTable(
    {metadata_table()},
    extensions = "Buttons",
    options = list(
      order = list(2, "desc"),
      dom = "Bfrtpli",
      buttons = c('copy', 'csv', 'excel', 'print')
    )
  )

}
