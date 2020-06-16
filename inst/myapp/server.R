
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

  ## Marker table output.
  marker_table <- callModule(markerTable, "markerTableInput")
  output$markerTableOutput <- DT::renderDataTable(
    {marker_table()},
    extensions = "Buttons",
    options = list(
      order = list(list(1, "asc"), list(7, "desc")),
      dom = "Bfrtpli",
      buttons = c('copy', 'csv', 'excel', 'print')
    )
  )

  ## Expression dim plot output.
  exp_dimplot <- callModule(expDimPlot, "expDimPlotInput")
  output$expDimPlotOutput <- renderPlot({exp_dimplot()}, height = 750)

}
