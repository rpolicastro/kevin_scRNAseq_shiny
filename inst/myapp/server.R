
function(input, output) {

  result <- callModule(metadataPlot, "metadataPlotInput")
  output$metadataPlotOutput <- renderText({result()})

}
