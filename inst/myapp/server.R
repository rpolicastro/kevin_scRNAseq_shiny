
function(input, output, session) {

  ## Meta-data plot output.
  metadataPlotServer("metadataPlot")

  ## Meta-data table output.
  metadataTableServer("metadataTable")

  ## Marker table output.
  markerTableServer("markerTable")

#  ## Marker dot-plot output.
##  marker_dotplot <- callModule(markerDotplot, "markerDotplotInput")
##  output$markerDotplotOutput <- renderPlot({marker_dotplot()}, height = 750)

  ## Expression dim plot output.
  expDimPlotServer("expDimPlot")

  ## Expression plot output.
  expPlotServer("expPlot")

  ## Expression table output.
  expTableServer("expTable")

  ## Term enrichment table output.
  enrichTableServer("enrichTable")

  ## Term enrichment dotplot.
  enrichDotplotServer("enrichDotPlot")

}
