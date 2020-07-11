
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
#  output$expPlotOutput <- renderPlot({exp_plot()}, height = 750)
#
#  ## Expression table output.
#  exp_table <- callModule(expTable, "expTableInput")
#  output$expTableOutput <- DT::renderDataTable(
#    {exp_table()},
#    extensions = "Buttons",
#    options = list(
#      order = list(list(2, "desc")),
#      dom = "Bfrtpli",
#      buttons = c('copy', 'csv', 'excel', 'print')
#    )
#  )
#
#  ## Term enrichment table output.
#  enrich_table <- callModule(enrichTable, "enrichTableInput")
#  output$enrichTableOutput <- DT::renderDataTable(
#    {enrich_table()},
#    extensions = "Buttons",
#    options = list(
#      order = list(list(2, "desc"), list(9, "asc")),
#      dom = "Bfrtpli",
#      buttons = c('copy', 'csv', 'excel', 'print')
#    )
#  )
#
#  ## Term enrichment dotplot.
#  enrich_dotplot <- callModule(enrichDotplot, "enrichDotplotInput")
#  output$enrichDotplotOutput <- renderPlot({enrich_dotplot()}, height = 750)

}
