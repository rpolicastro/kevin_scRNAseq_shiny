
function(input, output, session) {

  ## Meta-data plot output.
  metadataPlotServer("metadataPlot")
#  metadata_dimplot <- callModule(metadataPlot, "metadataPlotInput")
#  output$metadataPlotOutput <- renderPlot({metadata_dimplot()}, height = 750)

#  ## Meta-data table output.
#  metadata_table <- callModule(metadataTable, "metadataTableInput")
#  output$metadataTableOutput <- DT::renderDataTable(
#    {metadata_table()},
#    extensions = "Buttons",
#    options = list(
#      order = list(2, "desc"),
#      dom = "Bfrtpli",
#      buttons = c('copy', 'csv', 'excel', 'print')
#    )
#  )
#
#  ## Marker table output.
#  marker_table <- callModule(markerTable, "markerTableInput")
#  output$markerTableOutput <- DT::renderDataTable(
#    {marker_table()},
#    extensions = "Buttons",
#    options = list(
#      order = list(list(1, "asc"), list(7, "desc")),
#      dom = "Bfrtpli",
#      buttons = c('copy', 'csv', 'excel', 'print')
#    )
#  )
#
#  ## Marker dot-plot output.
##  marker_dotplot <- callModule(markerDotplot, "markerDotplotInput")
##  output$markerDotplotOutput <- renderPlot({marker_dotplot()}, height = 750)
#
#  ## Expression dim plot output.
#  exp_dimplot <- callModule(expDimPlot, "expDimPlotInput")
#  output$expDimPlotOutput <- renderPlot({exp_dimplot()}, height = 750)
#
#  ## Expression plot output.
#  exp_plot <- callModule(expPlot, "expPlotInput")
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
