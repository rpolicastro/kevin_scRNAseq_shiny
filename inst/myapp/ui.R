
fluidPage(theme = shinytheme("yeti"), navbarPage(title = "scRNA-seq",

## Meta-data page.
tabPanel("Meta-Data", tabsetPanel(

  ## Meta-data plot tab.
  tabPanel("Dim Plot", metadataPlotUI("metadataPlot")),
  ## Meta-data table tab.
  tabPanel("Table", metadataTableUI("metadataTable"))

)),

## Marker page.
tabPanel("Markers", tabsetPanel(

  ## Marker table tab.
  tabPanel("Table", markerTableUI("markerTable"))

#  ## Marker dot-plot.
##  tabPanel("Dot Plot", sidebarLayout(
##    markerDotplotInput("markerDotplotInput"),
##    mainPanel(width = 10, plotOutput("markerDotplotOutput"))
##  ))
#
)),

## Expression page.
tabPanel("Expression", tabsetPanel(

  ## Expression dim plot tab.
  tabPanel("Dim Plot", expDimPlotUI("expDimPlot")),

  ## Expession violin plot page.
  tabPanel("Expression Plot", expPlotUI("expPlot")),

  ## Expression table.
  tabPanel("Table", expTableUI("expTable"))
#    expTableInput("expTableInput"),
#    mainPanel(width = 10, DT::dataTableOutput("expTableOutput"))
#  ))
#
))

### Term Enrichment Page.
#tabPanel("Enrichment", tabsetPanel(
#
#  ## Table tab.
#  tabPanel("Table", sidebarLayout(
#    enrichTableInput("enrichTableInput"),
#    mainPanel(width = 10, DT::dataTableOutput("enrichTableOutput"))
#  )),
#
#  ## Dot-plot tab.
#  tabPanel("Dot Plot", sidebarLayout(
#    enrichDotplotInput("enrichDotplotInput"),
#    mainPanel(width = 9, plotOutput("enrichDotplotOutput"))
#  ))
#
#))

))
