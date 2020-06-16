
fluidPage(navbarPage(title = "scRNA-seq",

## Meta-data page.
tabPanel("Meta-Data", tabsetPanel(

  ## Meta-data plot tab.
  tabPanel("Dim Plot", sidebarLayout(
    metadataPlotInput("metadataPlotInput"),
    mainPanel(width = 10, plotOutput("metadataPlotOutput"))
  )),

  ## Meta-data table tab.
  tabPanel("Table", sidebarLayout(
    metadataTableInput("metadataTableInput"),
    mainPanel(width = 10, DT::dataTableOutput("metadataTableOutput"))
  ))

)),

## Marker page.
tabPanel("Markers", tabsetPanel(

  ## Marker table tab.
  tabPanel("Table", sidebarLayout(
    markerTableInput("markerTableInput"),
    mainPanel(width = 10, DT::dataTableOutput("markerTableOutput"))
  ))

))

))
