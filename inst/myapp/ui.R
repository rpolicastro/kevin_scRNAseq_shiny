
fluidPage(navbarPage(title = "scRNA-seq",

## Meta-data page.
tabPanel("Meta-Data", tabsetPanel(

  ## Meta-data plot tab.
  tabPanel("Plot", sidebarLayout(
    metadataPlotInput("metadataPlotInput"),
    mainPanel(textOutput("metadataPlotOutput"))
  ))

))

))
