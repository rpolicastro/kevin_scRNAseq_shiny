
#' Marker Table UI
#'
#' @param id id
#'
#' @export

markerTableUI <- function(id) {

  ## Namespace
  ns <- NS(id)

  ## Get sample choices.
  sample_sheet <- con %>%
    tbl("samples") %>%
    collect

  experiments <- unique(sample_sheet$experiment)

  sidebarLayout(
  ## Marker table UI.
  sidebarPanel( width = 2,
    selectInput(
      inputId = ns("experiment"), label = "Experiment",
      choices = experiments,
      selected = experiments[1]
    ),
    uiOutput(ns("clusters")),
    fluidRow(
      column(6, numericInput(
        inputId = ns("fdrcutoff"), label = "FDR Cutoff",
        value = 0.05, min = 0, max = 1, step = 0.005
      )),
      column(6, numericInput(
        inputId = ns("fccutoff"), label = "FC Cutoff",
        value = 1.5, min = 1.5, step = 0.1
      ))
    )
  ),
  mainPanel(width = 10, DT::dataTableOutput(ns("table")))
  )

}

#' Marker Table Server
#'
#' @inheritParams metadataPlotServer
#'
#' @export

markerTableServer <- function(
  id
) {

moduleServer(id, function(input, output, session) {

  ## Get clusters for each experiment.
  clusts <- reactive({
    clusters <- con %>%
      tbl(str_c(input$experiment, "_metadata")) %>%
      distinct(seurat_clusters) %>%
      pull(seurat_clusters)
    return(clusters)
  })

  ## Render the clusters based on experiment.
  output$clusters <- renderUI({
    ns <- session$ns
    pickerInput(
      inputId = ns("clusters"), label = "Clusters",
      choices = clusts(), selected = clusts(),
      multiple = TRUE,
      options = list(
        `actions-box` = TRUE,
        `selected-text-format` = "count > 1"
      )
    )
  })

  ## Retrieve table.
  marker_table <- reactive({

    # Get table from database.
    fc_cutoff <- log2(input$fccutoff)

    markers <- con %>%
      tbl(str_c(input$experiment, "_markers")) %>%
      filter(
        cluster %in% !!input$clusters,
        p_val_adj < !!input$fdrcutoff,
        abs(avg_log2FC) > !!input$fccutoff
      ) %>%
      select(-avg_logFC) %>%
      collect()

    setDT(markers)

    return(markers)

  })

  ## Output the table.
  output$table <- DT::renderDataTable(
    {marker_table()},
    extensions = "Buttons",
    options = list(
      order = list(list(1, "asc"), list(7, "desc")),
      dom = "Bfrtpli",
      buttons = c('copy', 'csv', 'excel', 'print')
    )
  ) 

})
}
