
#' Meta-Data Table Input
#'
#' @inheritParams metadataPlotUI
#'
#' @export

metadataTableUI <- function(
  id,
  ident = "orig.ident",
  clusters = "seurat_clusters",
  nCount = "nCount_SCT",
  nFeature = "nFeature_SCT",
  percentMT = "percent.mt"
) {

  ## Namespace.
  ns <- NS(id)

  ## Get sample choices.
  sample_sheet <- con %>%
    tbl("samples") %>%
    collect

  experiments <- unique(sample_sheet$experiment)

  ## Sidebar panel of inputs.
  sidebarLayout(
  sidebarPanel(width = 2,
    selectInput(
      inputId = ns("experiment"), label = "Experiment",
      choices = experiments,
      selected = experiments[1]
    ),
    uiOutput(ns("samples")),
    uiOutput(ns("clusters")),
    uiOutput(ns("datatypes"))
  ),
  mainPanel(width = 10, DT::dataTableOutput(ns("table")))
  )

}

#' Meta-Data Table Output Server
#'
#' @inheritParams metadataPlotServer
#'
#' @export

metadataTableServer <- function(
  id,
  ident = "orig.ident",
  clusters = "seurat_clusters"
) {

moduleServer(id, function(input, output, session) {

  ## Get sample table.
  samps <- con %>%
    tbl("samples") %>%
    collect
  samps <- as.data.table(samps)

  ## Get clusters for each experiment.
  clusts <- reactive({
    clusters <- con %>%
      tbl(str_c(input$experiment, "_metadata")) %>%
      distinct_at(clusters) %>%
      pull(clusters)
    return(clusters)
  })

  ## Render the samples based on experiment.
  output$samples <- renderUI({
    ns <- session$ns
    choices <- samps[experiment == input$experiment]$samples
    pickerInput(
      inputId = ns("samples"), label = "Samples",
      choices = choices, selected = choices,
      multiple = TRUE,
      options = list(
        `actions-box` = TRUE,
        `selected-text-format` = "count > 1"
      )
    )
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

  ## Prepare the table.
  metadata_table <- reactive({

    # Grab the data from the database.
    metadata <- con %>%
      tbl(str_c(input$experiment, "_metadata")) %>%
      filter_at(ident, all_vars(. %in% !!input$samples)) %>%
      filter_at(clusters, all_vars(.  %in% !!input$clusters)) %>%
      collect()

    setDT(metadata)

    return(metadata)

  }) 

  ## Get the column names of the metadata.
  cols <- reactive({
    cols <- colnames(metadata_table())
    cols <- cols[cols != "cell_id"]
    return(cols)
  })

  ## Render the column name choices.
  output$datatypes <- renderUI({
    ns <- session$ns
    pickerInput(
      inputId = ns("datatypes"), label = "Columns",
      choices = c(cols()), selected = c(ident, clusters),
      multiple = TRUE,
      options = list(
        `actions-box` = TRUE,
        `selected-text-format` = "count > 1"
      )
    )
  })

  ## Return only the desired columns.
  md <- reactive({
    selected_cols <- c("cell_id", input$datatypes)
    md <- metadata_table()[, ..selected_cols]
    return(md)
  })

  ## Output the table.
  output$table <- DT::renderDataTable(
    {md()},
    extensions = "Buttons",
    options = list(
      dom = "Bfrtpli",
      buttons = c('copy', 'csv', 'excel', 'print')
    )
  )

})
}
