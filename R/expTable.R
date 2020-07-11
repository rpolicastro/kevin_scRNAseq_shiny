
#' Expression Table UI
#'
#' @inheritParams metadataPlotUI
#'
#' @export

expTableUI <- function(
  id,
  ident = "orig.ident",
  clusters = "seurat_clusters"
) {

  ## Namespace.
  ns <- NS(id)

  ## Get sample choices.
  sample_sheet <- con %>%
    tbl("samples") %>%
    collect

  experiments <- unique(sample_sheet$experiment)

  sidebarLayout(
  ## Expression table UI.
  sidebarPanel(width = 2,
    selectInput(
      inputId = ns("experiment"), label = "Experiment",
      choices = experiments,
      selected = experiments[1]
    ),
    uiOutput(ns("samples")),
    uiOutput(ns("clusters")),
    textAreaInput(
      inputId = ns("genes"), label = "Genes",
      value = "tdTomato\ntdTomatoStop", rows = 3
    ),
    checkboxInput(
      inputId = ns("log2t"), label = "Log2+1 Transform",
      value = FALSE
    )
  ),
  mainPanel(width = 10, DT::dataTableOutput(ns("table")))
  )

}

#' Expression Table Server
#'
#' @inheritParams metadataPlotServer
#'
#' @export

expTableServer <- function(
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

  ## Get the metadata.
  md <- reactive({
    metadata <- con %>%
      tbl(str_c(input$experiment, "_metadata")) %>%
      filter_at(ident, all_vars(. %in% !!input$samples)) %>%
      filter_at(clusters, all_vars(. %in% !!input$clusters)) %>%
      select_at(c("cell_id", ident, clusters)) %>%
      collect()

    setDT(metadata, key = "cell_id")
    return(metadata)
  })

  ## Get the gene counts.
  cn <- reactive({
    genes <- str_split(input$genes, "\\s", simplify = TRUE)[1, ]
    validate(
      need(length(genes) <= 10, "Can only display 10 genes or less.")
    )

    counts <- con %>%
      tbl(str_c(input$experiment, "_counts")) %>%
      filter(gene %in% genes) %>%
      collect()

    setDT(counts, key = "cell_id")
    counts <- counts[cell_id %in% md()[["cell_id"]]]
    if (input$log2t) {
      counts[, exp := log2(exp + 1)]
    }
    return(counts)
  })

  exp_table <- reactive({

    ## Make genes columns, and cells rows.
    counts <- dcast(cn(), cell_id ~ gene, value.var = "exp")

    ## Merge in the meta-data with the counts.
    counts <- merge(md(), counts)

    return(counts)   

  })

  output$table <- DT::renderDataTable(
    {exp_table()},
    extensions = "Buttons",
    options = list(
      order = list(list(2, "desc")),
      dom = "Bfrtpli",
      buttons = c('copy', 'csv', 'excel', 'print')
    )
  )

})
}
