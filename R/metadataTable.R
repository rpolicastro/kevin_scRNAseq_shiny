
#' Meta-Data Table Input
#'
#' @inheritParams metadataPlotInput
#'
#' @export

metadataTableInput <- function(
  id,
  ident = "orig.ident",
  clusters = "seurat_clusters",
  nCount = "nCount_SCT",
  nFeature = "nFeature_SCT",
  percentMT = "percent.mt"
) {

  ## Namespace function.
  ns <- NS(id)

  ## Get clusters.
  clusters <- con %>%
    tbl("metadata") %>%
    distinct_at(clusters) %>%
    pull(clusters)

  ## Get samples.
  samples <- con %>%
    tbl("metadata") %>%
    distinct_at(ident) %>%
    pull(ident)

  ## Sidebar panel of inputs.
  sidebarPanel(width = 2,
    checkboxGroupInput(
      inputId = ns("sample"), label = "Sample",
      choices = samples, selected = samples
    ),
    selectInput(
      inputId = ns("cluster"), label = "Clusters",
      choices = clusters, selected = clusters,
      multiple = TRUE
    )
  )

}

#' Meta-Data Table Output Server
#'
#' @inheritParams metadataPlot
#'
#' @export

metadataTable <- function(
  input, output, session,
  ident = "orig.ident",
  clusters = "seurat_clusters",
  nCount = "nCount_SCT",
  nFeature = "nFeature_SCT",
  percentMT = "percent.mt"
) {

  ## Prepare the table.
  metadata_table <- reactive({

    # Grab the data from the database.
    metadata <- con %>%
      tbl("metadata") %>%
      filter_at(ident, all_vars(. %in% !!input$sample)) %>%
      filter_at(clusters, all_vars(.  %in% !!input$cluster)) %>%
      select_at(c("cell_id", ident, clusters, nCount, nFeature, percentMT)) %>%
      collect()

    setDT(metadata)

    return(metadata)

  }) 

  return(metadata_table)

}
