
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
  cluster_choices <- con %>%
    tbl("metadata") %>%
    distinct_at(clusters) %>%
    pull(clusters)

  ## Get samples.
  sample_choices <- con %>%
    tbl("metadata") %>%
    distinct_at(ident) %>%
    pull(ident)

  ## Sidebar panel of inputs.
  sidebarPanel(width = 2,
    pickerInput(
      inputId = ns("sample"), label = "Samples",
      choices = sample_choices, selected = sample_choices,
      multiple = TRUE,
      options = list(
        `actions-box` = TRUE,
        `selected-text-format` = "count > 1")
    ),
    pickerInput(
      inputId = ns("cluster"), label = "Clusters",
      choices = cluster_choices, selected = cluster_choices,
      multiple = TRUE,
      options = list(
        `actions-box` = TRUE,
        `selected-text-format` = "count > 1")
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
