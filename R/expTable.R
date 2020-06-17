
#' Expression Table UI
#'
#' @inheritParams metadataPlotInput
#'
#' @export

expTableInput <- function(
  id,
  ident = "orig.ident",
  clusters = "seurat_clusters"
) {

  ## Namespace.
  ns <- NS(id)

  ## Get sample choices.
  sample_choices <- con %>%
    tbl("metadata") %>%
    distinct_at(ident) %>%
    pull(ident)

  ## Get cluster choices.
  cluster_choices <- con %>%
    tbl("metadata") %>%
    distinct_at(clusters) %>%
    pull(clusters)

  ## Expression table UI.
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
    ),
    textAreaInput(
      inputId = ns("genes"), label = "Genes",
      value = "tdTomato\ntdTomatoStop", rows = 3
    )
  )

}

#' Expression Table Server
#'
#' @inheritParams metadataPlot
#'
#' @export

expTable <- function(
  input, output, session,
  ident = "orig.ident",
  clusters = "seurat_clusters" 
) {

  exp_table <- reactive({
    ## Get meta-data.
    metadata <- con %>%
      tbl("metadata") %>%
      filter_at(ident, all_vars(. %in% !!input$sample)) %>%
      filter_at(clusters, all_vars(. %in% !!input$cluster)) %>%
      select_at(c("cell_id", ident, clusters)) %>%
      collect()

    setDT(metadata, key = "cell_id")

    ## Get counts.
    genes <- str_split(input$genes, "\\s", simplify = TRUE)[1, ]
    validate(
      need(length(genes) <= 10, "Can only display 10 genes or less.")
    )

    counts <- con %>%
      tbl("counts") %>%
      filter(gene %in% genes) %>%
      collect()

    setDT(counts, key = "cell_id")
    counts <- counts[cell_id %in% metadata[["cell_id"]]]
    counts <- dcast(counts, cell_id ~ gene, value.var = "exp")

    ## Merge in the meta-data with the counts.
    counts <- merge(metadata, counts)

    return(counts)   

  })

  return(exp_table)

}
