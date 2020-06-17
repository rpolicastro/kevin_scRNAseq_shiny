
#' Marker Dot-Plot UI.
#'
#' @inheritParams metadataPlotInput
#'
#' @export

markerDotplotInput <- function(
  id,
  ident = "orig.ident",
  clusters = "seurat_clusters"
) {

  ## Namespace.
  ns <- NS(id)

  ## Get sample names.
  sample_choices <- con %>%
    tbl("metadata") %>%
    distinct_at(ident) %>%
    pull(ident)

  ## Get cluster names.
  cluster_choices <- con %>%
    tbl("metadata") %>%
    distinct_at(clusters) %>%
    pull(clusters)

  ## Marker dot-plot sidebar.
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
    numericInput(
      inputId = ns("numgenes"), label = "Genes Per Cluster",
      value = 2, min = 1, max = 5, step = 1
    ),
    sliderInput(
      inputId = ns("fontsize"), label = "Font Size",
      min = 1, max = 36, value = 18, step = 1
    )
  )

}

#' Marker Dot-Plot Server.
#'
#' @inheritParams metadataPlot
#'
#' @export

markerDotplot <- function(
  input, output, session,
  ident = "orig.ident",
  clusters = "seurat_clusters"
) {

  ## Dot plot.
  marker_dotplot <- reactive({

    ## Get meta-data.
    metadata <- con %>%
      tbl("metadata") %>%
      filter_at(ident, all_vars(. %in% !!input$sample)) %>%
      filter_at(clusters, all_vars(. %in% !!input$cluster)) %>%
      select_at(c("cell_id", ident, clusters)) %>%
      collect()

    setDT(metadata, key = "cell_id")

    ## Get marker table.
    markers <- con %>%
      tbl("markers") %>%
      filter(
        cluster  %in% !!input$cluster,
        avg_log2FC > 0
      ) %>%
      collect()

    setDT(markers)
    setorder(markers, cluster, -avg_log2FC)
    markers <- markers[, head(.SD, input$numgenes), by = cluster]

    ## Get gene counts.
    counts <- con %>%
      tbl("counts") %>%
      filter(gene %in% !!unique(markers[["gene"]])) %>%
      collect()

    setDT(counts, key = "cell_id")
    counts <- counts[cell_id %in% metadata[["cell_id"]]]

    ## Merge the marker and metadata data into the counts.
    counts <- merge(counts, metadata)

    ## Get the fraction of cells with expression for each gene and cluster.
    counts[, frac_exp := exp > 0]
    counts <- counts[,
      .(frac_exp = sum(frac_exp) / .N, avg_log2_exp = log2(mean(exp) + 1)),
      by = c(clusters, "gene")
    ]

    ## Make the marker dot-plot.
    p <- ggplot(counts, aes_string(x = "gene", y = clusters)) +
      geom_point(aes(size = frac_exp, color = avg_log2_exp)) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        text = element_text(size = input$fontsize)
      )

    return(p)

  })

  return(marker_dotplot)
  
}
