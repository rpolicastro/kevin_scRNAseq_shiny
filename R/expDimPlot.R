
#' Expression Dim Plot UI
#'
#' @inheritParams metadataPlotInput
#'
#' @export

expDimPlotInput <- function(
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
    ),
    searchInput(
      inputId = ns("gene"), label = "Gene", value = "tdTomato",
      btnSearch = icon("search"), btnReset = icon("remove")
    ),
    selectInput(
      inputId = ns("splitby"), label = "Split By",
      choices = c("none", ident, clusters),
      selected = "none"
    ),
    selectInput(
      inputId = ns("palette"), label = "Color Palette",
      choices = c("darkblue"), selected = "darkblue"
    ),
    numericInput(
      inputId = ns("ncol"), label = "Number of Columns",
      value = 2, min = 1, max = 25, step = 1
    ),
    sliderInput(
      inputId = ns("ptsize"), label = "Point Size",
      min = 0.25, max = 5, value = 0.75, step = 0.25
    ),
    sliderInput(
      inputId = ns("fontsize"), label = "Font Size",
      min = 1, max = 36, value = 18, step = 1
    )
  )

}

#' Expression Dim Plot Server
#'
#' @inheritParams metadataPlot
#'
#' @export

expDimPlot <- function(
  input, output, session,
  ident = "orig.ident",
  clusters = "seurat_clusters"
) {

  exp_plot <- reactive({

    ## Get meta-data.
    metadata <- con %>%
      tbl("metadata") %>%
      filter_at(ident, all_vars(. %in% !!input$sample)) %>%
      filter_at(clusters, all_vars(. %in% !!input$cluster)) %>%
      select_at(c("cell_id", ident, clusters)) %>%
      collect()

    setDT(metadata, key = "cell_id")

    ## Get UMAP coordinates.
    umap <- con %>%
      tbl("reductions") %>%
      filter_at("cell_id", all_vars(. %in% !!metadata[["cell_id"]])) %>%
      collect()

    setDT(umap, key = "cell_id")

    ## Get expression data.
    counts <- con %>%
      tbl("counts") %>%
      filter(gene == !!input$gene) %>%
      collect()

    setDT(counts, key = "cell_id")
    counts <- counts[cell_id %in% metadata[["cell_id"]]]
    counts[, log2_exp := log2(exp + 1)]

    ## Merge all of the data together.
    counts <- merge(metadata, counts)
    counts <- merge(counts, umap)

    ## Make plot.
    p <- ggplot(counts, aes(x = UMAP_1, y = UMAP_2)) +
      geom_point(aes(color = log2_exp), size = input$ptsize) +
      theme_minimal() +
      theme(text = element_text(size = input$fontsize))

    if (input$splitby != "none") {
      p <- p + facet_wrap(
        as.formula(str_c("~", input$splitby)),
        ncol = input$ncol
      )
    }

    if (input$palette == "darkblue") {
      p <- p + scale_color_gradient(low = "#e6e6e6", high = "darkblue")
    }

    return(p)

  })

  return(exp_plot)

}
