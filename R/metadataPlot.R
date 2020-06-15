
#' Meta-Data Plot Input UI
#'
#' @param id Input id for use in server functions.
#' @param ident Sample identity column name.
#' @param clusters Cluster column name.
#' @param nCount SCT UMI counts column name.
#' @param nFeature SCT feature counts column name.
#' @param percentMT Cell mitochondrial percentage column name.
#'
#' @export

metadataPlotInput <- function(
  id,
  ident = "orig.ident",
  clusters = "seurat_clusters",
  nCount = "nCount_SCT",
  nFeature = "nFeature_SCT",
  percentMT = "percent.mt"
) {

  ## Namespace function using ID.
  ns <- NS(id)

  ## Get sample choices.
  sample_choices <- con %>%
    tbl("metadata") %>%
    distinct_at(ident) %>%
    pull(ident)

  ## Sidebar panel of inputs.
  sidebarPanel(width = 2,
    checkboxGroupInput(
      inputId = ns("sample"), label = "Sample",
      choices = sample_choices,
      selected = sample_choices
    ),
    selectInput(
      inputId = ns("colorby"), label = "Color By",
      choices = c("none", ident, clusters, nCount, nFeature, percentMT),
      selected = clusters
    ),
    selectInput(
      inputId = ns("splitby"), label = "Split By",
      choices = c("none", ident, clusters),
      selected = "none"
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

#' Meta_data Plot Input Server
#'
#' @param input input
#' @param output output
#' @param session session
#' @inheritParams metadataPlotInput
#'
#' @export

metadataPlot <- function(
  input, output, session,
  ident = "orig.ident",
  clusters = "seurat_clusters",
  nCount = "nCount_SCT",
  nFeature = "nFeature_SCT",
  percentMT = "percent.mt"
) {

  ## Make the meta-data dim plot.
  dim_plot <- reactive({

    # Grab meta-data from database.
    metadata <- con %>%
      tbl("metadata") %>%
      filter_at(ident, all_vars(. %in% !!input$sample)) %>%
      select_at(c("cell_id", ident, clusters, nCount, nFeature, percentMT)) %>%
      collect()

    setDT(metadata, key = "cell_id")

    # Get the UMAP dimension.
    umap <- con %>%
      tbl("reductions") %>%
      filter(cell_id %in% !!metadata[["cell_id"]]) %>%
      collect()

    setDT(metadata, key = "cell_id")

    # Add the UMAP dimensions to the meta-data.
    metadata <- merge(metadata, umap)

    p <- ggplot(metadata, aes(x = UMAP_1, y = UMAP_2))

    if (input$colorby != "none")  {
      p <- p + geom_point(aes_string(color = input$colorby), size = input$ptsize)
    } else {
      p <- p + geom_point(size = input$ptsize)
    }

    p <- p +
      theme_minimal() +
      theme(text = element_text(size = input$fontsize))

    if (input$splitby != "none") {
      p <- p + facet_wrap(
        as.formula(str_c("~", input$splitby)),
        ncol = input$ncol
      ) 
    }   
    
    return(p)

  })
  
  return(dim_plot)

}
