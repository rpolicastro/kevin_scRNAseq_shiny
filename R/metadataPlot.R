
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

  cluster_choices <- con %>%
    tbl("metadata") %>%
    distinct_at(clusters) %>%
    pull(clusters)

  ## Sidebar panel of inputs.
  sidebarPanel(width = 2,
    fluidRow(
      column(width = 2, dropdownButton(
        selectInput(
          inputId = ns("theme"), label = "Theme",
          choices = c("minimal", "classic", "grey", "bw"),
          selected = "minimal"
        ),
        selectInput(
          inputId = ns("palette"), label = "Palette",
          choices = c("default", "viridis"),
          selected = "default"
        ),
        icon = icon("palette"),
        size = "sm"
      )),
      column(width = 2, dropdownButton(
        textInput(
          inputId = ns("filename"), label = "File Name",
          value = "metadata_dimplot.png"
        ),
        fluidRow(
          column(width = 6, numericInput(
            inputId = ns("height"), label = "Height",
            value = 8, min = 1, max = 36, step = 0.5
          )),
          column(width = 6, numericInput(
            inputId = ns("width"), label = "Width",
            value = 12, min = 1, max = 36, step = 0.5
          ))
        ),
        downloadButton(
          outputId = "download", label = "Download"
        ),
        icon = icon("save"),
        size = "sm",
        width = "300px"
      ))
    ),
    pickerInput(
      inputId = ns("sample"), label = "Samples",
      choices = sample_choices, selected = sample_choices,
      multiple = TRUE,
      options = list(
        `actions-box` = TRUE,
        `selected-text-format` = "count > 1"
      )
    ),
    pickerInput(
      inputId = ns("cluster"), label = "Clusters",
      choices = cluster_choices, selected = cluster_choices,
      multiple = TRUE,
      options = list(
        `actions-box` = TRUE,
        `selected-text-format` = "count > 1")
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
    conditionalPanel(
      condition = "input.splitby != 'none'", ns = ns,
      numericInput(
        inputId = ns("ncol"), label = "Number of Columns",
        value = 2, min = 1, max = 25, step = 1
      )
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
      filter_at(clusters, all_vars(. %in% !!input$cluster)) %>%
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

    if (input$theme == "minimal") {
      p <- p + theme_minimal()
    } else if (input$theme == "classic") {
      p <- p + theme_classic()
    } else if (input$theme == "grey") {
      p <- p + theme_grey()
    } else if (input$theme == "bw") {
      p <- p + theme_bw()
    }

    p <- p + theme(text = element_text(size = input$fontsize))

    if (input$palette == "viridis") {
      p <- p + scale_color_viridis_d()
    }

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
