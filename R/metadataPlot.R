
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

metadataPlotUI <- function(
  id,
  ident = "orig.ident",
  clusters = "seurat_clusters"
) {

  ## Namespace function using ID.
  ns <- NS(id)

  ## Get sample choices.
  sample_sheet <- con %>%
    tbl("samples") %>%
    collect

  experiments <- unique(sample_sheet$experiment)
  samples <- sample_sheet$samples

  sidebarLayout(
  ## Sidebar panel of inputs.
  sidebarPanel(width = 2,
    fluidRow(
      column(width = 2, dropdownButton(
        headerPanel(""),
        selectInput(
          inputId = ns("theme"), label = "Theme",
          choices = c("minimal", "classic", "grey", "bw"),
          selected = "minimal"
        ),
        selectInput(
          inputId = ns("palette"), label = "Palette",
          choices = c("Default", "Viridis", "Blues", "Reds", "Greens"),
          selected = "Default"
        ),
        checkboxInput(
          inputId = ns("invert"), label = "Invert Colors",
          value = FALSE
        ),
        icon = icon("palette"),
        size = "sm"
      )),
      column(width = 2, dropdownButton(
        headerPanel(""),
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
          outputId = ns("download"), label = "Download"
        ),
        headerPanel(""),
        icon = icon("save"),
        size = "sm",
        width = "300px"
      ))
    ),
    selectInput(
      inputId = ns("experiment"), label = "Experiment",
      choices = experiments,
      selected = experiments[1]
    ),
    uiOutput(ns("samples")),
    uiOutput(ns("clusters")),
    uiOutput(ns("colorby")),
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
  ),

  ## Main output panel.
  mainPanel(width = 10, plotOutput(ns("plot")))
  )
}

#' Meta_data Plot Input Server
#'
#' @inheritParams metadataPlotUI
#'
#' @export

metadataPlotServer <- function(
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
      collect()

    setDT(metadata, key = "cell_id")
    return(metadata)
  })

  ## Get the column names of the metadata.
  cols <- reactive({
    cols <- colnames(md())
    cols <- cols[cols != "cell_id"]
    return(cols)
  })

  ## Render the color-by choices.
  output$colorby <- renderUI({
    ns <- session$ns
    selectInput(
      inputId = ns("colorby"), label = "Color By",
      choices = c("none", cols()),
      selected = clusters
    )
  })


  ## Get the UMAP embeddings.
  um <- reactive({
    umap <- con %>%
      tbl(str_c(input$experiment, "_reductions")) %>%
      filter(cell_id %in% !!md()[["cell_id"]]) %>%
      collect()

    setDT(umap, key = "cell_id")
    return(umap)
  })


  ## Make the meta-data dim plot.
  dim_plot <- reactive({

    # Add the UMAP dimensions to the meta-data.
    metadata <- merge(md(), um())

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

    direction <- ifelse((input$invert), 1, -1)
    if (input$palette != "Default" && is(metadata[[input$colorby]], "numeric")) {
      if (input$palette == "Viridis") p <- p + scale_color_viridis_c(direction = direction)
      if (input$palette %in% c("Blues", "Reds", "Greens")) {
        p <- p + scale_color_distiller(palette = input$palette, direction = direction)
      }
    } else {
      if (input$palette == "Viridis") p <- p + scale_color_viridis_d(direction = direction)
      if (input$palette %in% c("Blues", "Reds", "Greens")) {
        p <- p + scale_color_brewer(palette = input$palette, direction = direction)
      }
    }

    if (input$splitby != "none") {
      p <- p + facet_wrap(
        as.formula(str_c("~", input$splitby)),
        ncol = input$ncol
      ) 
    }   
    
    return(p)
  })

  ## Output the meta-data dimplot.
  output$plot <- renderPlot({dim_plot()}, height = 750)

  ## Save the plot.
  output$download <- downloadHandler(
    filename = function() {input$filename},
    content = function(file) {
      ggsave(file, plot = dim_plot(), height = input$height, width = input$width)
    }
  )
  
})
}
