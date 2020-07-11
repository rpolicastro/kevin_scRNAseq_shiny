
#' Expression Dim Plot UI
#'
#' @inheritParams metadataPlotUI
#'
#' @export

expDimPlotUI <- function(
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
          choices = c("darkblue", "darkred", "darkgreen", "viridis"),
          selected = "darkblue"
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
    searchInput(
      inputId = ns("gene"), label = "Gene", value = "tdTomato",
      btnSearch = icon("search"), btnReset = icon("remove")
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
  ),
  mainPanel(width = 10, plotOutput(ns("plot")))
  )

}

#' Expression Dim Plot Server
#'
#' @importFrom stringr str_starts
#'
#' @inheritParams metadataPlotServer
#'
#' @export

expDimPlotServer <- function(
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

  ## Get the UMAP embeddings.
  um <- reactive({
    umap <- con %>%
      tbl(str_c(input$experiment, "_reductions")) %>%
      filter(cell_id %in% !!md()[["cell_id"]]) %>%
      collect()

    setDT(umap, key = "cell_id")
    return(umap)
  })

  ## Get the gene counts.
  cn <- reactive({
    counts <- con %>%
      tbl(str_c(input$experiment, "_counts")) %>%
      filter(gene == !!input$gene) %>%
      collect()

    setDT(counts, key = "cell_id")
    counts <- counts[cell_id %in% md()[["cell_id"]]]
    counts[, log2_exp := log2(exp + 1)]
    return(counts)
  })

  exp_plot <- reactive({
    
    ## Merge all of the data together.
    counts <- merge(md(), cn())
    counts <- merge(counts, um())

    ## Make plot.
    p <- ggplot(counts, aes(x = UMAP_1, y = UMAP_2)) +
      geom_point(aes(color = log2_exp), size = input$ptsize)

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

    if (input$splitby != "none") {
      p <- p + facet_wrap(
        as.formula(str_c("~", input$splitby)),
        ncol = input$ncol
      )
    }

    if (str_starts(input$palette, "dark")) {
      p <- p + scale_color_gradient(low = "#e6e6e6", high = input$palette)
    } else if (input$palette == "viridis") {
      p <- p + scale_color_viridis_c()
    }

    return(p)

  })

  ## Output the plot.
  output$plot <- renderPlot({exp_plot()}, height = 750)

  ## Save the plot.
  output$download <- downloadHandler(
    filename = function() {input$filename},
    content = function(file) {
      ggsave(file, plot = exp_plot(), height = input$height, width = input$width)
    }
  )

})
}
