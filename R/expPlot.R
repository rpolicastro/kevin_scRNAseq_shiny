
#' Expression Plot UI.
#'
#' @inheritParams metadataPlotUI
#'
#' @export

expPlotUI <- function(
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
  ## Expression plot UI.
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
          choices = c("default", "viridis"),
          selected = "default"
        ),
        icon = icon("palette"),
        size = "sm"
      )),
      column(width = 2, dropdownButton(
        headerPanel(""),
        textInput(
          inputId = ns("filename"), label = "File Name",
          value = "expression_plot.png"
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
    textAreaInput(
      inputId = ns("genes"), label = "Genes",
      value = "tdTomato\ntdTomatoStop", rows = 3
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
      inputId = ns("fontsize"), label = "Font Size",
      min = 1, max = 36, value = 18, step = 1
    )
  ),
  mainPanel(width = 10, plotOutput(ns("plot")))
  )

}

#' Expression Plot Server.
#'
#' @inheritParams metadataPlotServer
#'
#' @export

expPlotServer <- function(
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
    counts[, log2_exp := log2(exp + 1)]
    return(counts)
  })

  exp_plot <- reactive({

    ## Merge the metadata back in.
    counts <- merge(md(), cn())

    ## Make the plot.
    p <- ggplot(counts, aes(x = gene, y = log2_exp)) +
      geom_violin(aes(color = gene, fill = gene)) +
      theme(
        text = element_text(size = input$fontsize),
        axis.text.x = element_blank()
      )



    if (input$theme == "minimal") {
        p <- p + theme_minimal()
    } else if (input$theme == "classic") {
        p <- p + theme_classic()
    } else if (input$theme == "grey") {
      p <- p + theme_grey()
    } else if (input$theme == "bw") {
      p <- p + theme_bw()
    }

    if (input$splitby != "none") {
      p <- p + facet_wrap(
        as.formula(str_c("~", input$splitby)),
        ncol = input$ncol
      )
    }

    if (input$palette == "viridis") {
      p <- p + scale_fill_viridis_d()
    }

    return(p)
  })

  ## Output plot.
  output$plot <- renderPlot({exp_plot()}, height = 750)

  ## Save plot.
  output$download <- downloadHandler(
    filename = function() {input$filename},
    content = function(file) {
      ggsave(file, plot = exp_plot(), height = input$height, width = input$width)
    }
  )

})
}
