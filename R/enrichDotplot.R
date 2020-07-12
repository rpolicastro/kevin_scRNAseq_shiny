
#' Enrichment Dot-Plot UI.
#'
#' @inheritParams metadataPlotUI
#'
#' @export

enrichDotplotUI <- function(
  id
) {

  ## Namespace.
  ns <- NS(id)

  sample_sheet <- con %>%
    tbl("samples") %>%
    collect

  experiments <- unique(sample_sheet$experiment)

  sidebarLayout(
  ## Enrichment dot-plot inputs.
  sidebarPanel(width = 3,
    fluidRow(
      column(width = 2, dropdownButton(
        headerPanel(""),
        selectInput(
          inputId = ns("theme"), label = "Theme",
          choices = c("minimal", "classic", "grey", "bw"),
          selected = "minimal"
        ),
        textInput(
          inputId = ns("lowcolor"), label = "Low Color",
          value = "darkblue"
        ),
        textInput(
          inputId = ns("highcolor"), label = "High Color",
          value = "lightblue"
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
    uiOutput(ns("database")),
    uiOutput(ns("clusters")),
    selectInput(
      inputId = ns("direction"), label = "Marker Type",
      choices = c("up", "down"), selected = "up"
    ),
    numericInput(
      inputId = ns("topterms"), label = "Top Terms",
      value = 20, min = 1, max = 50, step = 1
    ),
    numericInput(
      inputId = ns("padjust"), label = "FDR Cutoff",
      value = 0.05, max = 0.05, step = 0.005
    ),
    fluidRow(
      column(width = 6, selectInput(
        inputId = ns("ptcolor"), label = "Point Color",
        choices = c("GeneRatio", "Count", "pvalue", "p.adjust"),
        selected = "p.adjust"
      )),
      column(width = 6, selectInput(
        inputId = ns("ptsize"), label = "Point Size",
        choices = c("GeneRatio", "Count", "pvalue", "p.adjust"),
        selected = "Count"
      ))
    ),
    selectInput(
      inputId = ns("yval"), label = "Axis Value",
      choices = c("GeneRatio", "Count", "pvalue", "p.adjust"),
      selected = "GeneRatio"
    ),
    sliderInput(
      inputId = ns("fontsize"), label = "Font Size",
      min = 1, max = 36, value = 18, step = 1
    )
  ),
  mainPanel(width = 9, plotOutput(ns("plot")))
  )

}

#' Enrichment Dot-Plot Server
#'
#' @importFrom forcats fct_reorder
#'
#' @inheritParams metadataPlotServer
#'
#' @export

enrichDotplotServer <- function(
  id,
  clusters = "seurat_clusters"
) {

moduleServer(id, function(input, output, session) {

  ## Get clusters for each experiment.
  clusts <- reactive({
    clusters <- con %>%
      tbl(str_c(input$experiment, "_metadata")) %>%
      distinct_at(clusters) %>%
      pull(clusters)
    return(clusters)
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

  ## Get available databases.
  dbs <- reactive({
    databases <- con %>%
      tbl(str_c(input$experiment, "_enriched")) %>%
      distinct(database) %>%
      pull(database)
    return(databases)
  })

  # Render available databases.
  output$database <- renderUI({
    ns <- session$ns
    pickerInput(
      inputId = ns("database"), label = "Term Databases",
      choices = dbs(), selected = dbs(),
      multiple = TRUE,
      options = list(
        `actions-box` = TRUE,
        `selected-text-format` = "count > 1")
    )
  })

  ## Make reactive dot-plot.
  enrich_dotplot <- reactive({

     ## Prepare data.
    grps <- str_c("cluster_", input$clusters)

    if (input$direction == "up") {
      grps <- str_c(grps, "_up")
    } else if (input$direction == "down") {
      grps <- str_c(grps, "_down")
    }

    enriched <- con %>%
      tbl(str_c(input$experiment, "_enriched")) %>%
      filter(group %in% grps, database %in% !!input$database) %>%
      select(-geneID, -qvalue, -BgRatio) %>%
      distinct() %>%
      collect()

    setDT(enriched)
    enriched <- enriched[p.adjust < input$padjust]

    enriched[, group := str_replace(group, "cluster_", "")]
    enriched[, c("cluster", "direction") := tstrsplit(group, "_", fixed = TRUE)]
    enriched <- enriched[,
      .(database, cluster, direction, ID, Description,
      Count, GeneRatio, pvalue, p.adjust)
    ]

    enriched[,
      GeneRatio := round(eval(parse(text = GeneRatio)), 3),
      by = seq_len(nrow(enriched))
    ]

    if (input$yval %in% c("p.adjust", "pvalue")) {
      setorderv(enriched, input$yval, order = 1L)
    } else {
      setorderv(enriched, input$yval, order = -1L) 
    }
    enriched <- head(enriched, input$topterms)

    enriched[, Description := str_c(
      Description, str_c("(", database, ")"), str_c("(", cluster, ")"), sep = " "
    )]
    enriched[, Description := fct_reorder(Description, enriched[[input$yval]])]

    ## Generate plot.
    p <- ggplot(enriched, aes_string(
        x = "Description", y = input$yval, color = input$ptcolor,
        size = input$ptsize
      )) +
      geom_point() +
      coord_flip() +
      scale_color_gradient(
        low = input$lowcolor, high = input$highcolor
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

    p <- p + theme(
      text = element_text(size = input$fontsize),
      axis.text.x = element_text(angle = 90, hjust = 1)
    )

    return(p)
  })

  ## Output dot-plot.
  output$plot <- renderPlot({enrich_dotplot()}, height = 750)

  ## Save dot-plot.
  output$download <- downloadHandler(
    filename = function() {input$filename},
    content = function(file) {
      ggsave(file, plot = enrich_dotplot(), height = input$height, width = input$width)
    }
  )

})
}
