
#' Enrichment Dot-Plot UI.
#'
#' @inheritParams metadataPlotInput
#'
#' @export

enrichDotplotInput <- function(id) {

  ## Namespace.
  ns <- NS(id)

  ## Get database choices.
  database_choices <- con %>%
    tbl("enriched") %>%
    distinct(database) %>%
    pull(database)

  ## Get groups.
  group_choices <- con %>%
    tbl("enriched") %>%
    distinct(group) %>%
    pull(group)

  ## Get cluster choices.
  cluster_choices <- group_choices %>%
    str_extract("^cluster_[A-Za-z0-9]+") %>%
    str_replace("cluster_", "") %>%
    unique

  ## Enrichment dot-plot inputs.
  sidebarPanel(width = 3,
    pickerInput(
      inputId = ns("database"), label = "Term Databases",
      choices = database_choices, selected = database_choices,
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
        `selected-text-format` = "count > 1"
      )
    ),
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
    ),
    fluidRow(
      column(width = 6, textInput(
        inputId = ns("lowcolor"), label = "Low Color",
        value = "darkblue"
      )),
      column(width = 6, textInput(
        inputId = ns("highcolor"), label = "High Color",
        value = "lightblue"
      ))
    )
  )

}

#' Enrichment Dot-Plot Server
#'
#' @importFrom forcats fct_reorder
#'
#' @inheritParams metadataPlot
#'
#' @export

enrichDotplot <- function(input, output, session) {

  ## Make reactive dot-plot.
  enrich_dotplot <- reactive({

     ## Prepare data.
    grps <- str_c("cluster_", input$cluster)

    if (input$direction == "up") {
      grps <- str_c(grps, "_up")
    } else if (input$direction == "down") {
      grps <- str_c(grps, "_down")
    }

    enriched <- con %>%
      tbl("enriched") %>%
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
      theme_minimal() +
      theme(
        text = element_text(size = input$fontsize),
        axis.text.x = element_text(angle = 90, hjust = 1)
      ) +
      scale_color_gradient(
        low = input$lowcolor, high = input$highcolor
      )

    return(p)

  })

  return(enrich_dotplot)

}
