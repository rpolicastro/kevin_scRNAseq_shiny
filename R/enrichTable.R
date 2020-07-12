
#' Term Enrichment Table UI
#'
#' @inheritParams metadataTableUI
#'
#' @export

enrichTableUI <- function(
  id
) {

  ## Namespace.
  ns <- NS(id)

  ## Get sample choices.
  sample_sheet <- con %>%
    tbl("samples") %>%
    collect

  experiments <- unique(sample_sheet$experiment)

  sidebarLayout(
  ## Enrichment table UI.
  sidebarPanel(width = 2,
    selectInput(
      inputId = ns("experiment"), label = "Experiment",
      choices = experiments,
      selected = experiments[1]
    ),
    uiOutput(ns("database")),
    uiOutput(ns("clusters")),
    checkboxGroupInput(
      inputId = ns("direction"), label = "Marker Type",
      choices = c("up", "down"), selected = c("up", "down"),
      inline = TRUE
    ),
    numericInput(
      inputId = ns("padjust"), label = "FDR Cutoff",
      value = 0.05, max = 0.05, step = 0.005
    )
  ),
  mainPanel(width = 10, DT::dataTableOutput(ns("table")))
  )

}

#' Enrichment Table Server
#'
#' @inheritParams metadataPlotServer
#'
#' @export

enrichTableServer <- function(
  id,
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

  ## Get enrichment data.
  enrich_table <- reactive({
    validate(
      need(length(input$direction) > 0, "A direction must be selected")
    )

    grps <- str_c("cluster_", input$clusters)

    if (all(input$direction == "up")) {
      grps <- str_c(grps, "_up")
    } else if (all(input$direction == "down")) {
      grps <- str_c(grps, "_down")
    } else {
      grps <- c(str_c(grps, "_up"), str_c(grps, "_down"))
    }

    enriched <- con %>%
      tbl(str_c(input$experiment, "_enriched")) %>%
      filter(
        group %in% grps,
        database %in% !!input$database
      ) %>%
      select(-geneID, -qvalue, -BgRatio) %>%
      collect()

    setDT(enriched)
    enriched <- enriched[p.adjust < input$padjust]
    enriched[, group := str_replace(group, "cluster_", "")]
    enriched[, c("cluster", "direction") := tstrsplit(group, "_", fixed = TRUE)]
    enriched <- enriched[,
      .(database, cluster, direction, ID, Description,
      Count, GeneRatio, pvalue, p.adjust)
    ]

    return(enriched)

  }) 

  ## Output the table.
  output$table <- DT::renderDataTable(
    {enrich_table()},
    extensions = "Buttons",
    options = list(
      order = list(list(2, "desc"), list(9, "asc")),
      dom = "Bfrtpli",
      buttons = c('copy', 'csv', 'excel', 'print')
    )
  )

})
}
