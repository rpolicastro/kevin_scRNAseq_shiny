
#' Marker Table UI
#'
#' @param id id
#'
#' @export

markerTableInput <- function(id) {

  ## Namespace
  ns <- NS(id)

  ## Get cluster names.
  cluster_choices <- con %>%
    tbl("markers") %>%
    distinct(cluster) %>%
    pull(cluster)

  ## Marker table UI.
  sidebarPanel( width = 2,
    pickerInput(
      inputId = ns("cluster"), label = "Clusters",
      choices = cluster_choices, selected = cluster_choices,
      multiple = TRUE,
      options = list(
        `actions-box` = TRUE,
        `selected-text-format` = "count > 1")
    ),
    fluidRow(
      column(6, numericInput(
        inputId = ns("fdrcutoff"), label = "FDR Cutoff",
        value = 0.05, min = 0, max = 1, step = 0.005
      )),
      column(6, numericInput(
        inputId = ns("fccutoff"), label = "FC Cutoff",
        value = 1.5, min = 1.5, step = 0.1
      ))
    )
  )

}

#' Marker Table Server
#'
#' @inheritParams metadataPlot
#'
#' @export

markerTable <- function(input, output, session) {

  ## Retrieve table.
  marker_table <- reactive({

    # Get table from database.
    fc_cutoff <- log2(input$fccutoff)

    markers <- con %>%
      tbl("markers") %>%
      filter(
        cluster %in% !!input$cluster,
        p_val_adj < !!input$fdrcutoff,
        abs(avg_log2FC) > fc_cutoff
      ) %>%
      select(-avg_logFC) %>%
      collect()

    setDT(markers)

    return(markers)

  })

  return(marker_table)

}
