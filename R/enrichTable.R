
#' Term Enrichment Table UI
#'
#'
#' @export

enrichTableInput <- function(id) {

  ## Namespace.
  ns <- NS(id)

  ## Get databases.
  database_choices <- con %>%
    tbl("enriched") %>%
    distinct(database) %>%
    pull(database)

  ## Get groups.
  group_choices <- con %>%
    tbl("enriched") %>%
    distinct(group) %>%
    pull(group)

  cluster_choices <- group_choices %>%
    str_extract("^cluster_[A-Za-z0-9]+") %>%
    str_replace("cluster_", "") %>%
    unique

  ## Enrichment table UI.
  sidebarPanel(width = 2,
    pickerInput(
      inputId = ns("database"), label = "Term Databases",
      choices = database_choices, selected = database_choices,
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
        `selected-text-format` = "count > 1"
      )
    ),
    checkboxGroupInput(
      inputId = ns("direction"), label = "Marker Type",
      choices = c("up", "down"), selected = c("up", "down"),
      inline = TRUE
    ),
    numericInput(
      inputId = ns("padjust"), label = "FDR Cutoff",
      value = 0.05, max = 0.05, step = 0.005
    )
  )


}

#' Enrichment Table Server
#'
#' @inheritParams metadataPlot
#'
#' @export

enrichTable <- function(input, output, session) {

  ## Get enrichment data.
  enrich_table <- reactive({
    validate(
      need(length(input$direction) > 0, "A direction must be selected")
    )

    grps <- str_c("cluster_", input$cluster)

    if (all(input$direction == "up")) {
      grps <- str_c(grps, "_up")
    } else if (all(input$direction == "down")) {
      grps <- str_c(grps, "_down")
    } else {
      grps <- c(str_c(grps, "_up"), str_c(grps, "_down"))
    }

    enriched <- con %>%
      tbl("enriched") %>%
      filter(group %in% grps, database %in% !!input$database) %>%
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

  return(enrich_table)

}
