
#' Expression Plot UI.
#'
#' @inheritParams metadataPlotInput
#'
#' @export

expPlotInput <- function(
  id,
  ident = "orig.ident",
  clusters = "seurat_clusters"
) {

  ## Namespace.
  ns <- NS(id)

  ## Get sample choices.
  sample_choices <- con %>%
    tbl("metadata") %>%
    distinct_at(ident) %>%
    pull(ident)

  ## Get cluster choices.
  cluster_choices <- con %>%
    tbl("metadata") %>%
    distinct_at(clusters) %>%
    pull(clusters)

  ## Expression plot UI.
  sidebarPanel(width = 2,
    pickerInput(
      inputId = ns("sample"), label = "Samples",
      choices = sample_choices, selected = sample_choices,
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
        `selected-text-format` = "count > 1")
    ),
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
  )

}

#' Expression Plot Server.
#'
#' @inheritParams metadataPlotInput
#'
#' @export

expPlot <- function(
  input, output, session,
  ident = "orig.ident",
  clusters = "seurat_clusters"
) {

  exp_plot <- reactive({

    ## Get metadata.
      metadata <- con %>%
        tbl("metadata") %>%
        filter_at(ident, all_vars(. %in% !!input$sample)) %>%
        filter_at(clusters, all_vars(. %in% !!input$cluster)) %>%
        select_at(c("cell_id", ident, clusters)) %>%
        collect()

      setDT(metadata, key = "cell_id")

    ## Get expression values for gene.
    genes <- str_split(input$genes, "\\s", simplify = TRUE)[1, ]
    validate(
      need(length(genes) <= 10, "Can only display 10 genes or less.")
    )

    counts <- con %>%
      tbl("counts") %>%
      filter(gene %in% genes) %>%
      collect()

    setDT(counts, key = "cell_id")
    counts <- counts[cell_id %in% metadata[["cell_id"]]]
    counts[, log2_exp := log2(exp + 1)]

    ## Merge the metadata back in.
    counts <- merge(metadata, counts)

    ## Make the plot.
    p <- ggplot(counts, aes(x = gene, y = log2_exp)) +
      geom_violin(aes(color = gene, fill = gene)) +
      theme_minimal() +
      theme(
        text = element_text(size = input$fontsize),
        axis.text.x = element_blank()
      )

    if (input$splitby != "none") {
      p <- p + facet_wrap(
        as.formula(str_c("~", input$splitby)),
        ncol = input$ncol
      )
    }

    return(p)

  })

  return(exp_plot)

}
