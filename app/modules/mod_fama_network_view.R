# modules/mod_fama_network_view.R

mod_fama_network_view_ui <- function(id, mode = c("static", "dynamic")) {
  ns <- NS(id)
  mode <- match.arg(mode)

  shared_text <- paste(
    "Each node is a document citing Fama (1970); edges capture statistically",
    "significant bibliographic coupling (shared references).",
    "Colors indicate clusters of documents.",
    "See the paper for the detailed methodology."
  )

  interpretation_text <- if (mode == "dynamic") {
    paste(
      shared_text,
      "This dynamic view shows successive overlapping time windows, so you can",
      "track how communities emerge, persist, or split across periods."
    )
  } else {
    paste(
      shared_text,
      "This static view covers the full period (1970-2010) in a single network."
    )
  }

  div(
    style = "
      border:2px solid #D4D4D4;
      border-radius:10px;
      padding:15px;
      background-color:#FAFAFA;
      height:100%;
    ",
    callout_box(
      title = "Interpretation",
      icon = "\U0001F4DA",
      text = interpretation_text
    ),
    uiOutput(ns("graph_selector")),
    div(
      style = "
        border:1px solid #D0D0D0;
        border-radius:8px;
        padding:2px;
        background:#FFF;
        margin-top:10px;
      ",
      shinycssloaders::withSpinner(
        girafeOutput(ns("network"), height = "640px"),
        type = 4,
        color = "#607D8B"
      )
    )
  )
}

mod_fama_network_view_server <- function(id, list_ggraph) {
  moduleServer(id, function(input, output, session) {
    lg <- reactive({
      if (shiny::is.reactive(list_ggraph)) {
        list_ggraph()
      } else {
        list_ggraph
      }
    })

    is_single_graph <- reactive(inherits(lg(), "girafe"))
    has_named_list <- reactive(is.list(lg()) && !is.null(names(lg())))

    output$graph_selector <- renderUI({
      if (is_single_graph()) {
        return(NULL)
      }
      choices <- if (has_named_list()) {
        names(lg())
      } else {
        as.character(seq_along(lg()))
      }
      shinyWidgets::sliderTextInput(
        inputId = session$ns("year"),
        label = "Select Year:",
        choices = choices,
        selected = choices[1],
        width = "100%"
      )
    })

    selected_year <- reactive({
      if (is_single_graph()) {
        return(NULL)
      }
      input$year
    })

    selected_graph <- reactive({
      if (is_single_graph()) {
        return(NULL)
      }
      req(selected_year())
      if (has_named_list()) {
        selected_year()
      } else {
        as.integer(selected_year())
      }
    })

    output$network <- renderGirafe({
      gg <- if (is_single_graph()) {
        lg()
      } else {
        lg()[[selected_graph()]]
      }
      girafe_options(gg, opts_selection(type = "single"))
    })

    outputOptions(output, "network", suspendWhenHidden = FALSE)

    selected_cluster <- reactiveVal(NULL)
    observeEvent(input$network_selected, {
      if (!is.null(input$network_selected)) {
        selected_cluster(input$network_selected)
      }
    })

    list(
      selected_cluster = selected_cluster,
      selected_year = selected_year,
      selected_graph = selected_graph,
      is_single_graph = is_single_graph
    )
  })
}
