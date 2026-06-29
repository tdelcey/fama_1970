# modules/mod_fama_network.R

modules_fama_network_ui <- function(id, mode = c("static", "dynamic")) {
  ns <- NS(id)
  mode <- match.arg(mode)

  fluidRow(
    column(6, mod_fama_network_view_ui(ns("view"), mode = mode)),
    column(6, class = "right-column", mod_fama_cluster_panels_ui(ns("panels")))
  )
}

modules_fama_network_server <- function(id, list_ggraph, corpus) {
  moduleServer(id, function(input, output, session) {
    view_state <- mod_fama_network_view_server(
      "view",
      list_ggraph = list_ggraph
    )

    mod_fama_cluster_panels_server(
      "panels",
      view_state = view_state,
      corpus = corpus
    )
  })
}
