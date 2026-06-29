# modules/mod_fama_cluster_panels.R

mod_fama_cluster_panels_ui <- function(id) {
  ns <- NS(id)

  div(
    style = "
      border:2px solid #D4D4D4;
      border-radius:10px;
      padding:15px;
      background-color:#FAFAFA;
      height:100%;
    ",
    div(
      style = "
        font-size:20px;
        font-weight:600;
        padding-left:10px;
        border-left:5px solid #3F51B5;
        margin-bottom:15px;
      ",
      "Cluster snapshot"
    ),
    div(
      style = "margin:6px 0 12px; font-size:13px; color:#444;",
      "Click a cluster in the graph to see its metadata."
    ),
    tabsetPanel(
      tabPanel(
        "Table",
        shinycssloaders::withSpinner(DTOutput(ns("cluster_info")))
      ),
      tabPanel(
        "TF-IDF",
        div(
          style = "margin:6px 0 12px; font-size:13px; color:#444;",
          "Top TF-IDF terms from titles in the selected cluster."
        ),
        shinycssloaders::withSpinner(DTOutput(ns("tfidf")))
      ),
      tabPanel(
        "Top journals",
        div(
          style = "margin:6px 0 12px; font-size:13px; color:#444;",
          "Frequency and TF-IDF."
        ),
        shinycssloaders::withSpinner(DTOutput(ns("top_journal")))
      ),
      tabPanel(
        "Time distribution",
        div(
          style = "margin:6px 0 12px; font-size:13px; color:#444;",
          "Cluster evolution over time."
        ),
        shinycssloaders::withSpinner(plotOutput(
          ns("nodes_distribution"),
          height = "140px"
        )),
        tags$hr(),
        shinycssloaders::withSpinner(plotOutput(
          ns("cluster_distribution"),
          height = "140px"
        ))
      )
    )
  )
}

mod_fama_cluster_panels_server <- function(id, view_state, corpus) {
  moduleServer(id, function(input, output, session) {
    selected_cluster <- view_state$selected_cluster
    selected_year <- view_state$selected_year
    is_single_graph <- view_state$is_single_graph
    is_single_graph_val <- reactive({
      if (shiny::is.reactive(is_single_graph)) {
        is_single_graph()
      } else {
        is_single_graph
      }
    })
    corpus_val <- reactive({
      if (shiny::is.reactive(corpus)) {
        req(corpus())
        corpus()
      } else {
        corpus
      }
    })

    cluster_data <- reactive({
      req(selected_cluster())
      corpus_val() %>%
        filter(
          !!if (!is_single_graph_val()) {
            corpus_val()$time_window == selected_year()
          } else {
            TRUE
          },
          dynamic_cluster_leiden == selected_cluster()
        ) %>%
        select(any_of(c(
          "first_author",
          "year",
          "title",
          "journal",
          "n_citations",
          "role_ga",
          "dynamic_cluster_leiden"
        ))) %>%
        unique() %>%
        arrange(desc(n_citations))
    })

    output$cluster_info <- renderDT({
      req(cluster_data())
      table_data <- cluster_data() %>% select(-dynamic_cluster_leiden)
      datatable(
        table_data,
        options = list(pageLength = 10)
      ) %>%
        formatStyle(columns = names(table_data), fontSize = "10px")
    })

    output$cluster_distribution <- renderPlot({
      req(cluster_data())
      cluster_id <- unique(cluster_data()$dynamic_cluster_leiden)

      if (is_single_graph_val()) {
        return(NULL)
      }

      corpus_val() %>%
        filter(dynamic_cluster_leiden %in% cluster_id) %>%
        count(dynamic_cluster_leiden, time_window) %>%
        ggplot(aes(x = time_window, y = n)) +
        geom_col(fill = "steelblue") +
        theme_minimal() +
        labs(x = "Time Window", y = "Documents") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    })

    output$nodes_distribution <- renderPlot({
      req(cluster_data())
      cluster_id <- unique(cluster_data()$dynamic_cluster_leiden)

      corpus_val() %>%
        filter(dynamic_cluster_leiden %in% cluster_id) %>%
        count(year, dynamic_cluster_leiden) %>%
        ggplot(aes(x = as.integer(year), y = n)) +
        geom_col(fill = "steelblue") +
        theme_minimal() +
        labs(x = "Year", y = "Documents") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    })

    output$tfidf <- renderDT({
      req(cluster_data())
      cluster_id <- unique(cluster_data()$dynamic_cluster_leiden)

      titles_df <- corpus_val() %>%
        filter(
          !!if (!is_single_graph_val()) {
            corpus_val()$time_window == selected_year()
          } else {
            TRUE
          }
        ) %>%
        select(source_id, title, dynamic_cluster_leiden)

      tidy_titles <- titles_df %>%
        unnest_tokens(word, title) %>%
        anti_join(stop_words, by = "word")

      tfidf <- tidy_titles %>%
        count(dynamic_cluster_leiden, word, sort = TRUE) %>%
        bind_tf_idf(word, dynamic_cluster_leiden, n)

      table_data <- tfidf %>%
        filter(dynamic_cluster_leiden %in% cluster_id) %>%
        slice_max(tf_idf, n = 10, with_ties = FALSE) %>%
        select(word, n, tf_idf)

      datatable(
        table_data,
        options = list(
          pageLength = 10,
          lengthChange = FALSE,
          searching = FALSE
        )
      ) %>%
        formatStyle(columns = names(table_data), fontSize = "10px")
    })

    output$top_journal <- renderDT({
      req(cluster_data())
      cluster_id <- unique(cluster_data()$dynamic_cluster_leiden)

      table_data <- corpus_val() %>%
        filter(
          !!if (!is_single_graph_val()) {
            corpus_val()$time_window == selected_year()
          } else {
            TRUE
          }
        ) %>%
        group_by(dynamic_cluster_leiden) %>%
        count(journal, dynamic_cluster_leiden) %>%
        group_by(dynamic_cluster_leiden) %>%
        bind_tf_idf(journal, dynamic_cluster_leiden, n) %>%
        arrange(desc(n)) %>%
        filter(dynamic_cluster_leiden %in% cluster_id) %>%
        ungroup %>%
        select(journal, n, tf_idf)

      datatable(
        table_data,
        options = list(
          pageLength = 10,
          lengthChange = FALSE,
          searching = FALSE
        )
      ) %>%
        formatStyle(columns = names(table_data), fontSize = "10px")
    })
  })
}
