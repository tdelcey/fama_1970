#' To manage dependencies, run
#' `renv::init()` # first time
#' `renv::snapshot()` # to update the renv.lock

# Dependencies
library(shiny)
library(tidyverse)
library(tidytext)
library(DT)
library(shinyWidgets)
library(ggiraph)
library(here)
library(ggwordcloud)
library(shinycssloaders)

# ---- Modules ----

local_modules <- here::here("app", "modules")
deployed_modules <- here::here("modules")
modules_dir <- if (dir.exists(local_modules)) {
  local_modules
} else {
  deployed_modules
}

list_files_modules <- list.files(
  modules_dir,
  pattern = "*.R",
  full.names = TRUE,
  recursive = TRUE
)


invisible(lapply(list_files_modules, source))

# ---- Load Data ----

# Load the data (local dev vs deployed)
resolve_app_data <- function(path) {
  local_path <- here("app", "data", path)
  deployed_path <- here("data", path)
  if (file.exists(local_path)) local_path else deployed_path
}

list_ggraph_all <- readRDS(resolve_app_data("graph_all_period.rds"))
selected_graphs_15 <- readRDS(resolve_app_data("selected_graphs_15.rds"))


list_ggraph_10 <- readRDS(resolve_app_data("list_ggraph_10.rds"))
list_ggraph_15 <- readRDS(resolve_app_data("list_ggraph_15.rds"))
list_ggraph_20 <- readRDS(resolve_app_data("list_ggraph_20.rds"))

corpus_all <- readRDS(resolve_app_data("corpus_all_periods.rds")) %>%
  rename(n_citations = n)
corpus_10 <- readRDS(resolve_app_data("corpus_10.rds")) %>%
  rename(n_citations = n)
corpus_15 <- readRDS(resolve_app_data("corpus_15.rds")) %>%
  rename(n_citations = n)
corpus_20 <- readRDS(resolve_app_data("corpus_20.rds")) %>%
  rename(n_citations = n)


# --- ui ----

# UI
ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "app.css")
  ),
  div(
    class = "app-shell",
    div(
      style = "text-align:center; margin-bottom:30px; margin-top:10px;",
      div(
        style = "font-size:32px; font-weight:700; margin-bottom:6px;",
        "The dissemination of Fama (1970)"
      ),
      div(
        style = "font-size:20px; color:#555; font-weight:400;",
        "A bibliometric analysis"
      )
    ),
    tabsetPanel(
      tabPanel(
        "Static analysis",
        modules_fama_network_ui("all", mode = "static")
      ),
      tabPanel(
        "Dynamic analysis",
        modules_fama_network_ui("window15", mode = "dynamic")
      ),
      tabPanel(
        "Robustness check",
        div(
          class = "panel-card",
          div(class = "panel-title", "Explore different window lengths"),
          div(
            class = "panel-note",
            "This tab allows you to explore different time window lengths for the dynamic analysis."
          ),
          radioButtons(
            "dynamic_window",
            "Window length",
            choices = c("10", "15", "20"),
            selected = "15",
            inline = TRUE
          )
        ),
        modules_fama_network_ui("window_select", mode = "dynamic")
      ),
      tabPanel(
        "About",
        mod_about_ui("about")
      )
    )
  )
)


# ---- Server ----

server <- function(input, output, session) {
  window_data <- reactive({
    req(input$dynamic_window)
    switch(
      input$dynamic_window,
      "10" = list(list_ggraph = list_ggraph_10, corpus = corpus_10),
      "15" = list(list_ggraph = list_ggraph_15, corpus = corpus_15),
      "20" = list(list_ggraph = list_ggraph_20, corpus = corpus_20)
    )
  })

  modules_fama_network_server(
    "all",
    list_ggraph = list_ggraph_all,
    corpus = corpus_all
  )
  modules_fama_network_server(
    "window15",
    list_ggraph = selected_graphs_15,
    corpus = corpus_15
  )
  modules_fama_network_server(
    "window_select",
    list_ggraph = reactive(window_data()$list_ggraph),
    corpus = reactive(window_data()$corpus)
  )
}

# Run app
shinyApp(ui = ui, server = server)
