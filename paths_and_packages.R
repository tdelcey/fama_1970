pacman::p_load(
  # General Data Manipulation and Visualization
  "tidyverse",
  "dplyr",
  "janitor",
  "ggsci",
  "ggplot2",
  "ggrepel",
  "scales",
  "patchwork",
  "reshape",
  "reshape2",
  "data.table",
  "broom",
  "stringr",
  "xtable",
  "kableExtra",

  # Text Processing and Analysis
  "tidytext",
  "textcat",
  "textdata",
  "text2vec",
  "tm",
  "quanteda",
  "spacyr",
  "pdftools",
  "tesseract",

  # Network Analysis
  "networkflow",
  "tidygraph",
  "ggraph",
  "graphlayouts",
  "backbone",
  "vite",

  # Web Scraping and APIs
  "rscopus",
  "here",
  "xml2",
  "XML",
  "rvest",
  "httr",
  "jsonlite",
  "RSelenium",
  "openalexR",

  # Visualization Enhancements
  "ggnewscale",
  "ggiraph",
  "gganimate",
  "plotly",
  "scico",
  "transformr",
  "ragg",
  "ggraph",
  "ggforce",
  "ggforce",

  # Shiny and Interactive Dashboards
  "shiny",
  "shinyWidgets",
  "shinylive",
  "flexdashboard",
  "rsconnect",
  "DT",
  "httpuv",

  # Machine learning

  "stm",
  "tidystm",
  "tidymodels",
  "textrecipes",
  "themis",
  "topicmodels",
  "ldatuning",
  "modelsummary",

  # Miscellaneous
  "future"
)


source(here::here("R", "_functions.R"))

project_path <- here::here()


data_path <- "C:/cloud/data"

# WOS FULL DATA
# WOS_data_path <- "C:/Users/thomd/Documents/MEGA/data/WoS/"
WOS_data_path <- here(data_path, "WoS")
data_path_project <- here(data_path, "fama_1970_project")

# project data path
wos_data_path <- here(data_path_project, "wos")
openalex_data_path <- here(data_path_project, "openalex")
intermediate_data_path <- here(data_path_project, "intermediate_data")
clean_data_path <- here(data_path_project, "clean_data")
figures_path <- here(data_path_project, "figures")
app_path <- here(project_path, "app", "data")

# for full text script
destination_path <- here(data_path_project, "text", "destination")
download_path <- here(data_path_project, "text", "download")
grobid_path <- here(data_path_project, "text", "grobid")
