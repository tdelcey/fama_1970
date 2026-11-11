pacman::p_load(
  # Data manipulation and visualization
  "tidyverse",
  "data.table",
  "scales",
  "readxl",
  "kableExtra",
  "ggrepel",
  "ggforce",
  "ggiraph",

  # Text processing
  "tidytext",

  # Network analysis
  "tidygraph",
  "ggraph",
  "backbone",      # version 2.x (2.1.5): backbone 3.x rejects data.frame edgelists
  "biblionetwork",
  "networkflow",   # GitHub only: see README
  "vite",          # GitHub only: see README

  # Paths
  "here"
)


source(here::here("R", "_functions.R"))

project_path <- here::here()


data_path <- if (.Platform$OS.type == "windows") {
  "C:/cloud/data"
} else {
  path.expand("~/cloud/data")
}

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
