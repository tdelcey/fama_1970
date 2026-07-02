# The Dissemination of Fama (1970): A Bibliometric Analysis

Replication package for the paper **"The Dissemination of Fama (1970): A Bibliometric Analysis"** by Thomas Delcey (Université de Bourgogne, LEDI).

The paper studies how Fama's 1970 *Journal of Finance* review article on the efficient market hypothesis disseminated across different communities of research — financial economics, law, management, accounting — from 1970 to 2010. The method combines quantitative bibliographic coupling network analysis (backbone extraction via the stochastic degree sequence model, Leiden clustering) with qualitative reading of documents in each cluster.

An online version of the interactive app is available at:
https://019c241f-91f4-a63b-1097-ed53083ffbbc.share.connect.posit.cloud

## Repository structure

```
fama_1970/
├── paper_V3.qmd           # Main paper (Quarto, renders to PDF)
├── references.bib         # BibTeX references
├── _quarto.yml            # Quarto project config
├── image/                 # Pre-generated figures (JPG/PNG) included in the paper
├── app/                   # Shiny interactive application (self-contained, data included)
└── R/                     # Analysis scripts (run before compiling the paper)
    ├── paths_and_packages.R          # Shared paths and package loading — edit data paths here
    ├── _functions.R                  # Shared helper functions
    ├── coupling_analysis_static.R    # Builds the static coupling network (1970–2010)
    ├── coupling_analysis_dynamic.R   # Builds the four dynamic coupling networks
    ├── figure_fama_citations.R       # Figure: citations by year for Fama articles
    ├── figure_clusters_disciplines.R # Figure: discipline share over time by cluster
    └── table_corpus_descriptive.R    # Tables: top journals, authors, articles
```

## Run the interactive app

The Shiny app in `app/` ships with pre-built `.rds` data files, so it runs out of the box — no access to the underlying licensed data is required.

**Requirements:** R ≥ 4.4

Install dependencies once:

```r
install.packages("pacman")
pacman::p_load(
  shiny, tidyverse, tidytext, DT, shinyWidgets,
  ggiraph, here, ggwordcloud, shinycssloaders
)
```

Then launch the app from the repository root:

```r
shiny::runApp("app")
```

## Reproduce the paper

The paper is a Quarto document that renders to PDF via LaTeX.

**The raw and processed data are not included in this repository** because they are large and/or contain licensed Web of Science data that cannot be redistributed. To replicate the analysis from scratch you need:
- Web of Science Core Collection citation records for articles citing Fama (1970)
- The processed `.rds` files in `clean_data/` (available on request from the author)

**Prerequisites:** R, Quarto, a LaTeX distribution (TinyTeX or TeX Live), and the R packages listed in `R/paths_and_packages.R` (installed via `pacman::p_load`).

**Step 1 — Configure data paths.**
Open `R/paths_and_packages.R` and set `data_path` to the directory where the project data lives on your machine:

```r
data_path <- "C:/your/path/to/data"
```

The script then derives sub-paths automatically. The expected folder structure under `data_path` is:

```
data_path/
└── fama_1970_project/
    ├── clean_data/      # Pre-processed .rds files consumed by the paper
    ├── wos/             # Raw Web of Science data
    ├── openalex/        # Raw OpenAlex data
    ├── intermediate_data/
    └── figures/
```

**Step 2 — Run the R scripts** (to regenerate processed data and figures):

```r
source("R/coupling_analysis_static.R")
source("R/coupling_analysis_dynamic.R")
source("R/figure_fama_citations.R")
source("R/figure_clusters_disciplines.R")
source("R/table_corpus_descriptive.R")
```

**Step 3 — Render the paper:**

```bash
quarto render paper_V3.qmd
```

Output goes to `_output/`.

## Contact

For access to the processed data, contact Thomas Delcey (thomas.delcey@ube.fr).
