# The Dissemination of Fama (1970): A Bibliometric Analysis

Replication package for the paper **"The Dissemination of Fama (1970): A Bibliometric Analysis"** by Thomas Delcey (Université de Bourgogne, LEDI).

The paper studies how Fama's 1970 *Journal of Finance* review article on the efficient market hypothesis disseminated across different communities of research — financial economics, law, management, accounting — from 1970 to 2010. The method combines quantitative bibliographic coupling network analysis (backbone extraction via the stochastic degree sequence model, Leiden clustering) with qualitative reading of documents in each cluster.

An online version of the interactive app is available at:
https://019f12e5-9feb-cbef-4462-676ff2bbde4f.share.connect.posit.cloud

The app itself is also included in this repository (see `app/` below) and can be run locally by anyone who downloads or clones the repository — no online access is required.

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

## Contact

For access to the processed data, contact Thomas Delcey (thomas.delcey@ube.fr).
