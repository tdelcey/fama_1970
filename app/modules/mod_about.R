# modules/mod_about.R

mod_about_ui <- function(id) {
  ns <- NS(id)

  div(
    style = "
      border:2px solid #D4D4D4;
      border-radius:10px;
      padding:20px;
      background-color:#FAFAFA;
    ",
    div(
      style = "font-size:24px; font-weight:600; margin-bottom:8px;",
      "The dissemination of Fama (1970): A bibliometric analysis"
    ),
    div(
      style = "font-size:14px; color:#555; margin-bottom:14px;",
      "Compiled February 3, 2026"
    ),
    callout_box(
      title = "Abstract",
      text = "What is the life of a seminal paper? This article addresses this question by examining the dissemination of Fama (1970), which introduced the efficient market hypothesis. Using network analysis, I map the communities that cite Fama (1970) and track their evolution over time. I show that the paper became not only canonical in financial economics but also a reference in law, management, and marketing. Market efficiency was interpreted in multiple ways, from a testable hypothesis about prices to a working assumption for policy evaluation. Tracing these pathways, the analysis highlights the growing influence of financial economics across the social sciences in the second half of the twentieth century. The article also contributes methodologically by providing an interactive, open-source platform to explore the networks and clusters.",
      border = "#607D8B",
      bg = "#F5F7FA"
    )
  )
}
