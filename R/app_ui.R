#' @title Application UI
#' @description Define the user interface for the SUF-Explorer Shiny application.
#' This includes the main navbar, sidebar, and all modules for dataset exploration and transformation.
#'
app_ui <- function() {

  # Make package www resources accessible in Shiny
  shiny::addResourcePath(
    "www",
    system.file("www", package = "NEPScribe")
  )

bslib::page_navbar(
  # --- Header includes CSS and JS from package, plus Shiny feedback/js initialization ---
  header = htmltools::tags$head(
    shinyFeedback::useShinyFeedback(),
    shinyjs::useShinyjs(),

    # Serve CSS from package via URL
    htmltools::tags$link(
      rel = "stylesheet",
      type = "text/css",
      href = "www/css/styles.css"
    ),

    # Serve JS from package via URL
    htmltools::tags$script(
      src = "www/js/js_snippets.js"
    )
  ),
  id = "nav",
  # --- Theme setup ---
  theme = bslib::bs_theme(
    bootswatch = "minty",
    base_font = bslib::font_google("Fira Sans"),
    code_font = bslib::font_google("Fira Sans"),
    heading_font = bslib::font_google("Fira Code")
  ),
  navbar_options = bslib::navbar_options(bg = "lightblue"),

  # --- Sidebar setup ---
  sidebar = bslib::sidebar(
    id = "sidebar",
    shiny::conditionalPanel(
      condition = "input.nav === 'Start'",
      title = "Starting Page",
      cohort_ui("cohort"),  # Module for choosing starting cohort
      settings_ui("settings"),
    ),
    shiny::conditionalPanel(
      condition = "input.nav === 'Explore Datasets  '",
      title = "Click here to select dataset",
      dataset_ui("explore_dataset")
    ),
    shiny::conditionalPanel(
      condition = "input.nav === 'Transform Data'",
      data_transformation_sidebar_ui("data_transformation")
    ),
    open = TRUE
  ),

  # --- Main panels ---
  bslib::nav_panel(
    title = "Start",
    htmltools::HTML(
      "<div style='display: flex; align-items: center;'>
         <img src='www/images/neps_logo.jpg' width='200' height='100' style='margin-right: 10px;'>
         <p style='font-size:22px; margin: 0;'><b>NEPScribe</b>
         <span style='font-size:14px'><b>Beta</b></span></p>
       </div>
       <br>
       <ul>
         <li><b>Dataset Exploration (SC3-SC6)</b>: Search for keywords in specific datasets and get an overview over variables and metadata</li>
         <li><b>Dataset Transformation (SC3-SC6)</b>: Dynamically generate STATA or R scripts that transform multiple NEPS data files into a person-year-format.</li>
       </ul>
       <br>
       <p><b>Before getting started, please select a starting cohort below:</b></p>
       <p>This determines which datasets will be loaded in the app. You can explore and transform data for the chosen cohort.</p>"
    ),
    icon = shiny::icon("door-open")
  ),
  bslib::nav_panel(title = "Explore Datasets  ", dataset_exploration_card(), icon = shiny::icon("table")),
  bslib::nav_panel(title = "Transform Data", cards_data_trans(), icon = shiny::icon("wrench")),

  bslib::nav_spacer(),

  # --- Help menu ---
  bslib::nav_menu(
    title = "Help",
    align = "right",
    bslib::nav_item(htmltools::tags$a("NEPS Website", href = "https://www.neps-data.de/", target="_blank")),
    bslib::nav_item(htmltools::tags$a("NEPS Documentation", href = "https://www.neps-data.de/Data-Center/Data-and-Documentation", target="_blank")),
    bslib::nav_item(htmltools::tags$a("SUF-Explorer Documentation", href = "", target="_blank")),
    bslib::nav_item(htmltools::tags$a("References", href = "", target="_blank")),
    bslib::nav_item(htmltools::tags$a("Contact Authors", href = "https://www.wzb.eu/de/personen/alexander-helbig", target="_blank")),
    bslib::nav_item(htmltools::tags$a("GitHub", href = "https://github.com/a-helbig/nepscribe", target="_blank"))
  )
)
}
