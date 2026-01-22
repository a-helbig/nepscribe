#' @title Application UI
#' @description Define the user interface for the SUF-Explorer Shiny application.
#' This includes the main navbar, sidebar, and all modules for dataset exploration and transformation.
#'
#' @keywords internal
#' @noRd
app_ui <- function() {

  app_version <- "v0.2.0"  # define your current app version here

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
    # make green ticks in picker inputs appear on the left side instead of right
      htmltools::tags$style(htmltools::HTML("
    .bootstrap-select .dropdown-menu li a span.check-mark {
      left: 10px;         /* distance from left */
      right: auto;         /* remove right alignment */
    }
    .bootstrap-select .dropdown-menu li a {
      padding-left: 30px;  /* add space for tick on the left */
    }
  ")),
    # highlight.js (syntax highlighting) for preview in data trans
    htmltools::tags$link(
      rel = "stylesheet",
      href = "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.9.0/styles/github.min.css"
    ),
    htmltools::tags$script(
      src = "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.9.0/highlight.min.js"
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
      settings_ui("settings"),
    ),
    shiny::conditionalPanel(
      condition = "input.nav === 'Transform Data'",
      data_transformation_sidebar_ui("data_transformation")
    ),
    shiny::conditionalPanel(
      condition = "input.nav === 'Explore Datasets  '",
      dataset_ui("explore_dataset")
    ),
    open = TRUE
  ),

  # --- Main panels ---
  bslib::nav_panel(
    title = "Start",
    htmltools::HTML(
      "<div style='display: flex; align-items: center;'>
       <img src='www/images/neps_logo.jpg' width='200' height='100' style='margin-right: 10px;'>
       <div>
         <p style='font-size:22px; margin: 0;'><b>NEPScribe</b>
         <span style='font-size:14px; margin-left: 5px;'>Beta</span></p>
         <small style='font-size:12px; color:gray;'>Version: ", app_version, "</small>
       </div>
     </div>
     <br>

     <!-- Features Box -->
     <div style='max-width: 900px; padding: 15px; border: 1px solid #ccc; border-radius: 8px; background-color: #f9f9f9;'>
       <p style='font-size:18px; font-weight: bold; margin-bottom: 10px;'>Features</p>
       <ul style='margin-left: 20px; line-height: 1.6;'>
                  <li>
              <b>Dataset Transformation (SC3-SC6):</b> Create a dynamic Stata or R script for person-year data preparation. It transforms and merges multiple NEPS SUF data files into a person-year format, with one row for each wave of each respondent.
              <br>
              <br>
              <ul>
                <li>Choose between using the spellfiles or the biography file as the baseline for data preparation.</li>
                <li>Additionally, select variables from most datasets and easily include them in the script.</li>
                <li>Optionally, add sample code for complex data preparation tasks, such as further training, highest educational degree, or children.</li>
                <li>Finally, obtain a script that handles most of the complex restructuring and merging of the data.</li>
                <li>However, careful review of the script and additional data preparation remain necessary.</li>
              </ul>
            </li>
            <br>
            <li>
           <b>Dataset Exploration (SC1-SC8):</b> Browse available meta data in NEPS SUF data to get an overview of datasets and variables.
           Search for keywords in specific or all datasets. Compare items and variables across starting cohorts.
         </li>
       </ul>
     </div>
     <br>
     <!-- Notes Box -->
     <div style='max-width: 900px; padding: 15px; border: 1px solid #ccc; border-radius: 8px; background-color: #f1f8ff;'>
            <p style='font-size:18px; font-weight: bold; margin-bottom: 10px;'>Note</p>
              <ul style='margin-left: 20px; line-height: 1.6;'>
                <li>
                  The app is based on NEPS semantic structure files, which are identical to NEPS SUF data files but have had all observations removed and are therefore publicly available.
                </li>
                <br>
                <li>
                  You may change the sidebar width in the sidebar to be able to read long variable names on smaller screens.
                </li>
                <br>
                <li>
                If you find any issues or bugs in the app or in the generated scripts, please report them to alexander.helbig@wzb.eu or open an issue on the app's github page (See help tab in the navbar).
                </li>
              </ul>
     </div>"
    ),
    icon = shiny::icon("door-open")
  ),
  bslib::nav_panel(title = "Transform Data", cards_data_trans(), icon = shiny::icon("wrench")),
  bslib::nav_panel(title = "Explore Datasets  ", dataset_exploration_card(), icon = shiny::icon("table")),
  bslib::nav_spacer(),

  # --- Help menu ---
  bslib::nav_menu(
    title = "Help",
    align = "right",
    bslib::nav_item(htmltools::tags$a("NEPS Website", href = "https://www.neps-data.de/", target="_blank")),
    bslib::nav_item(htmltools::tags$a("NEPS Documentation", href = "https://www.neps-data.de/Data-Center/Data-and-Documentation", target="_blank")),
    # bslib::nav_item(htmltools::tags$a("SUF-Explorer Documentation", href = "", target="_blank")),
    # bslib::nav_item(htmltools::tags$a("References", href = "", target="_blank")),
    bslib::nav_item(htmltools::tags$a("Contact Authors", href = "https://www.wzb.eu/de/personen/alexander-helbig", target="_blank")),
    bslib::nav_item(htmltools::tags$a("GitHub", href = "https://github.com/a-helbig/nepscribe", target="_blank")),
    bslib::nav_item(shiny::actionLink("show_changelog", "View Changelog"))
  )
)
}
