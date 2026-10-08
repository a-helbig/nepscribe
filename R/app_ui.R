#' @title Application UI
#' @description Define the user interface for the SUF-Explorer Shiny application.
#' This includes the main navbar, sidebar, and all modules for dataset exploration and transformation.
#'
#' @keywords internal
#' @noRd
#' @importFrom htmltools tags
app_ui <- function() {

  app_version <- "v0.3.5"  # define your current app version here

  # Make package www resources accessible in Shiny
  shiny::addResourcePath(
    "www",
    system.file("www", package = "NEPScribe")
  )
  # cache-busting: append the file's modification time so browsers reload changed CSS/JS
  www_version <- function(file) {
    mtime <- base::file.mtime(system.file("www", file, package = "NEPScribe"))
    base::paste0("www/", file, "?v=", base::format(mtime, "%Y%m%d%H%M%S"))
  }

bslib::page_navbar(
  # --- Header includes CSS and JS from package, plus Shiny feedback/js initialization ---
  header = htmltools::tags$head(
    shinyFeedback::useShinyFeedback(),
    shinyjs::useShinyjs(),

    # Serve CSS from package via URL
    htmltools::tags$link(
      rel = "stylesheet",
      type = "text/css",
      href = www_version("css/styles.css")
    ),
    htmltools::tags$head(
      htmltools::tags$meta(name = "robots", content = "noindex, nofollow")
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
    # Served locally (not from a CDN) so no visitor data is sent to third-party servers
    htmltools::tags$link(
      rel = "stylesheet",
      href = www_version("vendor/highlightjs/github.min.css")
    ),
    htmltools::tags$script(
      src = www_version("vendor/highlightjs/highlight.min.js")
    ),
    # Stata is not part of the common highlight.js bundle: official grammar, same version (11.9.0)
    htmltools::tags$script(
      src = www_version("vendor/highlightjs/languages/stata.min.js")
    ),

    # Serve JS from package via URL
    htmltools::tags$script(
      src = www_version("js/js_snippets.js")
    )
  ),
  id = "nav",
  # --- Footer with legal links, visible on every page ---
  # Links to the WZB pages, so nothing has to be maintained in the app itself
  footer = htmltools::tags$footer(
    class = "legal-footer",
    htmltools::tags$a("Legal notice", href = "https://wzb.eu/en/legal-notice", target = "_blank"),
    " \u00b7 ",
    htmltools::tags$a("Data protection", href = "https://wzb.eu/en/data-protection", target = "_blank")
  ),
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
    open = FALSE
  ),

  # --- Main panels ---
  bslib::nav_panel(
    title = "Start",
    htmltools::HTML(
      "<!-- padding-bottom keeps the last box clear of the fixed footer with the legal links -->
     <div style='max-width: 900px; margin: 0 auto; padding-bottom: 2.5rem;'>
     <!-- 3 columns so the title sits exactly centered, with the lizard to its left -->
     <div style='display: grid; grid-template-columns: 1fr auto 1fr; align-items: center;'>
       <img src='www/images/lizard_instead_of_neps.jpg' width='200' height='100' style='justify-self: end; margin-right: 10px;' alt=''>
       <div>
         <p style='font-size:32px; margin: 0;'><b>NEPScribe</b>
         <span style='font-size:16px; margin-left: 6px;'>Beta</span></p>
         <small style='font-size:12px; color:gray;'>Version: ", app_version, "</small>
       </div>
       <div></div>
     </div>
     <br>

     <!-- Features Box -->
     <div style='padding: 15px; border: 1px solid #ccc; border-radius: 8px; background-color: #f9f9f9;'>
       <p style='font-size:18px; font-weight: bold; margin-bottom: 10px;'>Features</p>
       <ul style='margin-left: 20px; line-height: 1.6;'>
                  <li>
              <b>Dataset Transformation (SC3-SC6):</b> Dynamically create a Stata or R script for person-year data preparation. It merges multiple NEPS SUF data files and transforms into a person-year format, with one row for each wave of each respondent.
              <br>
              <br>
              <ul>
                <li>Choose between using the spellfiles or the biography file as the baseline for data preparation.</li>
                <li>Select variables from most datasets and easily include them in the script.</li>
                <li>Add sample code for complex data preparation tasks, such as further training, highest educational degree, or children.</li>
                <li>Obtain a script that handles most of the complex restructuring and merging of the data.</li>
                <li>However, careful review of the script and additional data preparation remain necessary.</li>
              </ul>
            </li>
            <br>
            <li>
           <b>Dataset Exploration (SC1-SC8):</b> Browse available meta data in NEPS SUF data to get an overview of datasets and variables.
           </b>
           <br>
           <br>
           <ul>
           <li>Search for keywords in specific or all datasets.</li>
           <li>Compare items and variables across starting cohorts.</li>
           <li>Check what meta data is available for which variables.</li>
          </ul>
         </li>
       </ul>
     </div>
     <br>
     <!-- Notes Box -->
     <div style='padding: 15px; border: 1px solid #ccc; border-radius: 8px; background-color: #f1f8ff;'>
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
     </div>
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
    # Legal notice and data protection: links to the WZB pages (also in the footer)
    bslib::nav_item(htmltools::tags$a("Legal notice", href = "https://wzb.eu/en/legal-notice", target = "_blank")),
    bslib::nav_item(htmltools::tags$a("Data protection", href = "https://wzb.eu/en/data-protection", target = "_blank")),
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
