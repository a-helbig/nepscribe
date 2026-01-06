
app_ui <- function() { # define UI function
  page_navbar( # use bslibs page_navbar layout for the app
    header = tags$head(
      shinyFeedback::useShinyFeedback(),
      shinyjs::useShinyjs(),
      includeCSS("www/styles.css"),
      tags$script(src = "js_snippets.js")
    ),
    id = "nav",
    theme = bs_theme(
      bootswatch = "minty",
      base_font = font_google("Fira Sans"),
      code_font =  font_google("Fira Sans"),
      heading_font = font_google("Fira Code")
    ),
    navbar_options = navbar_options(bg = "lightblue"),
    sidebar = sidebar(
      id = "sidebar",
      # in the sidebar we use conditional panels, that are only enabled when a valid datapath is provided. In the cond.-panels we call all the different sidebar_UI Modules
      conditionalPanel(
        condition = "input.nav === 'Start'",
        title = "Starting Page",
        settings_ui("settings")
      ),
      conditionalPanel(
        condition = "input.nav === 'Explore Datasets  '",
        title = "Click here to select dataset",
        dataset_ui("explore_dataset")
      ),
      conditionalPanel(
        condition = "input.nav === 'Transform Data'",
        data_transformation_sidebar_ui("data_transformation")
      ),
      open = F
    ),

    nav_panel(
      title = "Start",
      HTML(
        "<div style='display: flex; align-items: center;'>
       <img src='neps_logo.jpg' width='200' height='100' style='margin-right: 10px;'>
       <p style='font-size:22px; margin: 0;'><b>SUF-Explorer</b>
       <span style='font-size:14px'><b>Beta</b></span></p>
     </div> <br> - Dataset Exploration (SC1-SC6): Search for keywords in specific or across all datasets in directory and have a first glance at the actual data sheets with filter possibility. <br> - Variable Exploration (SC1-SC6): Inspect frequency tables, meta-data and plots of selected variables. <br> - Life Course Vizualisation (SC3-SC6): Visualise life course trajectories with sequence plots. <br> - Dataset Transformation (SC3-SC6): Dynamically generate STATA or R scripts which transform multiple NEPS data files into one file with a person-year-format. <br><br> <b>Before getting started, please provide the filepath to your local NEPS-SUF directory.</b> - Make sure that this directory contains only NEPS SUF data files (not the zipped download file).
       <br> - Note that some features may not function properly with older SUF versions, so it is recommended to use the latest SUF release."

      ),
      p(""),
      datapath_ui("datapath"), # call the module "datapath_ui"
      , icon = icon("door-open")
    ),
    nav_panel(title = "Explore Datasets  ", dataset_exploration_card, icon = icon("table")),
    nav_panel(title = "Transform Data", cards_data_trans, icon = icon("wrench")),
    nav_spacer(),
    nav_menu(
      title = "Help",
      align = "right",
      nav_item(tags$a("NEPS Website", href = "https://www.neps-data.de/", target="_blank")),
      nav_item(tags$a("NEPS Documentation", href = "https://www.neps-data.de/Data-Center/Data-and-Documentation", target="_blank")),
      nav_item(tags$a("SUF-Explorer Documentation", href = "", target="_blank")),
      nav_item(tags$a("References", href = "", target="_blank")),
      nav_item(tags$a("Contact Authors", href = "https://www.wzb.eu/de/personen/alexander-helbig", target="_blank")),
      nav_item(tags$a("GitLab", href = "https://gitlab.wzb.eu/ahelbig/suf-explorer-production", target="_blank"))
    )
  )
}
