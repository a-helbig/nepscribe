app_server <- function(input, output, session) {

  session$onSessionEnded(function() {
    stopApp()
  })

  # disable nav_panels 2-5 when the app starts
  shinyjs::disable(selector = '.navbar-nav a[data-value="Explore Datasets  "')
  shinyjs::disable(selector = '.navbar-nav a[data-value="Transform Data"')

  # call the different server modules and pass reactives for datapath and selected dataset from other modules as arguments to the other modules

  # we call datapath_server here and we use its react+ive return value in all other modules to load data
  datapath_reactive <- datapath_server("datapath")

  # we call settings_server here and we use its reactive return value in all other modules to load settings
  settings_reactive <- settings_server("settings")

  # observe to enable navpanels after a datapath with dta files inside has been provided
  observe({
    req(datapath_reactive()$datapath)
    # When we have a Datapath (length of char vector in provided datapath is not 0), then enable nav panels
    if(length(list.files(datapath_reactive()$datapath, pattern="^SC\\d+.*\\.dta$", full.names=TRUE)) != 0){
      toggle_sidebar("sidebar")
      shinyjs::enable(selector = '.navbar-nav a[data-value="Explore Datasets  "')
      shinyjs::enable(selector = '.navbar-nav a[data-value="Transform Data"')
    }
  })

  # send input from sidebar width selector to custom js snippet in order to change the size of the sidebar
  observeEvent(settings_reactive(), {
    session$sendCustomMessage("sidebarWidth", settings_reactive()$sidebarWidth)
  })

  # call dataset explorer server module and handover reactive datapath
  reactive_data_list <- dataset_explorer_server(id = "explore_dataset", datapath_reactive, settings_reactive)

  # call data transformation server module
  data_transformation_server("data_transformation",datapath_reactive,settings_reactive)

}
