#' Cards for Dataset Exploration and Data Transformation
#'
#' These functions create UI cards for the Shiny app:
#' - `dataset_exploration_card()` for exploring datasets
#' - `cards_data_trans()` for data transformation
#'
#' @return `bslib::navset_card_tab` objects
#' @keywords internal

#' Dataset Exploration Card
#'
#' Creates a `navset_card_tab` containing a tab for dataset exploration:
#' Variable Table: Shows a data overview table with settings dropdown.
#'
#' @return A `navset_card_tab` object for Shiny UI.
dataset_exploration_card <- function() {
  bslib::navset_card_tab(
    full_screen = TRUE,
    title = htmltools::HTML("<b>Explore Datasets</b>"),
    header = shiny::tagList(
      htmltools::tags$script(
        htmltools::HTML(
          '$(document).on("shiny:inputchanged", function(event) {
               $(\'span[data-toggle="tooltip\']\').tooltip({
                   html: true,
                   trigger: "hover"
               });
           });'
        )
      ),
      htmltools::tags$style(
        htmltools::HTML(
          '.tooltip .tooltip-inner {
               background-color: grey !important;
               color: white !important;
               border: 1px solid grey !important;
           }
           .tooltip .tooltip-arrow {
               border-top-color: grey !important;
           }'
        )
      )
    ),
    bslib::nav_panel(
      title = htmltools::HTML("<span data-toggle='tooltip' title='Meta information on selected dataset'>Variable Table</span>"),
      bslib::card_title(""),
      dataset_overview_ui(id = "explore_dataset"),
      full_screen = FALSE
    )
  )
}


#' Data Transformation Card
#'
#' Creates the UI card for the "Transform Data" tab
#'
#' @return A `navset_card_tab` object
# cards_data_trans <- function() {
#   bslib::navset_card_tab(
#     height = 450,
#     full_screen = TRUE,
#     title = htmltools::HTML("<b>Transform Data</b>"),
#
#     # Info panel
#     bslib::nav_panel(
#       title = htmltools::HTML("<span data-toggle='tooltip' title='Show Info Page on Transform data'>Info</span>"),
#       bslib::layout_column_wrap(
#         width = 1/2,
#         height = 300,
#         bslib::layout_column_wrap(
#           width = 1,
#           heights_equal = "row",
#           bslib::card(
#             shiny::div(
#               htmltools::HTML(
#                 "<div style=max-width: 800px; margin: 0 auto; font-family: Arial, sans-serif; line-height: 1.6;>
#     <h4 style=font-size: 18px; color: #333;>What is this?</h4>
#     <br>
#     <p style=font-size: 18px; color: #555;>
#         This feature will allow you to create dynamic scripts that will transform NEPS SUF data into a person-year format,
#         where each row in the dataset corresponds to one year in a respondent's life and has a unique life-course status (e.g. employment or vocational training).
#     </p>
#     <p style=font-size: 18px; color: #555;>
#         This data structure is essential for various research projects, but implementing it can be challenging without prior experience.
#     </p>
#     <p style=font-size: 18px; color: #555;>
#         The script with basic settings will create a person-year-dataset that only contains very basic spell-related information.
#     </p>
#     <p style=font-size: 18px; color: #555;>
#         You can customize numerous settings to align with the requirements of your research project and enrich the resulting dataset with variables from all datasets.
#     </p>
#     <div style=background-color: #f9f9f9; border-left: 4px solid #007BFF; padding: 10px; margin: 20px 0;>
#         <strong>Important Note:</strong><br>
#         This feature is restricted to Starting Cohorts 3-6.
#     </div>
# </div>"
#               )
#             )
#           ),
#           bslib::card(
#             shiny::div(
#               htmltools::HTML(
#                 "<div style=max-width: 800px; margin: 0 auto; font-family: Arial, sans-serif; line-height: 1.6;>
#     <h4 style=font-size: 18px; color: #333;>What else to consider?</h4>
#     <br>
#     <p style=font-size: 18px; color: #555;>
#         The generated script is intended as a template and should not be used without review. We highly recommend thoroughly understanding each data preparation step outlined in the script. There may be alternative or more effective methods for preparing a person-year dataset that better align with your specific analytical needs.
#     </p>
#     <p style=font-size: 18px; color: #555;>
#         Additionally, the generated person-year dataset will likely require further variable preparation.
#     </p>
#     <p style=font-size: 18px; color: #555;>
#         Generally, it is advisable to review the script line by line to identify and correct any potential errors.
#     </p>
# </div>"
#               )
#             )
#           )
#         )
#       )
#     ),
#
#     # Spell Prioritization panel
#     bslib::nav_panel(
#       title = htmltools::HTML("<span data-toggle='tooltip' title='Edit spell priorisation order in person year dataset script if desired'>Spell Prioriation</span>"),
#       data_transformation_prio_ui("data_transformation")
#     ),
#
#     # Add Variables panel
#     bslib::nav_panel(
#       title = htmltools::HTML("<span data-toggle='tooltip' title='Add additional variables to the person year dataset script'>Additional Variables</span>"),
#       htmltools::tags$head(
#         htmltools::tags$style(
#           ".multi-wrapper {
#             height: 60vh;  /* 60% of the viewport height */
#           }
#           .multi-wrapper .non-selected-wrapper,
#           .multi-wrapper .selected-wrapper {
#             height: 90%;
#           }"
#         )
#       ),
#       data_transformation_add_variables_ui("data_transformation")
#     )
#   )
# }
