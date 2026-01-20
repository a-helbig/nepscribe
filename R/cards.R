#' Dataset Exploration Card
#'
#' Creates a `navset_card_tab` containing a tab for dataset exploration:
#' Variable Table: Shows a data overview table with settings dropdown.
#'
#' @return A `navset_card_tab` object for Shiny UI.
#' @keywords internal
#' @noRd

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
#' @keywords internal
#' @noRd
cards_data_trans <- function() {
  bslib::navset_card_tab(
    height = 450,
    full_screen = TRUE,
    title = htmltools::HTML("<b>Transform Data</b>"),

    # Info panel
    bslib::nav_panel(
      title = htmltools::HTML("<span data-toggle='tooltip' title='Show Info Page on Transform data'>Info</span>"),
      bslib::layout_column_wrap(
        width = 1/2,
        height = 300,
        bslib::layout_column_wrap(
          width = 1,
          heights_equal = "row",
          bslib::card(
            shiny::div(
              htmltools::HTML(
                "<div style=max-width: 800px; margin: 0 auto; font-family: Arial, sans-serif; line-height: 1.6;>
    <h4 style=font-size: 18px; color: #333;>What is this?</h4>
    <br>
    <p style=font-size: 18px; color: #555;>
        This feature will allow you to create dynamic scripts that will transform NEPS SUF data into a person-year format,
        where each row in the dataset corresponds to one year in a respondent's life and has a unique life-course status (e.g. employment or vocational training).
    </p>
    <p style=font-size: 18px; color: #555;>
        This data structure is essential for various research projects, but implementing it can be challenging without prior experience.
    </p>
    <p style=font-size: 18px; color: #555;>
        The script with basic settings will create a person-year-dataset that only contains very basic spell-related information.
    </p>
    <p style=font-size: 18px; color: #555;>
        You can customize numerous settings to align with the requirements of your research project and enrich the resulting dataset with variables from all datasets.
    </p>
    <div style=background-color: #f9f9f9; border-left: 4px solid #007BFF; padding: 10px; margin: 20px 0;>
        <strong>Important Note:</strong><br>
        This feature applies only to Starting Cohorts 3 to 6. However, it has so far only been extensively tested for SC6. Scripts for SC3, SC4 and SC5 may need substantial adjustments.
    </div>
</div>"
              )
            )
          ),
          bslib::card(
            shiny::div(
              htmltools::HTML(
                "<div style=max-width: 800px; margin: 0 auto; font-family: Arial, sans-serif; line-height: 1.6;>
    <h4 style=font-size: 18px; color: #333;>What else to consider?</h4>
    <br>
    <p style=font-size: 18px; color: #555;>
        The generated script is intended as a template and should not be used without review. We highly recommend thoroughly understanding each data preparation step outlined in the script. There may be alternative or more effective methods for preparing a person-year dataset that better align with your specific analytical needs.
    </p>
    <p style=font-size: 18px; color: #555;>
        Additionally, the generated person-year dataset will likely require further variable preparation.
    </p>
    <p style=font-size: 18px; color: #555;>
        Generally, it is advisable to review the script line by line to identify and correct any potential errors.
    </p>
</div>"
              )
            )
          )
        ),
        bslib::card(
          shiny::div(
          htmltools::HTML(
            "<div style=max-width: 800px; margin: 0 auto; font-family: Arial, sans-serif; line-height: 1.6;>
    <h4 style=font-size: 18px; color: #333;>How to use?</h4>
    <br>

    <h5 style=font-size: 16px; color: #007BFF;>Format</h5>
    <p style=font-size: 18px; color: #555;>
        First, you will need to choose between two format options in the sidebar:
    </p>
    <ol style=font-size: 18px; color: #555;>
        <li><b>Harmonized Spell Format</b>: This option prepares data based on the edited and cleaned biography file to represent life-course trajectories. It is recommended to use this format. </li>
        <li><b>Original Subspell Format</b>: This option uses the originally recorded subspell episodes. Please note that data cleaning and smoothing operations performed in the biography are not accounted for in this preparation.</li>
    </ol>

    <h5 style=font-size: 16px; color: #007BFF;>Starting cohort</h5>
    <p style=font-size: 18px; color: #555;>
        Now, you need to choose a starting cohort.
    </p>

    <h5 style=font-size: 16px; color: #007BFF;>Script Type</h5>
    <p style=font-size: 18px; color: #555;>
        Next, you can select whether you want an R or STATA script.
    </p>

    <h5 style=font-size: 16px; color: #007BFF;>Settings</h5>
    <p style=font-size: 18px; color: #555;>
        Additionally, you may choose options to handle missing values, switch to English labels or add parallel spells information.
    </p>

    <h5 style=font-size: 16px; color: #007BFF;>Add exemplary data preparation</h5>
    <p style=font-size: 18px; color: #555;>
        On top of that,  you might add exemplary data preparation of variables from modules that cannot be directly merged with the person-year dataset.
    </p>

        <h5 style=font-size: 16px; color: #007BFF;>Variable labels language</h5>
    <p style=font-size: 18px; color: #555;>
        You may also switch the language of the variable labels in the script.
    </p>

    <h5 style=font-size: 16px; color: #007BFF;>Spell Prioritization</h5>
    <p style=font-size: 18px; color: #555;>
        Furthermore, you can alter the spell prioritization order using the second tab in the top right corner.
    </p>

    <h5 style=font-size: 16px; color: #007BFF;>Additional Variables</h5>
    <p style=font-size: 18px; color: #555;>
        In the third tab, you can add additional variables from various NEPS datasets to the script. Some datasets (e.g. further training related datasets) are excluded though due to additional data preparation steps required for including variables from these datsets.
    </p>

    <h5 style=font-size: 16px; color: #007BFF;>Preview and Download</h5>
    <p style=font-size: 18px; color: #555;>
        Lastly, you can preview and download the script incorporating all your adjusted settings.
    </p>

        <h5 style=font-size: 16px; color: #007BFF;>Optional: Provide Datapath</h5>
    <p style=font-size: 18px; color: #555;>
        If you wish, you can provide the local file path to the NEPS SUF data (in .dta format) so it is immediately available in your script. Alternatively, you can insert the path manually.
    </p>
</div>"
          )
        )
      )
    )),

    # Spell Prioritization panel
    bslib::nav_panel(
      title = htmltools::HTML("<span data-toggle='tooltip' title='Edit spell priorisation order in person year dataset script if desired'>Spell Prioriation</span>"),
      data_transformation_prio_ui("data_transformation")
    ),

    # Add Variables panel
    bslib::nav_panel(
      title = htmltools::HTML("<span data-toggle='tooltip' title='Add additional variables to the person year dataset script'>Additional Variables</span>"),
      htmltools::tags$head(
        htmltools::tags$style(
          ".multi-wrapper {
            height: 60vh;  /* 60% of the viewport height */
          }
          .multi-wrapper .non-selected-wrapper,
          .multi-wrapper .selected-wrapper {
            height: 90%;
          }"
        )
      ),
      data_transformation_add_variables_ui("data_transformation")
    )
  )
}
