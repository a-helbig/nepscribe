# labels for user controlled prio of episodes
.labels <- list(
  "22 - School",
  "23 - Vocational Preparation",
  "24 - Vocational Training",
  "25 - Military or Civil Service",
  "26 - Employment",
  "27 - Unemployment",
  "29 - Parental Leave",
  "30 - Gap",
  "31 - Licence Courses (Voc. Train.)",
  "99 - Data-Edition Gap"
)

# Custom text for the buttons
.buttons = list(
  list(extend = 'copy', text = 'Copy Data'),
  list(extend = 'csv', text = 'Download CSV'),
  list(extend = 'excel', text = 'Download Excel')
)

.captiontext <- shiny::div(shiny::strong("Loading"), shiny::br(), "Please wait")


