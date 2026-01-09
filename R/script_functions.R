
#' helper function to put these further education prep lines into multiple script versions. R.
#' @keywords internal
#' @noRd
further_training_gen_r <- function(english, suf_version_short) {
  lines_of_code <-  c(
    "# Add indicators on Further Training Participation ---------------------------------------",
    "",
    "# explanations and further details on the data preparation: https://www.lifbi.de/Portals/2/NEPS%20Survey%20Papers/NEPS-Survey-Paper_CI.pdf",
    "",
    "",
    "# Training Courses: 1. Step: spFurtherEducation data set as a base ----------------------------------",
    "",
    "# read data FurtherEducation",
    paste0("ft_data <- read_neps(paste0(datapath, 'SC6_FurtherEducation_D_', suf_version,'.dta'), english = ", english, ")"),
    "",
    "# set NA values for the date variables so that we can compute harmonized date vars and get rid of courses without valid dates",
    "ft_data <- ft_data  |> ",
    "  replace_values_with_na(vars = c('tx2821y', 'tx2821m', 'tx2822y', 'tx2822m'))",
    "",
    "# generate date variables",
    "ft_data <- ft_data |> ",
    "  mutate(start = ((ft_data$tx2821y - 1960) * 12) + ft_data$tx2821m - 1,",
    "         end = ((ft_data$tx2822y - 1960) * 12) + ft_data$tx2822m - 1) |>  #startdate in months since jan. 1960",
    "  filter(!is.na(start) & !is.na(end))",
    "",
    "# no course- and further training-module in wave 1 ('ALWA'), get rid of this wave",
    "ft_data <- ft_data |> ",
    "  filter(wave > 1)",
    "",
    "# Training Courses: 2. Step: Merge  cohort profile ---------------------------------",
    "",
    "# we need the interview date from this module to identify and delete retrospective courses, that were recorded more than 2 years ago.",
    "",
    "# read data cohortprofile in order to get access to interview dates",
    paste0("interview_data <- read_neps(paste0(datapath, 'SC6_CohortProfile_D_', suf_version,'.dta'), english = ", english, ") |> "),
    if(suf_version_short %in% c(8:11))" select(ID_t, wave, inty, intm) |> rename(tx8600m = intm, tx8600y = inty) |> ",
    if(suf_version_short %in% c(12:14))" select(ID_t, wave, tx8600y, tx8600m) |>",
    if(suf_version_short > 14)"  select(ID_t, wave, tx8601y, tx8601m) |> rename(tx8600m = tx8601m, tx8600y = tx8601y) |> ",
    "    filter(wave > 1)",
    "",
    "# take 'FurtherEducation' dataset as a base for the merging with cohort profile",
    "ft_data <-",
    "  left_join(ft_data,",
    "               interview_data,",
    "               by = c('ID_t', 'wave'))",
    "",
    "# generate interview date",
    "ft_data <- ft_data |>",
    "mutate(interv = ((tx8600y - 1960) * 12) + tx8600m - 1,",
    "timegap = interv-end)  |>",
    "select(-tx8600y,-tx8600m)",
    "",
    "# remove interview_data",
    "rm(interview_data)",
    "",
    "# some courses ended a long time before the interview.",
    "tabyl(ft_data$timegap)",
    "",
    "# 1. respondents who didnt participate in the survey in one wave -> reported episodes since their last interview which might be up to 2-3 years ago",
    "# 2. in wave 2 and 4 all episodes of vocational training were registered retrospectively -> reported vocational training episodes that were classified as courses (from spVocTrain) possibly ended years before the interview",
    "",
    "# get rid of courses that ended more than two years before interview",
    "ft_data <- ft_data |> filter((interv-end < 25) & (interv-end >= 0))",
    "",
    "# keep it simple",
    "ft_data <- ft_data |> ",
    "  select(-c(tx2822c, tx28202_R, tx28204, tx2821m, tx2821y, tx2822m, tx2822y))",
    "",
    "# now we need to gather information on occupational vs private courses that are spread over different datasets depending on the wave where data was collected",
    "",
    "# Training Courses: 3. Step: join furtherEdu2 --------------------------------------------------------",
    "",
    "# load furtheredu2 to get access to variable t279040 on occupation/private training for waves 2-10 from detail loop",
    "",
    paste0("further_edu2 <- read_neps(paste0(datapath, 'SC6_spFurtherEdu2_D_', suf_version,'.dta'), col_select = c('ID_t', 'course', 't279040'), english = ", english, ")"),
    "",
    "# merge it with ft_data",
    "ft_data <-",
    "  left_join(ft_data,",
    "               further_edu2,",
    "               by = c('ID_t', 'course'))",
    "",
    "rm(further_edu2)",
    "",
    "# Training Courses: 4. Step: join furtherEdu1 --------------------------------------------------------",
    "",
    "# load furtheredu1 to get access to variable t279040 on occupation/private training for waves 10+ from further training module",
    paste0("further_edu1 <- read_neps(paste0(datapath, 'SC6_spFurtherEdu1_D_', suf_version,'.dta'), col_select = c('ID_t', 'wave', 'course', 't279040'), english = ", english, ")"),
    "",
    "further_edu1 <- further_edu1 |> ",
    "  filter(wave > 10)  # keep only waves above 10",
    "",
    "ft_data <-  left_join(ft_data, further_edu1, by = c('ID_t', 'wave', 'course'))",
    "ft_data <- replace_values_with_na(ft_data, vars = c('t279040.x', 't279040.y'))",
    "",
    "rm(further_edu1)",
    "",
    "# variable harmonization required, because we left_joined variables that were existent in both datasets but were not specified in the 'by' argument. Get rid of of t279040.x and t279040.y vars ",
    "suppressWarnings(ft_data <- ft_data |> ",
    "mutate(t279040 = dplyr::coalesce(t279040.x, t279040.y)) |> ",
    "select(-t279040.x,-t279040.y))",
    "",
    "# Training Courses: 5. Step: join spcourses ----------------------------------------------------------",
    "",
    "# load course data to get access to variable t279040 on occupation/private training for waves 10+ from course module",
    paste0("spcourses <- read_neps(paste0(datapath, 'SC6_spCourses_D_', suf_version,'.dta'), col_select = c('ID_t','wave', paste0('t279030_w',1:5), paste0('course_w',1:5), 'sptype'), english = ", english, ")"),
    "",
    "# generate counter per ID and wave",
    "spcourses <- spcourses  |> ",
    "  group_by(ID_t, wave) |> ",
    "  mutate(n = row_number()) |> ",
    "  ungroup()",
    "",
    "# reshape to long format and then to wide in order to have t279030 and course number in wide format again",
    "spcourses <- spcourses |> ",
    "  pivot_longer(",
    "    cols = c(starts_with('t27'), starts_with('course_w')),",
    "    names_to = c('colname', 'number'),",
    "    names_pattern = '([a-z\\\\d]+)(_\\\\w\\\\d+)'",
    "  ) |> ",
    "  pivot_wider(",
    "    names_from = 'colname',",
    "    values_from = 'value' ",
    "  )",
    "",
    "# course is missing when no (further) course reported - remove!",
    "spcourses <- spcourses  |> ",
    "  filter(!is.na(course))",
    "",
    "# renaming to the names of the variables in furtherEdu1",
    "spcourses <- rename(",
    "  spcourses,",
    "  t279040 = t279030",
    ") |> ",
    "  select(-number, -n)",
    "",
    "# now we have a clean dataset with 3 identifiers: ID_t, wave, course and two variables of interest: t279040 and sptype",
    "",
    "# merge this with ft_data",
    "ft_data <- left_join(ft_data,",
    "                       spcourses,",
    "                       by = c('ID_t', 'wave', 'course'))",
    "",
    "# unite the 2 variable versions of t279040 again and deselect .x .y versions.",
    "ft_data <- ft_data |> ",
    "  mutate(t279040 = dplyr::coalesce(t279040.x,t279040.y)) |> ",
    "  select(-c(t279040.x,t279040.y))",
    "",
    "# Categorization of Further Education activities in private/job",
    "",
    "# generate private/job-related dummy variable",
    "ft_data <- ft_data |> ",
    "  mutate(jobrelated_course = NA, ",
    "         jobrelated_course = ifelse(t279040 == 2, 0, jobrelated_course),",
    "         jobrelated_course = ifelse(t279040 %in% c(1,3), 1, jobrelated_course))",
    "",
    "# remove spcourses obj",
    "rm(spcourses)",
    "",
    "# missing information on private or job-related course motivation, mostly below wave 11 ",
    "ft_data |> ",
    "  janitor::tabyl(wave, jobrelated_course, show_na=T)",
    "",
    "# before wave 11, data on whether the course was taken for personal or job-related reasons was collected for a maximum of two courses per person (spFurtherEdu2) during the detail loop. So we have quite some courses that have missings here. We will now use the context in which the course was mentioned to determine whether the course was occupational or not. If the course is associated with a vocational training episode, employment episode, or unemployment episode, it will be classified as occupational. For further details, please refer to the paper by Gruettgen et al. (2023).",
    "",
    "# Training Courses: 6. Step: edit jobrelated_course variable according to context --------",
    "",
    "# if info on job related is NA, we check if its a course form voctrain module or a course from course module reported in the context of voctrain, employment or unemployment",
    "ft_data <- ft_data |>",
    "mutate(",
    "jobrelated_course = case_when(",
    "(tx28200 == 24 & is.na(jobrelated_course)) ~ 1,",
    "(tx28200 == 35 & sptype %in% c(24, 26, 27) & is.na(jobrelated_course)) ~ 1,",
    "TRUE ~ jobrelated_course",
    ")",
    ")",
    "",
    "",
    "# Training Courses: 7. Step: Recoding and final indicator preparation --------",
    "",
    "# for the rest of the courses with NA in jobrelated_course, we assume that these were not for occupational reasons",
    "ft_data <- ft_data |> ",
    "  mutate(jobrelated_course = case_when(",
    "    is.na(jobrelated_course) ~ 0,       # Set NA to 0",
    "    TRUE ~ jobrelated_course            # Leave other values unchanged",
    "  ))",
    "",
    "# set missings for training hours variable",
    "ft_data <- ft_data |> ",
    "  replace_values_with_na(vars = 'tx28203')",
    "",
    "# generate dummy variable for participation in at least 1 course per wave",
    "ft_data <- ft_data |> ",
    "  group_by(ID_t,wave) |> ",
    "  mutate(jobrelated_course = max(jobrelated_course),",
    "         jobrelated_course_n  = sum(jobrelated_course),",
    "         jobrelated_course_hours = max(sum(tx28203[jobrelated_course == 1], na.rm = TRUE))) |> ",
    "ungroup()",
    "",
    "# reduce to main vars of interest and to unique rows across ID_t and wave",
    "ft_data <- ft_data |> ",
    "  group_by(ID_t,wave) |> ",
    "  select(ID_t, wave, jobrelated_course, jobrelated_course_n, jobrelated_course_hours) |> ",
    "  filter(row_number()==1)",
    "",
    "# make labels for generated variables: ",
    "",
    if(english)"# Change class of var to labelled dbl",
    if(english)"ft_data$jobrelated_course <- haven::labelled(ft_data$jobrelated_course, labels = c(No = 0, Yes = 1), label = 'Job-related training course'
)",
    if(english)"",
    if(english)"# Assign varlabel to variable",
    if(english)"attr(ft_data$jobrelated_course_n, 'label') <- 'Number of job-related training courses'",
    if(english)"",
    if(english)"# Assign varlabel to variable",
    if(english)"attr(ft_data$jobrelated_course_hours, 'label') <- 'Hours spent in job-related training courses'",
    if(!english)"# Change class of var to labelled dbl",
    if(!english)"ft_data$jobrelated_course <- haven::labelled(ft_data$jobrelated_course, labels = c(Nein = 0, Ja = 1), label = 'Berufsbezogene Weiterbildung'
)",
    if(!english)"",
    if(!english)"# Assign varlabel to variable",
    if(!english)"attr(ft_data$jobrelated_course_n, 'label') <- 'Anzahl berufsbezogener Weiterbildungen'",
    if(!english)"",
    if(!english)"# Assign varlabel to variable",
    if(!english)"attr(ft_data$jobrelated_course_hours, 'label') <- 'Berufsbezogene Weiterbildung: Teilnahme in Stunden'",
    "",
    "# join training data to bio",
    "bio <- left_join(bio,ft_data, by=c('ID_t','wave'))",
    "",
    "# set NA values in the further training indicators to 0, these are essential waves, where no course was reported, while 0 in further training preparation actually meant that there was a course but no jobrelated course",
    "bio <- bio |> ",
    "mutate(across(c('jobrelated_course', 'jobrelated_course_n', 'jobrelated_course_hours'), ~ tidyr::replace_na(., 0)))",
    "",
    "# illustrative inspection of the new job related training dummy variable",
    "bio |>",
    "tabyl(wave, jobrelated_course) |>",
    "adorn_percentages('row') |>",
    "adorn_pct_formatting(digits = 2) |>",
    "adorn_ns()",
    "",
    "rm(ft_data)"
  )
}


#' helper function to put these further education prep lines into multiple script versions. Stata.
#' @keywords internal
#' @noRd
further_training_gen_stata <- function(english, suf_version_short) {
  lines_of_code <-  c(
    "********************************************************************************",
    "* Add indicators on further training participation",
    "******************************************************************************** ",
    "",
    "* Explanations and further details on data preparation of further training in NEPS: https://www.lifbi.de/Portals/2/NEPS%20Survey%20Papers/NEPS-Survey-Paper_CI.pdf",
    "",
    "********************************************************************************",
    "* Training Courses: 1. Step: spFurtherEducation data set as a base ",
    "******************************************************************************** ",
    "",
    "tempfile person_year_data",
    "save `person_year_data'",
    "",
    "// using the FurtherEducation-Data set as basis",
    "use \"$DATA\\SC6_FurtherEducation_D_$suf.dta\", clear",
    "",
    "* set missings",
    "foreach var of varlist tx2821y tx2821m tx2822y tx2822m {",
    "\treplace `var' = missing() if `var' == -55",
    "}",
    "",
    "* generate start/end date vars",
    "gen start=ym(tx2821y, tx2821m)",
    "\tformat start %tm",
    "gen ende=ym(tx2822y, tx2822m)",
    "\tformat ende %tm",
    "order start ende, after (tx28200)\t\t",
    "keep if start < . & ende < .\t\t",
    "",
    "// no course- and further training-module in wave 1 ('ALWA'), get rid of this wave",
    "drop if wave == 1",
    "",
    "********************************************************************************",
    "* Training Courses: 2. Step: Merge  cohort profile ",
    "********************************************************************************",
    "",
    "* we need the interview date from this module to identify and delete retrospective courses, that were recorded more than 2 years ago.",
    "",
    "preserve",
    "use \"$DATA\\SC6_CohortProfile_D_$suf.dta\", clear",
    if(suf_version_short %in% c(8:11))"keep ID_t wave inty intm",
    if(suf_version_short %in% c(8:11))"rename inty tx8600y",
    if(suf_version_short %in% c(8:11))"rename intm tx8600m",
    if(suf_version_short %in% c(12:14))"keep ID_t wave tx8600y tx8600m",
    if(suf_version_short > 14)"keep ID_t wave tx8601y tx8601m",
    if(suf_version_short > 14)"rename tx8601y tx8600y",
    if(suf_version_short > 14)"rename tx8601m tx8600m",
    "drop if wave == 1 ",
    "",
    "gen interv=ym(tx8600y, tx8600m) // interview date in each wave",
    "format interv %tm",
    "",
    "tempfile intd",
    "save `intd'",
    "restore",
    "",
    "merge m:1 ID_t wave using `intd', ///",
    "\tkeepusing(interv) keep(master match) nogen",
    "",
    "********************************************************************************",
    "* some courses ended a long time before the interview. Why?",
    "",
    "* 1. respondents who didnt participate in the survey in one wave -> reported episodes since their last interview which might be up to 2-3 years ago",
    "* 2. in wave 2 and 4 all episodes of vocational training were registered retrospectively -> reported vocational training episodes that were classified as courses (from spVocTrain) possibly ended years before the interview",
    "",
    "* get rid of courses that ended more than two years before interview",
    "keep if (interv-ende < 25) & (interv-ende >=0)",
    "",
    "* keep it simple",
    "drop tx2822c tx28202_R tx28204 tx2821m tx2821y tx2822m tx2822y",
    "",
    "* now we need to gather information on occupational vs private courses that are spread over different datasets depending on the wave where data was collected",
    "",
    "********************************************************************************",
    "* Training Courses: 3. Step: join furtherEdu2",
    "********************************************************************************",
    "",
    "* professional/private reasons, attendance during working hours, course costs Employer",
    "merge m:1 ID_t course using \"$DATA/SC6_spFurtherEdu2_D_$suf.dta\", keepusing(ID_t course t279040) // merge 1:1 is not possible because of missing values in course-variable for most courses from vocTrain",
    "",
    "drop if _merge==2 // 14 infos from using can't be linked (these are courses with missing start or endddates, which we dropped at the beginning)",
    "drop _merge\t\t\t\t",
    "",
    "********************************************************************************",
    "* Training Courses: 4. Step: join furtherEdu1 ",
    "********************************************************************************",
    "",
    "* set -54 to missing here in order to be able to update it with the update option in merging below",
    "replace  t279040 =.m if  t279040 ==-54",
    "",
    "preserve ",
    "",
    "* load furtheredu1 to get access to variable t279040 on occupation/private training for waves 10+ from further training module",
    "use \"$DATA/SC6_spFurtherEdu1_D_$suf.dta\", clear",
    "",
    "keep if wave > 10 // information only to be added for courses from wave 11 on",
    "",
    "tempfile fe1",
    "save `fe1'",
    "",
    "restore",
    "",
    "* merge furtherEdu1",
    "merge m:1 ID_t wave course using `fe1', ///",
    "\tkeepusing(t279040) nogen update // update variables in FurtherEducation if wave>=11",
    "",
    "",
    "********************************************************************************",
    "* Training Courses: 5. Step: join spcourses",
    "********************************************************************************",
    "",
    "* load course data to get access to variable t279040 on occupation/private training for waves 10+ from course module",
    "preserve ",
    "",
    "use \"$DATA/SC6_spCourses_D_$suf.dta\", clear",
    "",
    "bys ID_t wave: gen n =_n",
    "keep ID_t wave t279030_w* course_w* n sptype",
    "reshape long t279030_w course_w, i(ID_t wave n) j(course_nr)",
    "drop if course_w==. // course_w missing when no (further) course reported",
    "",
    "rename course_w course // rename for merging",
    "",
    "",
    "drop course_nr n",
    "",
    "tempfile co",
    "save `co'",
    "",
    "restore",
    "",
    "merge m:1 ID_t wave course using `co', ///",
    "\tkeepusing(t279030_w sptype) gen(_mergeco) keep(master match)",
    "",
    "* set missings",
    "replace t279040 =. if inlist(t279040, -97,-98)",
    "replace t279030_w =. if inlist(t279040, -97,-98, -54)",
    "",
    "",
    "* unite the 2 variable of interest versions: t279040 t279030_w  ",
    "replace t279040 = t279030_w if missing(t279040) & !missing(t279030_w)",
    "drop t279030_w",
    "",
    "tab wave t279040",
    "",
    "* variable harmonization required, because we merged variables that were existent in both datasets but were not specified in the 'by' argument. Get rid of of t279040.x and t279040.y vars ",
    "",
    "assert _mergeco!=2\t",
    "drop _mergeco",
    "",
    "********************************************************************************",
    "* Categorization of Further Education activities in private/job",
    "",
    "",
    "* generate private/job-related dummy variable",
    "gen jobrelated_course = .",
    "replace jobrelated_course = 0 if t279040==2",
    "replace jobrelated_course = 1 if t279040==1 | t279040==3 // \"both\" = job-related",
    "",
    "tab wave jobrelated_course, mis",
    "",
    "",
    "* before wave 11, data on whether the course was taken for personal or job-related reasons was collected for a maximum of two courses per person (spFurtherEdu2) during the detail loop. So we have quite some courses that have missings here. We will now use the context in which the course was mentioned to determine whether the course was occupational or not for courses where this info is still missing. If the course is associated with a vocational training episode, employment episode, or unemployment episode, it will be classified as occupational. For further details, please refer to the paper by Gruettgen et al. (2023).",
    "",
    "",
    "********************************************************************************",
    "* Training Courses: 6. Step: edit jobrelated_course variable according to context ",
    "********************************************************************************",
    "",
    "// before wave 11 data on whether course was for private or job-related reasons only collected for (up to) 2 courses per person (spFurtherEdu2) (see survey paper for details)",
    "// -> option to distinguish between private and professional Further Training for missing values based on corresponding life episode of reported course",
    "",
    "* if info on job related is missing, we check if its a course form voctrain module or a course from course module reported in the context of voctrain, employment or unemployment",
    "",
    "// replace variable with addition of job-related courses depending on context",
    "replace jobrelated_course=1 if tx28200==24 & jobrelated_course==.  // courses stated as vocational training",
    "",
    "replace jobrelated_course=1 if tx28200==35 & inlist(sptype ,24,26,27) & jobrelated_course==. // courses registered in the course-module and mentioned in context of employment, unemployment or vocational training (from wave 12 on licence courses mentioned in the module for vocational training (24) go into the course-module-loop, so they are included in spCourses (35))",
    "tab wave jobrelated_course, mis",
    "",
    "********************************************************************************",
    "* Training Courses: 7. Step: Recoding and final indicator preparation",
    "********************************************************************************",
    "",
    "* For the rest of the courses with NA in jobrelated_course, we assume that these were not for occupational reasons",
    "replace  jobrelated_course = 0 if missing(jobrelated_course)",
    "",
    "* Set missings for training hours variable",
    "replace tx28203 = . if tx28203 < 0 ",
    "",
    "fre jobrelated_course",
    "",
    "********************************************************************************",
    "* Generate training indicators",
    "",
    "* Generate dummy variable for participation in at least 1 course per wave",
    "bysort ID_t wave: egen max_jobrelated_course = max(jobrelated_course)",
    "replace jobrelated_course = max_jobrelated_course",
    "drop max_jobrelated_course",
    "* Create jobrelated_course_n as sum within group",
    "bys ID_t wave: egen jobrelated_course_n = total(jobrelated_course)",
    "* Create helper variable: tx28203 only when jobrelated_course == 1, else 0",
    "gen tx28203_jobrelated = tx28203 if jobrelated_course == 1",
    "replace tx28203_jobrelated = 0 if tx28203_jobrelated == .",
    "* Sum tx28203_jobrelated within group",
    "bysort ID_t wave: egen jobrelated_course_hours = total(tx28203_jobrelated)",
    "drop  tx28203_jobrelated ",
    "",
    "* Reduce to main vars of interest and to unique rows across ID_t and wave  ",
    "keep ID_t wave jobrelated_course jobrelated_course_n jobrelated_course_hours",
    "bys ID_t wave: keep if _n == 1",
    "",
    "* Labels for generated variables: ",
    "",
    if(!english)"label var jobrelated_course \"Berufsbezogene Weiterbildung\"",
    if(!english)"label define jobrelated_course 0 \"Nein\" 1 \"Ja\"",
    if(!english)"label val jobrelated_course jobrelated_course",
    "",
    if(english) "label var jobrelated_course \"Job-related training\"",
    if(english) "label define jobrelated_course 0 \"No\" 1 \"Ja\"",
    if(english) "label val jobrelated_course jobrelated_course",
    "",
    if(!english)"label var jobrelated_course_n \"Anzahl berufsbezogener Weiterbildungen\"",
    if(english)"*label var jobrelated_course_n \"Number of job-related training courses\"",
    "",
    if(!english)"label var jobrelated_course_hours \"Berufsbezogene Weiterbildung: Teilnahme in Stunden\"",
    if(english)"*label var jobrelated_course_hours \"Job-related training hours\"",
    "",
    "*  set NA values in jobrelated_course_hours to 0 hours",
    "replace jobrelated_course_hours = 0 if jobrelated_course_hours == .",
    "",
    "tempfile ft_data",
    "save `ft_data'",
    "",
    "use `person_year_data', clear",
    "",
    "merge 1:1 ID_t wave using `ft_data', keep(master match)",
    "",
    "* set NA values in the further training indicators to 0, these are essential waves, where no course was reported, while 0 in further training preparation actually meant that there was a course but no jobrelated course",
    "replace jobrelated_course = 0 if jobrelated_course == .",
    "",
    "* illustrative inspection of the new job related training dummy variable",
    "tab wave jobrelated_course, row"
  )
}

#' function to generate educational qualification preparation. Stata.
#' @keywords internal
#' @noRd
gen_parallel_spells_stata <- function(format="harmonized") {
  # Create a character vector to hold the lines of code
  lines_of_code <- c(
    "",
    "*******************************************************************************",
    "* Paralell  Spells Data Prep",
    "*******************************************************************************",
    "",
    "* There can be multiple spells simultaneously, so multiple columns for parallel spells are generated per variable.",
    "",
    "* group by ID_t and month and keep the sorting from above. Then generate a month counter per ID_t and month",
    if(format=="harmonized") "by ID_t month: gen n = _n",
    if(format=="subspell") "by ID_t wave: gen n = _n",
    "* We set maximum to 7 parallel spells because there are only a handful of ids that have up to 15, which is odd anyway and the computation would take way more time for these few cases",
    "drop if n > 7",
    "* Now we summarize n and then create a local which we use in the for loop to generate the parallel sidespells",
    "quietly summarize n",
    "local maxval = r(max)",
    "forvalues i = 1/`maxval' {",
    "",
    "  * generate variable containers",
    "  gen sidespell`i'_sptype = .",
    "  gen sidespell`i'_splink = .",
    "  gen sidespell`i'_start = .",
    "  gen sidespell`i'_end = .",
    "",
    "  * Group by ID_t and month and put sptype, splink, startdate and enddatefrom next month in variable",
    if(format=="harmonized") "  bys ID_t month (n): replace sidespell`i'_sptype = sptype[_n+`i'] if n == 1",
    if(format=="harmonized") "  bys ID_t month (n): replace sidespell`i'_splink = splink[_n+`i'] if n == 1",
    if(format=="harmonized") "  bys ID_t month (n): replace sidespell`i'_start = start[_n+`i'] if n == 1",
    if(format=="harmonized") "  bys ID_t month (n): replace sidespell`i'_end = end[_n+`i'] if n == 1",
    if(format=="subspell")   "  bys ID_t wave (n): replace sidespell`i'_sptype = sptype[_n+`i'] if n == 1",
    if(format=="subspell")   "  bys ID_t wave (n): replace sidespell`i'_splink = splink[_n+`i'] if n == 1",
    if(format=="subspell")   "  bys ID_t wave (n): replace sidespell`i'_start = start[_n+`i'] if n == 1",
    if(format=="subspell")   "  bys ID_t wave (n): replace sidespell`i'_end = end[_n+`i'] if n == 1",
    "}",
    ""
  )
}

#' function to generate educational qualification preparation. R.
#' @keywords internal
#' @noRd
gen_parallel_spells_r <- function(format="harmonized") {
  # Create a character vector to hold the lines of code
  lines_of_code <- c(
    "# Paralell Spells Data Prep ----------------------------------------",
    "",
    "# There can be multiple spells simultaneously, so multiple columns for parallel spells are generated per variable.",
    "",
    if(format=="harmonized")"    # group by ID_t and month and keep the sorting from above. Then generate a month counter per ID_t and month",
    if(format=="subspell")"    # group by ID_t and wave and keep the sorting from above. Then generate a wave counter per ID_t and wave",
    "    bio <- bio |>",
    "      lazy_dt() |> # use this function from dtplyr for faster processing",
    if(format=="harmonized")"      group_by(ID_t, month) |> ",
    if(format=="subspell")"      group_by(ID_t, wave) |> ",
    "      mutate(n = row_number()) |> ",
    "      ungroup() |>",
    "      as_tibble() # required because of use of lazy_dt function",
    "",
    "    # We set maximum to 7 parallel spells because there are only a handful of ids that have up to 15, which is odd anyway and the computation would take way more time for these few cases",
    "    bio <- bio |> ",
    "      filter(n < 8)",
    "",
    "    # Now we max value of n which we use in the loop to generate the parallel sidespells",
    "    maxval <- max(bio$n)",
    "",
    "    # For each offset i, create parallel spells columns by joining offset rows within groups",
    "    for(i in 1:maxval) {",
    "      # Create a temporary data frame with the offset variables and the key columns + offset index",
    "      offset_df <- bio  |> ",
    "        mutate(n = n - i)  |>      # shift n backwards by i to align next spells with n == 1 rows",
    "        filter(n == 1)  |>         # keep only those rows that will be aligned with the \"n == 1\" rows in bio",
    if(format=="harmonized")"        select(ID_t, month, n, sptype, splink, start, end) %>%",
    if(format=="subspell")"          select(ID_t, wave, n, sptype, splink, start, end) %>%",
    "        rename(!!paste0(\"sidespell\", i, \"_sptype\") := sptype,",
    "               !!paste0(\"sidespell\", i, \"_splink\") := splink,",
    "               !!paste0(\"sidespell\", i, \"_start\") := start,",
    "               !!paste0(\"sidespell\", i, \"_end\") := end)",
    "      ",
    if(format=="harmonized")"      # Join the offset data to original bio on ID_t, month, n",
    if(format=="subspell")"        # Join the offset data to original bio on ID_t, wave, n",
    "      bio <- bio  |> ",
    if(format=="harmonized")"        left_join(offset_df, by = c(\"ID_t\", \"month\", \"n\"))",
    if(format=="subspell")"        left_join(offset_df, by = c(\"ID_t\", \"wave\", \"n\"))",
    "    }",
    "",
    "rm(offset_df)",
    ""
  )
}


#' function to generate educational qualification preparation. Stata.
#' @keywords internal
#' @noRd
gen_qualification_prep_code_stata <- function(english, sc, suf_version, suf_version_short) {
  # Create a character vector to hold the lines of code
  lines_of_code <- c(
    "*******************************************************************************",
    "* Educational Qualification",
    "*******************************************************************************",
    "preserve",
    "",
    "* load cohort profile to get access to interviewdates",
    paste0("use \"$DATA\\",sc,"_CohortProfile_D_$suf.dta\", clear"),
    "",
    if(sc %in% c("SC3","SC6"))"keep ID_t wave tx8601y tx8601m",
    if(sc %in% c("SC4","SC5"))"keep ID_t wave tx8600y tx8600m",
    "",
    if(sc %in% c("SC3","SC6"))"rename tx8601y inty",
    if(sc %in% c("SC3","SC6"))"rename tx8601m intm",
    if(sc %in% c("SC4","SC5"))"rename tx8600y inty",
    if(sc %in% c("SC4","SC5"))"rename tx8600m intm",
    "* gen interviewdate",
    "gen intdate = ym(inty,intm)",
    "",
    "drop inty intm",
    "",
    "*drop non-participation waves",
    "drop if missing(intdate)",
    "",
    "*reshape interviewdate to wideformat",
    "reshape wide intdate, i(ID_t) j(wave)",
    "",
    "tempfile intd",
    "save `intd'",
    "",
    "* load education data",
    paste0("use \"$DATA\\",sc,"_Education_D_$suf.dta\", clear"),
    "",
    "* gen education date variable that holds the date of qualification",
    "gen edud=ym(datey,datem)",
    "",
    "keep ID_t number edud tx28101 tx28103",
    "",
    "rename tx28101 casmin",
    "rename tx28103 isced",
    "",
    "*reshape qualifications to wideformat",
    "reshape wide edud casmin isced, i(ID_t) j(number)",
    "",
    "* merge interviewdata",
    "merge m:1 ID_t using `intd', keep(master match) nogen",
    "",
    "* count maximum number of qualifications",
    "local count = 0",
    "",
    "foreach var of varlist _all {",
    "  if substr(\"`var'\",1,6) == \"casmin\" {",
    "    local ++count",
    "  }",
    "}",
    "",
    "* gen cs vars that indicate the casmin achieved in wave x. It loops over all waves and generates empty indicators casmin_wave_X. Then it loops over all casmins and replaces the cs casmin indicator with the original casmin if the casmin date is smaller/equal to the intdate and the letter is not missing",
    if(sc!="SC4")"forval x = 1/$suf_short {",
    if(sc=="SC4")"forval x = 3/$suf_short {",
    "  gen wave_casmin_`x'=.",
    "  gen wave_isced_`x'=.",
    "    forval y = 1/`count' {",
    "    replace wave_casmin_`x' = casmin`y' if edud`y' <= intdate`x' & intdate`x' != ." ,
    "    replace wave_isced_`x' = isced`y' if edud`y' <= intdate`x' & intdate`x' != . ",
    "  }",
    "}",
    "",
    "drop intd* casmin* edud* isced*",
    "",
    "*reshape education indicators to long format",
    "reshape long wave_casmin_ wave_isced_, i(ID_t) j(wave)",
    "",
    "rename wave_casmin_  casmin",
    "rename wave_isced_  isced",
    "",
    "lab var casmin \"CASMIN\"",
    "lab var isced \"ISCED-97\"",
    "lab val casmin de1957",
    "lab val isced de1980",
    "",
    "* gen education groups",
    "gen education = .",
    "replace education = 1 if casmin == 0 | casmin == 1 | casmin == 3",
    "replace education = 2 if casmin == 5",
    "replace education = 3 if casmin == 2 | casmin == 4",
    "replace education = 4 if casmin == 6",
    "replace education = 5 if isced == 8",
    "replace education = 6 if casmin == 7 | casmin == 8",
    "",
    "*define labels",
    if(!english)"label variable education \"Hoechster Bildungsabschluss\"",
    if(!english)"#delimit ;",
    if(!english)"label define edulbl",
    if(!english)"1 \"1. Keine abgeschlossene Ausbildung, kein Abitur\"",
    if(!english)"2 \"2. Keine abgeschlossene Ausbildung mit Abitur\"",
    if(!english)"3 \"3. Berufsausbildung ohne Abitur\"",
    if(!english)"4 \"4. Berufsausbildung mit Abitur\"",
    if(!english)"5 \"5. Meister/Techniker/Fachschule\"",
    if(!english)"6 \"6. Akademiker (Uni/FH)\"",
    if(!english)";",
    if(!english)"#delimit cr",
    if(!english)"label values education edulbl",
    if(!english)"",
    if(english)"label variable education \"Highest educational qualification\"",
    if(english)"#delimit ;",
    if(english)"label define edulbl",
    if(english)"1 \"1. No completed vocational training, no high school diploma\"",
    if(english)"2 \"2. No completed vocational training with high school diploma\"",
    if(english)"3 \"3. Vocational training without a high school diploma\"",
    if(english)"4 \"4. Vocational training with a high school diploma\"",
    if(english)"5 \"5. Master/Technician/Specialized School\"",
    if(english)"6 \"6. Academic (University/University of Applied Sciences)\"",
    if(english)";",
    if(english)"#delimit cr",
    if(english)"label values education edulbl",
    if(english)"",
    "* save prepared education data here",
    "tempfile education",
    "save `education'",
    "",
    "* restore person-year-dataset",
    "restore",
    "",
    "* merge keeping only master and merged cases",
    "merge 1:1 ID_t wave using `education', keep(1 3) nogen",
    ""
  )}

#' function to generate educational qualification preparation. R
#' @keywords internal
#' @noRd
gen_qualification_prep_code_r <- function(english, SC, suf_version,suf_version_short) {
  lines_of_code <- c(
    "# Educational Qualification ----------------------------------------------------------",
    "",
    "# load cohort profile to get access to interviewdates",
    if(SC %in% c("SC6", "SC3"))paste0("intdata <- read_neps(paste0(datapath,'", SC, "_CohortProfile_D_", suf_version, ".dta'), col_select = c('ID_t','wave','tx8601y', 'tx8601m'), english = ", english,") |> rename(inty = tx8601y,intm = tx8601m)"),
    if(SC %in% c("SC4", "SC5"))paste0("intdata <- read_neps(paste0(datapath,'", SC, "_CohortProfile_D_", suf_version, ".dta'), col_select = c('ID_t','wave','tx8600y', 'tx8600m'), english = ", english,") |> rename(inty = tx8600y,intm = tx8600m)"),
    "",
    "# set neps missings to NA",
    "intdata <- replace_values_with_na(intdata)",
    "",
    "# gen interviewdate and drop non-participation waves",
    "intdata <- intdata |> ",
    "  mutate(intdate = ((inty - 1960) * 12) + intm -1) |> ",
    "  filter(!is.na(intdate)) |> ",
    "  select(-c(intm, inty))",
    "",
    "# pivot wider",
    "intdata <- intdata |> ",
    "  pivot_wider(names_from = 'wave', names_prefix = 'intdate', values_from= 'intdate')",
    "",
    "# load education data",
    paste0("edu_data <- read_neps(paste0(datapath,'", SC,"_Education_D_", suf_version,".dta'), english = ", english, ")"),
    "",
    "# extract value labels",
    "casmin_var_labels <- attr(edu_data$tx28101, \"label\")",
    "casmin_val_labels <- attr(edu_data$tx28101, \"labels\")",
    "isced_var_labels <- attr(edu_data$tx28103, \"label\")",
    "isced_val_labels <- attr(edu_data$tx28103, \"labels\")",
    "wave_var_labels <- attr(intdata$wave, \"label\")",
    "wave_val_labels <- attr(intdata$wave, \"labels\")",
    "",
    "# gen education date variable that holds the date of qualification",
    "edu_data <- edu_data |> ",
    "  mutate(edud = ((datey - 1960) * 12) + datem -1) |>",
    "  select(ID_t, number, tx28101, tx28103, edud) |> ",
    "  rename(casmin = tx28101,",
    "         isced = tx28103) ",
    "  ",
    "# exkurs: Reshape Edu data wide",
    "edu_data <- edu_data |> ",
    "  pivot_wider(names_from = 'number', values_from= c('casmin', 'edud', 'isced'))",
    "",
    "# merge interviewdata by ID_t",
    "edu_data <- left_join(edu_data, intdata, by = c('ID_t'))",
    "rm(intdata)",
    "",
    "# generate a count of max number of qualifications per ID",
    "# Pattern: prefix followed by one or more digits",
    "pattern <- paste0(\"^\", \"casmin_\", \"[0-9]+\")",
    "# Count column names matching pattern",
    "count <- sum(grepl(pattern, colnames(edu_data)))",
    "",
    "# Get all column names starting with 'intdate'",
    "intdate_cols <- grep('^intdate\\\\d+$', names(edu_data), value = TRUE)",
    "",
    "# Extract numeric suffixes from column names",
    "suffixes <- as.integer(str_extract(intdate_cols, '\\\\d+$'))",
    "",
    "# Get minimum suffix; if none found, set min_suffix to 1 (or whatever fallback)",
    "if (length(suffixes) > 0) {",
    "  min_suffix <- min(suffixes, na.rm = TRUE)",
    "} else {",
    "  min_suffix <- 1",
    "}",
    "# generate cs vars that indicate the casmin achieved in wave x. It loops over all waves and generates empty indicators casmin_wave_x. Then it loops over all casmins and replaces the new cs casmin indicator with the original casmin if the casmin date is smaller/equal to the intdate and the letter is not missing",
    paste0("for (x in min_suffix:",suf_version_short,") {"),
    "  wave_col_casmin <- paste0(\"casmin_wave_\", x)",
    "  wave_col_isced <- paste0(\"isced_wave_\", x)",
    "  edu_data[[wave_col_casmin]] <- NA_real_  # or NA_integer_ depending on variable type",
    "  edu_data[[wave_col_isced]] <- NA_real_  # or NA_integer_ depending on variable type",
    "  intdate_col <- paste0(\"intdate\", x)",
    "  ",
    "  for (y in 1:count) {",
    "    casmin_col <- paste0(\"casmin_\", y)",
    "    isced_col <- paste0(\"isced_\", y)",
    "    edud_col <- paste0(\"edud_\", y)",
    "    ",
    "    # Logical condition to update casmin_wave_x",
    "    condition <- (!is.na(edu_data[[intdate_col]])) &",
    "      (edu_data[[edud_col]] <= edu_data[[intdate_col]]) &",
    "      !is.na(edu_data[[edud_col]])",
    "    ",
    "    # Replace casmin_wave_x where condition is TRUE",
    "    edu_data[[wave_col_casmin]][condition] <- edu_data[[casmin_col]][condition]",
    "    edu_data[[wave_col_isced]][condition] <- edu_data[[isced_col]][condition]",
    "  }",
    "}",
    "",
    "# pivot education indicators to long format",
    "edu_data <- edu_data %>%",
    "  select(ID_t, starts_with(\"casmin_wave_\"), starts_with(\"isced_wave_\")) %>%",
    "  pivot_longer(",
    "    cols = -ID_t,                     # all except ID_t",
    "    names_to = c(\".value\", \"wave\"),   # .value 'unpacks' values to separate columns",
    "    names_pattern = \"(casmin|isced)_wave_(\\\\d+)\"  # regex to extract 'casmin' or 'isced' and wave number",
    "  )",
    "",
    "# label these vars",
    "edu_data$casmin <- haven::labelled(edu_data$casmin, labels = casmin_val_labels, label = casmin_var_labels)",
    "edu_data$isced <- haven::labelled(edu_data$isced, labels = isced_val_labels, label = isced_var_labels)",
    "",
    "# recode education groups, so we get a less detailed indicator",
    "edu_data <- edu_data %>%",
    "  mutate(education = case_when(",
    "    isced == 8             ~ 5,",
    "    casmin %in% c(0, 1, 3) ~ 1,",
    "    casmin == 5            ~ 2,",
    "    casmin %in% c(2, 4)    ~ 3,",
    "    casmin == 6            ~ 4,",
    "    casmin %in% c(7, 8)    ~ 6,",
    "    TRUE                   ~ NA_real_",
    "  ))",
    "",
    if(!english)"# create labels vector (names = codes)",
    if(!english)"edulbl <- c(",
    if(!english)"  \"1. Keine abgeschlossene Ausbildung, kein Abitur\"     = 1,",
    if(!english)"  \"2. Keine abgeschlossene Ausbildung mit Abitur\"       = 2,",
    if(!english)"  \"3. Berufsausbildung ohne Abitur\"                     = 3,",
    if(!english)"  \"4. Berufsausbildung mit Abitur\"                      = 4,",
    if(!english)"  \"5. Meister/Techniker/Fachschule\"                     = 5,",
    if(!english)"  \"6. Akademiker (Uni/FH)\"                              = 6",
    if(!english)")",
    if(!english)"",
    if(english)"edulbl <- c(",
    if(english)"  \"1. No completed vocational training, no high school diploma\"  = 1,",
    if(english)"  \"2. No completed vocational training with high school diploma\" = 2,",
    if(english)"  \"3. Vocational training without a high school diploma\"         = 3,",
    if(english)"  \"4. Vocational training with a high school diploma\"            = 4,",
    if(english)"  \"5. Master/Technician/Specialized School\"                      = 5,",
    if(english)"  \"6. Academic (University/University of Applied Sciences)\"      = 6",
    if(english)")",
    "",
    "# Convert education to haven labelled class with labels ",
    if(!english)"edu_data$education <- labelled(",
    if(!english)"  edu_data$education,",
    if(!english)"  labels = edulbl,",
    if(!english)"  label = \"Hoechster Bildungsabschluss\")",
    if(english)"edu_data$education <- labelled(",
    if(english)"  edu_data$education,",
    if(english)"  labels = edulbl,",
    if(english)"  label = \"Highest Qualification\")",
    "",
    "# Convert wave var to haven labelled double class, otherwise we cant merge with bio",
    "edu_data$wave <- as.numeric(edu_data$wave)",
    "edu_data$wave <- labelled(",
    "edu_data$wave,",
    "labels = wave_val_labels,",
    "label = wave_var_labels)",
    "",
    "# join qualifications to person-year dataset",
    "bio <- left_join(bio, edu_data, by =c(\"ID_t\",\"wave\"))",
    "rm(edu_data)"
  )
}

#' function to generate child example prep. R SC5 SC6.
#' @keywords internal
#' @noRd
gen_children_example_r_sc5_6 <- function(english, sc, suf_version){
  lines_of_code <- c(
    "# Children exemplary data prep------------------------------------",
    "",
    "# Goal: prepare dataset for merging with person-year-data where we get info about the count of studying children for each wave (expecting 0 for most IDs). ",
    "# Approach: We will reduce the child dataset to ids with children and only waves where at least 1 child studied. Then we generate a count variable to count how many studying children people have. Then we join this dataset to biography and overwrite NAs in the studying child count indicator with 0 for all those that have no studying children.",
    "",
    "# load data",
    "",
    "# in order to get children info on age, sex, type, we need to take child dataset. Here we only get info on biological children",
    paste0("child <- read_neps(paste0(datapath, ","'", sc, "_spChild_D_', suf_version, '.dta'), english = ", english, ")"),
    "",
    "# print example of dataset structure ",
    if(sc== "sc6")"print(child |> select(ID_t, wave, child, subspell, ts33210) |> filter(ID_t == 8000338), n=30)",
    if(sc== "sc5")"print(child |> select(ID_t, wave, child, subspell, ts33210) |> filter(ID_t == 7008482), n=30)",
    "",
    "# data prep",
    "child <- child |> ",
    "  filter(subspell !=0 & ts33210 == 8) |> # delete harmonized spells and keep waves with children that study",
    "  mutate(studying_children = 1) |> # set flag",
    "  select(ID_t, wave, studying_children) |> # reduce",
    "  group_by(ID_t, wave) |> # group_by id_t and wave",
    "  mutate(studying_children = sum(studying_children)) |> # sum up children per id/wave",
    "  filter(row_number() == 1) |> # reduce to unique waves",
    "  ungroup()",
    "",
    "# label the new variable",
    if(!english) "child$studying_children <- haven::labelled(child$studying_children, label = 'Anzahl studierender Kinder')",
    if(english) "child$studying_children <- haven::labelled(child$studying_children, label = 'Count of studying children')",
    "",
    "# inspect data structure (of a person with more than 1 studying child at some waves)",
    if(sc== "sc6")"print(child |> filter(ID_t == 8000338))",
    if(sc== "sc5")"print(child |> filter(ID_t == 7008482))",
    "",
    "# join child data prep example to person-year dataset",
    "bio <- left_join(bio, child, by =c('ID_t','wave'))",
    "",
    "# Set NA values in studying children to 0",
    "bio <- bio |>",
    "mutate(studying_children = tidyr::replace_na(studying_children, 0))",
    ""
  )
}

#' function to generate child example prep. R SC3 SC4.
#' @keywords internal
#' @noRd
gen_children_example_r_sc3_4 <- function(english, sc, suf_version){
  lines_of_code <- c(
    "# Exemplary children data preparation -----------------------------------",
    "",
    "# Goal: prepare dataset for merging with person-year-data where we get info about children taken care of at home. this indicator is 1 if one more children are taken care of at home and 0 if the child or all children are in childcare",
    "",
    "# load dataset child",
    paste0("child <- read_neps(paste0(datapath, ","'", sc, "_spChild_D_', suf_version, '.dta'), english = ", english, ")"),
    "",
    "# first filter for children that are alive and live in household. Note that we need to handle NA values explicitly, because we dont want to remove observations with NA values in these indicators",
    "child <- child |> ",
    "  filter(",
    "    !((!is.na(ts33310) & ts33310 == 2) | (died == 1) | (ts33205 == 2))",
    "  )",
    "",
    "# Generate childcare at home indicator",
    "child <- child |> ",
    "  mutate(childcare_at_home_child_level = NA_integer_, # first set the new indicator to NA (int)",
    "         childcare_at_home_child_level = if_else(!is.na(ts2741a), 1L, childcare_at_home_child_level), # set to int 1 if the relevant variable is not NA",
    "         childcare_at_home_child_level = if_else(rowSums(across(c(ts2741a, ts2741b, ts2741c, ts2741d, # set to int 0 if any of the given childcare indicators is 1, because then we have external childcare ",
    "                                                                  ts2741e, ts2741f, ts2741g, ts2741h,",
    "                                                                  ts2741i, ts2741j)) == 1, na.rm = TRUE) > 0,",
    "                                                 0L, childcare_at_home_child_level))",
    "",
    "# now generate an indicator on the wave level if there is any child taken care of at home in this wave",
    "# Step 1: compute max per group ",
    "childcare_max <- child |> ",
    "  group_by(ID_t, wave) |> ",
    "  summarize(",
    "    childcare_at_home = if (all(is.na(childcare_at_home_child_level))) NA_real_ else max(childcare_at_home_child_level, na.rm = TRUE),",
    "    .groups = 'drop'",
    "  )",
    "",
    "# Step 2: join back to original data",
    "child <- child |> ",
    "  left_join(childcare_max, by = c('ID_t', 'wave'))",
    "",
    "# inspect example case",
    if(sc=="SC4")"print(child |> select(ID_t, wave, child, ts33103, childcare_at_home_child_level, childcare_at_home) |> filter(ID_t ==  4011830))",
    if(sc=="SC3")"print(child |> select(ID_t, wave, child, ts33103, childcare_at_home_child_level, childcare_at_home) |> filter(ID_t ==  4007552))",
    "",
    "# reduce to distinct observations per ID_t and wave in order to be able to merge the dummy indicator to the person year dataset,",
    "child <- child  |> ",
    "  group_by(ID_t, wave)  |> ",
    "  filter(row_number() == 1) |> ",
    "  ungroup()",
    "",
    "# remove obs with missings in childcare_at_home - only a few cases that are most likely data errors.",
    "print(child |> select(ID_t, wave, child, childcare_at_home, disagint, disagwave, ts33103) |> filter(is.na(childcare_at_home)))",
    "child <- child |> ",
    "  filter(!is.na(childcare_at_home))",
    "",
    "# reduce vars",
    "child <- child |> ",
    "  select(ID_t, wave, childcare_at_home)",
    ""
  )
}

#' function to generate child example prep. Stata SC5 SC6.
#' @keywords internal
#' @noRd
gen_children_example_stata_sc5_6 <- function(english, sc, suf_version){
  lines_of_code <- c(
    "*******************************************************************************",
    "* Exemplary children data preparation",
    "*******************************************************************************",
    "",
    "* Goal: prepare dataset for merging with person-year-data where we get info about the count of studying children for each wave (expecting 0 for most IDs).",
    "* Approach: We will reduce the child dataset to ids with children and only waves where at least 1 child studied. Then we generate a count variable to count how many studying children people have. Then we merge this dataset to biography and overwrite NAs in the studying child count indicator with 0 for all those that have no studying children.",
    "",
    "preserve",
    "",
    "* Load child dataset",
    paste0("use \"$DATA\\", sc, "_spChild_D_", suf_version, ".dta\", clear"),
    "",
    "*  keep only rows where subspell != 0 & ts33210 == 8",
    "keep if subspell != 0 & ts33210 == 8",
    "",
    "* Create a flag for studying children",
    "gen studying_children = 1",
    "",
    "* Keep only necessary variables",
    "keep ID_t wave studying_children",
    "",
    "* Generate count of children per ID_t-wave combination",
    "bysort ID_t wave: gen studying_children_count = sum(studying_children)",
    "",
    "* Keep last row per ID_t-wave with the total sum per group",
    "bysort ID_t wave (studying_children_count): keep if _n == _N",
    "",
    "* inspect example case",
    if(sc== "sc6") "list if ID_t == 8000338",
    if(sc== "sc5") "list if ID_t == 7008482",
    "",
    "* drop old studying_children variable",
    "drop studying_children",
    "",
    "* Rename count variable to studying_children",
    "rename studying_children_count studying_children",
    "",
    "* Label the variable,",
    if(english)"label variable studying_children \"Count of studying children\"",
    if(!english)"label variable studying_children \"Anzahl studierender Kinder\"",
    "",
    "* Save this prepared child dataset temporarily",
    "tempfile child_prep",
    "save `child_prep', replace",
    "",
    "* restore person-year-dataset",
    "restore",
    "",
    "* now merge child prep to person-year-dataset",
    "merge 1:1 ID_t wave using `child_prep', nogen keep(master match)",
    "",
    "* Replace missing studying_children values with 0 (i.e., those without any studying child)",
    "replace studying_children = 0 if missing(studying_children)",
    ""
  )
}

#' function to generate child example prep. Stata SC3 SC4.
#' @keywords internal
#' @noRd
gen_children_example_stata_sc3_4 <- function(english, sc, suf_version){
  lines_of_code <- c(
    "*******************************************************************************",
    "* Exemplary children data preparation",
    "*******************************************************************************",
    "",
    "* Goal: prepare dataset for merging with person-year-data where we get info about children taken care of at home. this indicator is 1 if one more children are taken care of at home and 0 if the child or all children are in childcare",
    "",
    "preserve",
    "",
    "* load dataset child",
    paste0("use \"$DATA\\", sc, "_spChild_D_", suf_version, ".dta\", clear"),
    "",
    "*first drop children outside of household and deceased children",
    "drop if ts33310 == 2 | died == 1 | ts33205 == 2",
    "",
    "* Generate childcare at home indicator",
    "gen childcare_at_home_child_level = .",
    "replace childcare_at_home_child_level = 1 if ts2741a != . // if the first childcare variable is not missing, we set this to 1 because now we know that we have information on childcare. We then replace it with 0 in the next step if there is external childcare",
    "foreach var of varlist ts2741a ts2741b ts2741c ts2741d ts2741e ts2741f ts2741g ts2741h ts2741i ts2741j {",
    "replace childcare_at_home_child_level = 0 if `var' == 1",
    "}",
    "",
    "* now generate an indicator on the wave level if there is any child taken care of at home in this wave
    bysort ID_t wave: egen childcare_at_home = max(childcare_at_home_child_level) ",
    "",
    if(sc=="SC4")"* inspect example case sc4",
    if(sc=="SC3")"* inspect example case sc3",
    if(sc=="SC4")"list ID_t wave child ts33103 childcare_at_home_child_level childcare_at_home if ID_t ==  4011830  ",
    if(sc=="SC3")"list ID_t wave child ts33103 childcare_at_home_child_level childcare_at_home if ID_t ==  4007552  ",
    "",
    "* reduce to distinct observations per ID_t and wave in order to be able to merge the dummy indicator to the person year dataset,
    bysort ID_t wave: keep if _n == 1",
    "",
    "* remove obs with missings in childcare_at_home - only a few cases that are most likely data errors.",
    "list ID_t wave child childcare_at_home disagint disagwave ts33202 ts33103 if childcare_at_home == .",
    "",
    "keep if childcare_at_home != .",
    "",
    "* reduce vars",
    "keep ID_t wave childcare_at_home",
    "",
    "",
    "* Label the variable,",
    if(english)"label variable childcare_at_home \"Childcare at home Dummy\"",
    if(!english)"label variable childcare_at_home \"Kinderbetreuung zuhause Dummy\"",
    "* Save this prepared child dataset temporarily",
    "tempfile child_prep",
    "save `child_prep', replace",
    "",
    "* restore person-year-dataset",
    "restore",
    "",
    "* now merge child prep to person-year-dataset",
    "merge 1:1 ID_t wave using `child_prep', nogen keep(master match)",
    "",
  )
}

#' function to generate R strings that read and merge neps datafiles to the person year dataset based on a list that is generated by the app user selecting variables in different datasets in the data transformation tab
#' @keywords internal
#' @noRd
 generate_strings <- function(data_list, suf_version, english) {
  # Initialize an empty character vector to store the results
  result_vector <- character()

  # Initialize string that holds regexp which is used to identify datasets that contain the variable spstat because that variable is used to get rid of harmonized episodes
  spstat_vars_regex <- "spEmp|spGap|spMilitary|spParLeave|spSchool_|spUnemp|spVocPrep|spVocTrain|spParentSchool_|spInternship_"

  # Loop over each dataframe in the provided list
  for (i in seq_along(data_list)) {
    df <- data_list[[i]]

    # check if there are any non-na values in the variable column
    if(sum(!is.na(df$variable))>0){

      # Prepare the strings for the current dataframe
      dataset_name <- stringr::str_match(df[1, "Dataset"], "SC\\d+_(.*?)_S")[, 2] # extract dataset basename
      selected_variables <- paste0("\"", dplyr::pull(df, variable), "\"", collapse = ", ")  # extract variables and put it into a string vector

      merge_vector <- unlist(gsub(" ", ", ",df[3][1,])) # take linkage keys from the list and put commas between them

      # Create the strings - in the read dta line in string3 we check if its one of the listed spell datasets, if yes, we add also the variable "spstat" because we need it to get rid of harmonized episodes in order to being able to merge it to the person year base file
      string1 <- paste0("# ",6+i,".", " Add selected variables from ", dataset_name," -------------")
      string2 <- ""
      # here we replace also _S_ in the dataset name with _D_ because this is pasted in the script but comes from the semantic files
      string3 <- paste0(dataset_name, " <- read_neps(paste0(datapath,\"", stringr::str_replace_all(stringr::str_extract(df[1, "Dataset"], pattern= "SC\\d_.*_S_"), "_S_", "_D_"), suf_version, ".dta\"), ", "col_select = c(", merge_vector, ", ", selected_variables, ifelse(stringr::str_detect(df[1, "Dataset"],  spstat_vars_regex),", \"spstat\"",""), "), english = ", english, ")")
      string4 <- ""
      # now we filter for spstat < 30 and get rid of those episodes but only when spstat exists and if we have 3 linkage keys
      string5 <- if(stringr::str_detect(df[1, "Dataset"],  spstat_vars_regex)) { "# filter for non-harmonized episodes in order to only merge the wave specific subspell information" }
      string6 <- if(stringr::str_detect(df[1, "Dataset"],  spstat_vars_regex)) { paste0(dataset_name, " <- ",dataset_name," |>
         filter(spstat < 30)")
      }
      string7 <- if(length(base::strsplit(merge_vector, " ")[[1]])==3) {""}
      string8 <- paste0("bio <- left_join(bio, ", dataset_name, ", by = c(", merge_vector,"))")
      string9 <- ""
      string10 <- paste0("rm(", dataset_name, ")")
      string11 <- ""
      string12 <- "stopifnot(1==2) # We stop script execution here, in order to draw your attention to the following Notes. You may delete this line now."
      string13 <- if(stringr::str_detect(df[1, "Dataset"],  spstat_vars_regex)) {"# Note: Researchers often have to deal with missings values, when preparing panel datasets. For variables in NEPS spell datasets however, it is even more important."}
      string14 <- if(stringr::str_detect(df[1, "Dataset"],  spstat_vars_regex)) {"# Sometimes it might be advisable to use a 'carry-forward' approach, replacing missing values with the most recent non-missing value from previous waves, to address missing data caused by filtering (e.g., when spell information is collected only during the initial interview because it is assumed to be time-invariant), new items or data issues."}
      string15 <- if(stringr::str_detect(df[1, "Dataset"],  spstat_vars_regex)) {"# However, it is important to carefully evaluate each spell-related variable to determine, whether carrying information forward (or backward) is appropriate. Additionally, you may also want to examine variables in non-spell datasets for missing values and consider methods such as carry-forward imputation or multiple imputation."}
      string16 <- if(stringr::str_detect(df[1, "Dataset"],  spstat_vars_regex)) {"# This process cannot be fully automated and must be performed thoughtfully by the researcher. Below, we provide routines for replacing missing values with valid values from preceding or subsequent rows, grouped by ID_t and splink (Episodes). Uncomment if you want to use them."}
      string17 <- if(stringr::str_detect(df[1, "Dataset"],  spstat_vars_regex)) {""}
      string18 <- if(stringr::str_detect(df[1, "Dataset"],  spstat_vars_regex)) {"# First, print the joined spell-related variables (if its alot of variables, you should list only a few of them stepwise and decide for each variable, if you want to carry forward non-missing information or not)"}
      string19 <- if(stringr::str_detect(df[1, "Dataset"],  spstat_vars_regex)) {paste0("# print(bio |> select(\"ID_t\", \"wave\", \"splink\", ", selected_variables, "), n = 40)")}
      string20 <- if(stringr::str_detect(df[1, "Dataset"],  spstat_vars_regex)) {""}
      string21 <- if(stringr::str_detect(df[1, "Dataset"],  spstat_vars_regex)) {"# Set Missings on selected variables."}
      string22 <- if(stringr::str_detect(df[1, "Dataset"],  spstat_vars_regex)) {"# bio <- replace_values_with_na(bio)"}
      string23 <- if(stringr::str_detect(df[1, "Dataset"],  spstat_vars_regex)) {""}
      string24 <- if(stringr::str_detect(df[1, "Dataset"],  spstat_vars_regex)) {"# Now use tidyr::fill to carry non-missing information forward and backward !!where approriate!!."}
      string25 <- if(stringr::str_detect(df[1, "Dataset"],  spstat_vars_regex)) {paste0("# vars_to_fill <- c(",selected_variables,")")}
      string26 <- if(stringr::str_detect(df[1, "Dataset"],  spstat_vars_regex)) {"# bio <- bio |>
        # arrange(ID_t, splink, wave) |> # sort ascending
        # group_by(ID_t, splink) |>
        # fill(all_of(vars_to_fill), .direction = 'downup') |> # forward then backward
        # ungroup()"}
      string27 <- if(stringr::str_detect(df[1, "Dataset"],  spstat_vars_regex)) {""}

      # Concatenate the current strings into the result vector
      result_vector <- c(result_vector, string1, string2, string3, string4, string5, string6, string7, string8, string9,string10,string11,string12,string13,string14,string15,string16,string17,string18,string19,string20,string21,string22,string23,string24,string25,string26,string27)
    }

    else {
      # Do nothing and proceed to the next iteration
    }

  }
  return(result_vector)
 }

#' function to generate Stata strings that read and merge neps datafiles to the person year dataset based on a list that is generated by the app user selecting variables in different datasets in the data transformation tab
#' @keywords internal
#' @noRd
generate_strings_stata <- function(data_list, suf_version) {
  # Initialize an empty character vector to store the results
  result_vector <- character()

  # Initialize string that holds regexp which is used to identify datasets that contain the variable spstat because that variable is used to get rid of harmonized episodes
  spstat_vars_regex <- "spEmp|spGap|spMilitary|spParLeave|spSchool_|spUnemp|spVocPrep|spVocTrain|spParentSchool_|spInternship_"

  # Loop over each dataframe in the provided list
  for (i in seq_along(data_list)) {
    df <- data_list[[i]]

    # check if there are any non-na values in the variable column
    if(sum(!is.na(df$variable))>0){

      # Prepare the strings for the dataframe by extracting dataset basename
      dataset_name <- stringr::str_match(df[1, "Dataset"], "SC\\d+_(.*?)_S")[, 2]
      # Extract dataset full name
      full_dataset_name <- stringr::str_extract(df[1, "Dataset"], pattern= "SC\\d_.*_S_")
      # Extract variables and put it into a string vector
      selected_variables <- paste(stringr::str_replace_all(dplyr::pull(df, variable),"\"",""), collapse = " ")
      # Generate a string that holds the variables as 1 string for later listing in stata code
      selected_variables_collapsed <- paste(selected_variables, collapse = " ")
      # Extract sptype number for listing
      sptype_number <- df |>
        dplyr::filter(stringr::str_detect(Dataset, dataset_name)) |>
        dplyr::pull(Sptype)

      # Take linkage keys from the list and put commas between them
      merge_vector <- unlist(stringr::str_replace_all(df[3][1,],"\"",""))

      # Create the strings - in the read dta line in string3 we check if its one of the listed spell datasets, if yes, we add also the variable "spstat" because we need it to get rid of harmonized episodes in order to being able to merge it to the person year base file
      string0 <- "*******************************************************************************"
      string1 <- paste0("* ",6+i,".", " Add selected variables from ", dataset_name)
      string2 <- "*******************************************************************************"
      string3 <- ""
      string4 <- "preserve"
      string5 <- paste0("use ", merge_vector," ", selected_variables, ifelse(stringr::str_detect(df[1, "Dataset"],  spstat_vars_regex),"  spstat "," "), "using \"$DATA\\",stringr::str_replace_all(full_dataset_name, "_S_", "_D_"), suf_version, ".dta\", clear")
      string6 <- ""
      string7 <- if(stringr::str_detect(df[1, "Dataset"],  spstat_vars_regex)) { "* filter for non-harmonized episodes in order to only merge the wave specific subspell information" }
      # now we filter for spstat < 30 and get rid of those episodes but only when spstat exists and if we have 3 linkage keys
      string8 <- if(stringr::str_detect(df[1, "Dataset"],  spstat_vars_regex)) {
        "keep if spstat < 30"
      }
      string9 <- if(stringr::str_detect(df[1, "Dataset"],  spstat_vars_regex)) {""}
      string10 <- "tempfile data"
      string11 <- "save `data'"
      string12 <- ""
      string13 <- "restore"
      string14 <- ""
      # if we have unique identifier in both datasets we can merge 1:1 - this is the case when we have at least 2 merging variables, however if we only merge by ID_t (eg with Basics), we need to merge m:1
      string15 <- if(length(merge_vector) > 1) paste0("merge 1:1 ", merge_vector, " using `data', keep(1 3)  nogen") else if (length(merge_vector)==1) paste0("merge m:1 ", merge_vector, " using `data', keep(1 3) nogen")
      string16 <- ""
      string16 <- "assert 1 == 2 // We stop script execution here, in order to draw your attention to the following notes. You may delete this line now."
      string17 <- if(stringr::str_detect(df[1, "Dataset"],  spstat_vars_regex)) { "* NOTE: Researchers often have to deal with missings values when preparing panel datasets. For variables in NEPS spell datasets however, it is even more important. Sometimes it might be advisable to use a 'carry-forward' approach, replacing missing values with the most recent non-missing value from previous waves, to address missing data caused by filtering (e.g., when spell information is collected only during the initial interview because it is assumed to be time-invariant), new items or data issues. However, it is important to carefully evaluate each spell-related variable to determine whether carrying information forward (or backward) is appropriate. Below are routines for filling missing values using valid values from preceding or following rows within each ID_t and splink (Episodes). Uncomment these lines if you want to use them."}
      string18 <- if(stringr::str_detect(df[1, "Dataset"],  spstat_vars_regex)) {"* First, list the merged spell-related variables (If its alot of variables, you should list only a few of them stepwise and decide for each variable if you want to carry forward non-missing information or not)"}
      string19 <- if(stringr::str_detect(df[1, "Dataset"],  spstat_vars_regex)) {paste0("list ID_t wave splink ", selected_variables_collapsed, " if sptype == ", unique(sptype_number), " in 1/40, sepby(ID_t splink) ") }
      string20 <- if(stringr::str_detect(df[1, "Dataset"],  spstat_vars_regex)) {""}
      string21 <- if(stringr::str_detect(df[1, "Dataset"],  spstat_vars_regex)) {"* Set Missings on selected variables. It is recommended to use the function nepsmiss from the nepstools ado to recode all missing values to specific missing codes. You may install it with: net install nepstools, from(http://nocrypt.neps-data.de/stata). If you want to exclude specific missings from being recoded you might use statas mvdecode function instead."}
      string22 <- if(stringr::str_detect(df[1, "Dataset"],  spstat_vars_regex)) {"* nepsmiss _all"}
      string23 <- if(stringr::str_detect(df[1, "Dataset"],  spstat_vars_regex)) {""}
      string24 <- if(stringr::str_detect(df[1, "Dataset"],  spstat_vars_regex)) {"* Now carry forward existing information to rows with missings data. Please edit the varlist in order to only carry forward information on variables you are sure about"}
      string25 <- if(stringr::str_detect(df[1, "Dataset"],  spstat_vars_regex)) {paste0("*foreach var of varlist ", selected_variables_collapsed, " {")}
      string26 <- if(stringr::str_detect(df[1, "Dataset"],  spstat_vars_regex)) {"*bys ID_t splink (wave): replace `var' = `var'[_n-1] if missing(`var') & !missing(`var'[_n-1]) //fill missings from preceding rows"}
      string27 <- if(stringr::str_detect(df[1, "Dataset"],  spstat_vars_regex)) {"*}"}
      string28 <- if(stringr::str_detect(df[1, "Dataset"],  spstat_vars_regex)) {""}
      string29 <- if(stringr::str_detect(df[1, "Dataset"],  spstat_vars_regex)) {"*Note: It might also make sence for some variables to carry information backward when there are missing values at the beginning an episode. If you want to do that, you can use the same procedure but flip the ordering of spells:"}
      string30 <- if(stringr::str_detect(df[1, "Dataset"],  spstat_vars_regex)) {"*gsort ID_t splink -wave // Order subspells in descending order to easily access the last value of group ID_t and splink for the carry backwards operation"}
      string31 <- if(stringr::str_detect(df[1, "Dataset"],  spstat_vars_regex)) {paste0("*foreach var of varlist ", selected_variables_collapsed, " {")}
      string32 <- if(stringr::str_detect(df[1, "Dataset"],  spstat_vars_regex)) {"*by ID_t splink: replace `var' = `var'[_n-1] if missing(`var') & !missing(`var'[_n-1]) // now carry forward information again"}
      string33 <- if(stringr::str_detect(df[1, "Dataset"],  spstat_vars_regex)) {"*}"}
      string34 <- if(stringr::str_detect(df[1, "Dataset"],  spstat_vars_regex)) {""}
      string35 <- if(stringr::str_detect(df[1, "Dataset"],  spstat_vars_regex)) {"*sort ID_t splink wave // sort in natural way again"}

      # Concatenate the current strings into the result vector
      result_vector <- c(result_vector, string0, string1, string2, string3, string4, string5, string6, string7, string8, string9, string10, string11, string12, string13, string14,string15,string16,string17,string18,string19,string20,string21,string22,string23, string24, string25, string26, string27, string28, string29, string30, string31, string32, string33, string34, string35)

    }

    else {
      # Do nothing and proceed to the next iteration
    }

  }

  return(result_vector)
}

