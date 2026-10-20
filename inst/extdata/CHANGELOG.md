## Version 0.1.1 (2026-01-16)

* Launch of beta version with all basic features working

## Version 0.2.0 (2026-01-22)

* Implemented a changelog feature in the help tab and display app
version on starting page.
* Small Changes to UI in 'Explore Datasets' and 'Transform Datasets'.
* Data transformation: SC5 spell prioritisation scripts now recognize
spelltype 36 - internships.
* Solved an issue with NA values in spell prioritisation code, when
spell prioritisation tab wasn’t clicked before preview or
downloading the script.
* Added and improved comments and layout of generated scripts in
subspell format in R and Stata scripts.
* The 'Additional Variables' informational commentary in the scripts
is now produced a single time at the end of the code block, replacing
the previous behavior of generating it after each joined spell dataset.
* Further training exemplary code in scripts is now only available for SC6.
It may be released for other SCs in the future.
* Added hover texts for all inputs in the UI.

## Version 0.3.0 (2026-04-30)

* Implemented (retrospective) feature to add work and unemployment experience indicators.
* Improved commentary on all scripts.
* Resolved an issue in stata scripts, when prioritisation list was changed. This change was not really applied to the data because a new variable prio\_temp was generated in the recoding line, which wasnt used then in the sorting for the prioritisation process.
* Resolved an issue in educational qualification data preparation chunks.
* When loading large ptarget datasets in the 'add additional variables' feature, a popup appears to warn users that loading the variable names may take a few moments.
* Variable first\_wave removed from all scripts.
* Fixed merging issues when adding variables from spVocBreaks, spVocExtExam and spSchoolExtExam.
* Various small typo fixes.
* Updated nepstool dependency to 0.1.3

## Version 0.3.1 (2026-05-06)

* Few small changes in the scripts
* Improved readme file
* Updated nepstool dependency to 0.1.4

## Version 0.3.2 (2026-09-18)

* Updated SC6 semantic structured files to 17-0-0
* Updated SC8 semantic structured files to 2-0-0
* Added some additional commentary to carry forward approach in stata script

## Version 0.3.3 (2026-09-22)

* The sidebar can now be resized by clicking and dragging its edge, and it collapses by default on the Start page, where it isn't needed.
* Multiple smaller fixes and changes to the generated Stata and R scripts.
* Centered the content on the Start page.
* Resolved two warnings raised during package checks: internal app functions were being exported unintentionally, and non-ASCII characters (ä, ö, ü, ß, etc.) in the source code caused encoding warnings.
* Fixed a JavaScript error in the 'Explore Datasets' tab that broke the variable tooltips.

## Version 0.3.4 (2026-09-24)

* Replaced the "selected variables" dropdown in Additional Variables with an always-visible, collapsible summary of what's been added, grouped by dataset. Individual variables can be unchecked to exclude them from the script without fully removing them, and whole datasets can be removed with one click.
* Added hover tooltips showing each variable's survey question text directly in the Additional Variables picker, so you no longer need to switch to Explore Datasets to look it up.
* Every input in the Transform Data sidebar and Explore Datasets now has its own distinct, consistently-styled hover tooltip. Previously, radio button and checkbox groups shared a single combined tooltip for all options, and some tooltips used the browser's native black style instead of matching the rest of the app.
* Fixed labels and question-text tooltips not updating when the language toggle was switched after variables were already loaded or confirmed.
* Fixed tooltips occasionally getting stuck on screen after clicking a variable in the Additional Variables picker.
* Added a note explaining why not every NEPS dataset is available in Additional Variables.
* Variables can now be added to the script directly from Explore Datasets: variables that can be added are marked with a grey + in the table. Select their rows and click "Add Selected to Script" to send them to Additional Variables in Transform Data. Only variables from datasets that can be merged into a person-year dataset, and from the starting cohort selected in Transform Data, can be added; anything skipped is listed with the reason.
* In Additional Variables, picking a dataset now pre-selects the variables already added for it, so "Confirm Vars" adds to them instead of replacing them.
* Additional Variables: the "Reset Everything" button now sits next to "Confirm Vars", and the summary of selected variables is aligned with the variable picker.
* Explore Datasets: the table buttons and the search field now share one toolbar row, the search field is highlighted, and the buttons are visually separated.
* Start page: the title is now larger and centered.
* Closing the app in a browser tab no longer stops it on the server, which could disconnect other users at the same time.
* Fixed the exemplary highest-education preparation for SC4 in Stata scripts, which left most values missing. R and Stata scripts now classify education the same way.
* Fixed the SC3/SC4 exemplary children preparation in R and Stata scripts.
* R scripts in original subspell format now load the janitor package, which the further training example needs.
* R scripts: adding variables from more than one spell dataset no longer leaves duplicate spstat.x / spstat.y columns in the person-year data.

## Version 0.3.5 (2026-10-08)

* Spell prioritisation no longer uses the duration of an episode. Among otherwise equal parallel episodes (same spell type, main/side episode and working hours), splink now decides, i.e. the order in which the episodes were reported. Within a spell module, earlier reported episodes usually started earlier, so this favours the episode that has lasted longer at the interview rather than the one with the longer total duration. This applies to both formats, in Stata and R scripts.
* Original subspell format: spells with missing start or end dates are no longer dropped. The dates are not needed for selecting the spells ongoing at the interview or for the prioritisation, so these spells are now kept (their duration variable is missing).
* Legal notice and data protection: the Help menu and a footer on every page now link to the corresponding pages of the WZB. The previous Impressum window was removed.
* The syntax highlighting in the script preview (highlight.js) is now served by the app itself instead of an external CDN, so no visitor data is sent to third-party servers.
* Script preview in Transform Data: Stata scripts are now syntax-highlighted like R scripts (commands, comments, strings and macros).

## Version 0.3.6 (2026-10-09)

* Exemplary children preparation in R and Stata scripts: the example person shown for inspection is now selected automatically from the data.

## Version 0.4.0 (unreleased)

* Explore Datasets: a new chart icon next to each variable opens its frequencies from the public NEPS codebook, together with the variable's question text. It shows in which waves the variable has valid answers, the frequency table across waves and each value's share of the valid cases. Labels follow the German/English switch. A button in the top right corner enlarges the window to almost the whole screen, which helps with variables that have many waves or values.
* Explore Datasets: clicking the grey + of a variable now adds just that variable to the script. There is no need to select rows or scroll up to "Add Selected to Script" for a single variable.
* Explore Datasets: the item text of a variable is now shown by default in the table (column Itemtext, next to Questiontext) and in the codebook window. For items that share one question, such as a list of reasons or an agreement battery, it says what the variable refers to exactly, e.g. "It was too expensive for me." under a question on reasons for not taking courses.
* Explore Datasets: the metadata columns Varlabel and Variablequestion are now called Variablelabel and Itemtext.
* Faster response when adding variables from large datasets like pTarget: the metadata of a dataset is now read only once instead of on every action.
* Start page: the description of Explore Datasets now mentions the codebook frequencies and sending variables to the script, with a note on where the frequencies come from.
* Fixes from testing the scripts for Starting Cohorts 3, 4 and 5:
* SC3: when variables from pTarget are added, the variable tx20100 (row recommended for linkage) is now loaded as well, so the filter on it no longer stops the R and Stata scripts with an error. It is dropped again after the filter unless it was selected.
* Harmonized spell format: if a respondent has the same interview month in two waves (e.g. SC5), only the first of these waves is kept, in R and Stata and for all cohorts. Before, R assigned the same spell to both waves while Stata kept only one (SC5 only).
* Original subspell format, Stata: parallel episodes are now prioritised exactly as in R. Missing codes in the main/side episode marker (spms) are treated as side episodes, and missing working hours and training types are filled completely from later subspells of the same episode (before, only one row was filled).
* Original subspell format, R: all negative missing codes in the date variables are set to NA, not only -97 and -98. SC5: season codes in the internship dates are now recoded as well (R and Stata).
* Original subspell format: R and Stata scripts now drop the same helper variables at the end (working hours, training type, ongoing flags, spstat), so both formats and both languages end up with the same variables.
* Highest education example (R): the wave labels are now taken before the interview dates are reshaped, which removes the warning "Unknown or uninitialised column: wave" and keeps the wave labels.
* Stata scripts with English labels: every NEPS dataset is now switched to English directly after loading. Before, variables added by a merge lost their value labels, because only the German label sets were carried over. The highest education example now also uses the English label sets for CASMIN and ISCED.
* New variable spells_reported in both formats and languages: 1 if spell data were reported in that wave. In the harmonized spell format it marks person-years that only exist because the Biography covers them retrospectively (e.g. SC5 online waves, SC3/SC4 school waves), where variables added from spell datasets are missing. In the original subspell format it is always 1.
* Starting Cohorts 3, 4 and 5: the "not fully tested" warning is replaced by cohort-specific notes: a short recommendation, the survey context (tx80230 in SC3/SC4), which waves the two formats cover and the new variable spells_reported. The same notes are written at the top of the generated scripts.
* SC3/SC4: adding variables from spEmp (under Additional Variables or from Explore Datasets) shows a notice on the reduced employment module for persons under 21 without completed vocational training (tf23913). The generated script contains the same note.
* SC4: adding variables from pTarget (under Additional Variables or from Explore Datasets) shows a notice that it only covers students surveyed in school (no person-years in SC4) and points to pTargetCATI / CohortProfile. The generated script contains the same note.
* Spell prioritisation: a note on the licence course recode explains that licence courses are no longer recorded in spVocTrain from the field year 2018/19 on (SC4, SC6) or at all (SC3).
