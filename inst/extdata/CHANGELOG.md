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

## Version 0.3.4 (2026-09-22)

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
