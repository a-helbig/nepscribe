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

