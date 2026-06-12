# dev version

## Imports
* Accept various spellings of AMEL column
* Recognise "ID" as sample name column

## Analysis

* Expose `dviSolve()` argument `limit` as parameter labelled "Pairwise LR min".
* Expose `dviSolve()` argument `maxAssign` in Settings dialog window.
* Add titles to Joint tables.
* More robust layout of solution plots

## Other

* Move **dvir** from *Suggests* to *Imports*.
* Fix sheet names and colWidths in excel download


# diviana 0.4.2

This is the first version with a NEWS file.

## Imports

* Add support for plain text files
* Handle columns "Relationship" and "Kinship" in plain text AM files
* Allow `.fam` files for custom frequency databases
* Show error when frequency database does not contain all markers

## Aliases

* Alias dialog: Add button for replacing original names with aliases
* Alias dialog: Add "Keep until first" option in alias dialog

## Triangles

* Add informative message when no data is displayed
* Add "Across families" checkbox

## Analysis

* Add joint table to output
* Shorten result tab headings and add informative titles
* Fix bug giving wrong pairwise LRs in joint analysis
* Fix bug giving wrong aliases in symmetric identifications
* Re-implement Excel download
* Change download button label to "Results"

## Documentation

* Update "Pedigree" help page
* Update README with screenshot

## Other

* Make busy indicator pulse thicker
* Clean up interface styling and appearance
* Improve error handling in overview plot
