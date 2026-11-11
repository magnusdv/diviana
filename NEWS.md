# dev

## Imports

* Redesigned AM/PM import dialogs.
* Added confirmation before replacing complete DVI datasets.
* AM text import now includes an option to prepend family name to sample IDs.
* Fixed missing-person numbering and family-prefixed labels when adding AM data.
* Allow complete DVI datasets without PM samples.

## AM data and pedigrees

* A link icon (🔗) indicates that an AM sample is assigned to a pedigree.
* Improved reference selection in the pedigree editor, prioritising unassigned references from the current family.
* Extended relabelling options, including naming missing persons after families.
* Added a `PM -> AM` button for manual identifications.

## Aliases

* Added a "Do not use aliases" option, which is now the default.
* Improved alias generation, text extraction and preview ordering.
* Allow duplicate AM aliases, but not PM aliases.

## Other

* Added AM/PM export to text and Excel.
* Added sample and assignment counts to data card headers.
* Improved automatic accordion behaviour for large datasets.
* Use `startApp()` instead of `runApp()` in `launchApp()`.


# diviana 0.5.2

## Imports

* Added a separate "Load DVI dataset" dialog for `.fam`, `.rds` and `.RData` files, with dataset preview and inspection warnings.

* Simplified AM/PM imports, with automatic detection of text formats (genotype tables, relationship tables and GeneMapper exports).

* Improved handling of disconnected reference pedigrees in Familias `.fam` files, with more informative warnings for families requiring manual inspection.

* Added support for automatic pedigree construction from relationship information in AM text files.

## PM data

* Completed the "Merge PM" module, with support for dropout, drop-in and typing errors, an option to ignore sex, and Excel export.

## AM data

* Added a "Relabel" button for systematically renaming families and missing persons, with preview and confirmation.

## Pedigrees

* Improved plots of large pedigrees.
* Added pagination for the Overview plot.
* Improved the pedigree editor and navigation between families.



# diviana 0.5.0

## Imports

* Familias files with disconnected reference families are now supported. Such families are internally 'repaired' (artificially connected) and should be manually inspected after loading.

## AM data

* Clicking a family name opens the pedigree in the pedigree plot window.

## PM data

* A new module "Merge PM" is added, activated by a button in the PM data window.

## Other

* Added new dataset `heli`, exemplifying PM merging.


# diviana 0.4.5

## Imports

* Reprocess file when changing file type.
* Fix odd-looking error messages when wrong file type.

## Pedigrees

* Fix broken plots of pedigrees with more than 4 generations. 

## Analysis

* New option "Compute reference-free LRs (LR0)" in Settings dialog window. This is a useful diagnostic in cases with closely related (e.g. parent-child) victims.

* Add option `Compact` to result tables, making them more focused on positive findings. The current behaviour is to hide samples with conclusion "No match" or "Excluded".


# diviana 0.4.4

## Imports

* Avoid crash when clicking "Save" too early
* Fix bug failing to recognise "-/-" as missing data
* Recognise "ID" as sample name column
* Accept various spellings of AMEL column
* Harmonise marker names when adding more samples 
* Various fixes of the "Sources" fields

## Analysis

* Expose `dviSolve()` argument `limit` as parameter labelled "Pairwise LR min".
* Expose `dviSolve()` argument `maxAssign` in Settings dialog window.
* Add titles to Joint tables.
* More robust layout of solution plots

## Other

* Move **dvir** from *Suggests* to *Imports*.
* Fix sheet names and colWidths in excel download
* Disable triangle calculations with more than 5000 pairs
* Move Reset button to top-right corner


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
