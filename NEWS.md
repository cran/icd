# Version 3.3
 * Fixes #160, #161
 * Includes replication code for benchmarking, showing enormous speed of 'icd' over alternatives.
 * Improved referencing and documentation
 * Functions to print vectors of ICD codes more cleanly when they have 'icd' classes or attributes.

# Version 3.2.1
 * Pediatric Complex Chronic Condition functions more consistent with the other comorbidity functions
 * Minor bug fixes
 * Fixes #157
 * Eigen library is multithreaded, so OpenMP dependencies now dropped from this package, simplifying and speeding up build, dropping configure script, etc..

# Version 3.2.0
 * added [Pediatric Complex Chronic Condition comorbidity classification](https://jackwasey.github.io/icd/articles/PCCC.html)
 * added ICD-10 procedure code data, and AHRQ procedure code classification -- see [ICD-10 vignette](https://jackwasey.github.io/icd/articles/ICD-10.html)
 * added ICD-10-CM data from US CMS for 2015 to 2018
 * fixed 588 instead of 588.0 in Quan Elixhauser ICD-9 comorbidity map
 * fix regression where codes appearing in two comorbidities would only count once
 * default best-effort ordering of identifiers (e.g. Visit ID, Encounter, Patient ID) is done. There are many cases where this is not possible (disordered input data), or obvious (the IDs are given as a factor, where the factor level order does not resemble the order of appearance in the vector). For performance, this can be turned off using `restore_id_order = FALSE` in comorbidity calculations.
 * added `icd9_comorbid_charlson` and `icd10_comorbid_charlson` as synonyms for the Quan/Deyo comorbidity calculations. `comorbid_charlson` will infer the ICD type.
 * depend on new package [icd.data](https://github.com/jackwasey/icd.data) for large, rarely modified data files
 
# Version 3.1.2
 * Documentation improved
 * https://jackwasey.github.io/icd/ now has `pkgdown` generated site in `gh-pages` branch.
 * Allow more functions to work without having called `library(icd)` first

# Version 3.1.1
 * Includes article submitted to the Journal of Statistical Software, brings in minimal new dependencies with `rticles` and `tinytex`
 * Drop RODBC dependency. Uranium sample data can still be reproduced offline.

# Version 3.0
 * major improvement in comorbidity calculation speed using matrix multiplication behind the scenes. ICD-10 codes were slower than ICD-9 code comorbidities before, and are now 1.5 - 2 orders of magnitude faster.
 * Added AHRQ clinical condition software (CCS) classification for ICD-9 and ICD-10, thanks to Vitaly Druker, fixes #127
 * added 'charlson' synonyms for 'quan_deyo', e.g. Charlson comorbidities can now be found using `comorbid_charlson(patient_data)`
 * Added AHRQ clinical condition software (CCS) classification for ICD-9 and ICD-10, thanks to Vitaly Druker, fixes #127
 * function names have lost the 'icd_' prefix, old names kept for compatiblity, but easier now to write `comorbid(patient_data)`, and `icd::comorbid` may also be used, and which many consider good practice. `explain_icd` synonym avoids name conflict with the popular `dplyr` package, but `icd::explain` also a nice option.
 * Assume C++11, which is now four years old, enabling code simplification
 * rename all functions to remove the icd_, people can use the concise icd:: if library(icd) was not callled
 * deprecated rarely or unused code, including %i9in%
 
# Version 2.4.1
 * updated HCC to include 2017 data, thanks to @anobel fixes #136
 * fixed github #133 - segfault with empty ICD-10 string in icd10_comorbid
 * add Suggested dependency on microbenchmark, now CRAN is sensibly not requiring Suggests to be installed even for testing
 * re-enabled testthat C++ Catch tests and make examples tests run correctly
 * drop Wecker continuous integration support
 * fixed #108 - when no valid codes are given to icd_explain_table

# Version 2.3
 * Fixed nasty bug which failed to sort the names of a vector of ICD codes correctly, resulting in some incorrect icd_explain. Fixes github #123, #124, #126.
 * update stale URLs for example data sources

# Version 2.2
 * Removed deprecated functions and data. `icd9` still available in CRAN repo, but not being updated. This greatly speeds up and simplifies the test suite.
 * Bug fixes #89 #99 #106 #109 #100 #111 #113 #115 #116 #117 with many thanks to bug reporters and users
 * stopped changing an attribute by reference, thanks to Tomas Kalibera for highlighting.
 
# Version 2.1
 * Many thanks to @anobel for providing parsing and lookup of Hierarchical Condition Codes.
 * Many thanks to @edlee123 for contributing `explain_table` which tabulates results of looking up various information about a list of ICD codes. This is a new feature which may be changed as it is used in the real world, and more tests are developed.
 * removed `stringr` family of dependencies: it was often slower on benchmarking than built-ins, and no clear benefits other than internally consistent syntax, and stringr updates caused CRAN warnings due to a documentation change.
 * Fixes #86, thanks to @anobel
 * Fixes #91
 * Fixes #93 thanks to @nancyorgan
 * Fixes #97, thanks to @michaelgao8
 * Speed improvements, mostly simply from using base R hashed Environments instead of lists
 
# Version 2.0
 * ICD-10 support, including ICD-10 comorbidity mappings, validation and explanations (code to description). ICD-10-CM 2016 is included.
 * Extended the already comprehensive test suite to cover ICD-10 and fixed various bugs. Thanks to @wmurphyrd for providing some Elixhauser test data.
 * Package `icd9` should now be uninstalled.
 * Simplify functions
  - Automatic detection of ICD version when not specified
  - Automatic detection of use of short or decimal format codes
  - e.g. `icd9ExplainShort` becomes `icd_explain`
 * Optional use of the S3 class system so data can be described as ICD-9 or ICD-10, so appropriate functions are used without guesswork. This means that you can optionally label your data with the appropriate class, e.g. `as.icd10("A01")` or `as.icd9cm("0101")`. This will help avoid mistakes when working with mixed data.
 * ICD-9 and ICD-10 sub-versions, particularly for distinguishing of ICD-9 (WHO) from ICD-9-CM, and same for ICD-10. This can also be extended to other countries, e.g. ICD-10-CA, or other code types altogether.
 * Soft deprecated all `icd9` prefix functions, now this package equally covers ICD-10. New naming scheme follows Hadley Wickham's preferred coding style, using underscores. Most public functions begin with `icd_`. Package data, and version specific functions, are named with `icd_`, `icd9_`, `icd10_`, prefixes. E.g. `icd10_chapters` and `icd9cm_hierarchy`. All deprecated functions will still work, but they give warnings (sometimes many). The warnings can be turned off with an option. The original test suite from `icd9` runs and passes on the `icd` package, with only minimal changes.
 * Completely deprecated some previously soft deprecated functions, e.g. `icd9ValidDecimal`
 * Works with very latest `testthat` which has backward-incompatible changes
 * Minimized dependencies, but `icd` does import `stringi` via `stringr` to give cleaner string processing. Base string processing is still used as it is often faster. `magrittr` is now too useful not to import, has no dependencies of its own, and is imported by `stringr` anyway. CRAN now also seems to need base packages to be listed as imports.
 * More automated testing with Travis and Wercker, Codecov and Coveralls.

# Version 1.3
 * With many thanks to @wmurphyrd, Quan's revised scoring system for Charlson comorbidities is now included.
 * Re-enabled OpenMP, and also use GNU C++ standard library parallel extensions (which also use OpenMP) when available. Thoroughly tested with various docker compiler configurations and no memory or undefined behavior problems appear.
 * Use `fastmatch` for fast factor generation, but with the tweak of not sorting the levels. This had been by far the slowest step in generating comorbidities.

# Version 1.2.2
 * Completed fix from version 1.2.1 by temporarily disabling OpenMP on all platforms. Still runs quickly due to other optimizations made in supporting the OpenMP. It'll be back, but only when I or someone else can create a docker or VM image which runs Clang 3.7 or greater, OpenMP, with/without LLVM C++ standard library. See #75

# Version 1.2.1
 * Partly fixed obscure memory access violation error seen only withwhen using OpenMP clang 3.7 on fedora, and maybe OS X.

# Version 1.2
 * Make annual revisions of ICD-9-CM available. The package includes data from each year which CMS has published (versions 23 to 32). The default is to use version 32. More work will be needed to make it straightforward to use an arbitrary version when running `icd9` commands. These are avaiable in the package data `icd9Billable`. See vignette for examples.
 * Inexplicably, the only canonical list which includes both ICD-9-CM codes and headings is an RTF file. `icd9` can now parse this eclectically formatted document to extract all the headings, so it is not possible to do `icd9Explain` on a non-billable four-digit code, e.g. 643.0 (Mild hyperemesis of pregnancy). Previously on three-digit and billable (i.e. lead node) codes were used. In principle, the RTF parsing code could be run on previous versions going back to about year 2000. It seems that most years are the same or expand previous years, although there are a few deletions. Ideally, we would know what year/version a given ICD-9 code was coded under, and then validate or interpret accordingly. This can indeed be done for billable codes, but until the RTF is parsed for previous years, not for headings.
 * Condense and range functions has been completely reworked now that all the intermediate heading codes are available. This means there will be slight differences in the results produced, and there are still very minor quirks, but the tests cover thoroughly all known ICD-9-CM codes in existence from any available version.
 * The package data was refreshed with comorbidities derived from source ranges specified by the original authors. These deliberately produce valid but non-existent codes, and these are now slightly different with the range work. This should not have any impact on comorbidity assignmentments from real ICD-9 codes, but keeps the package consistent with itself.
 * OpenMP and C++11 are now both enabled on platforms which allow this (i.e. everything except Solaris), which gives performance improvements.
 * Van Walraven comorbidity score (analagous to Charlson score, but based on Elixhauser comorbidities) added by @wmurhpyrd, with thanks.
 * Dropped most included data from the package, as most can be retrieved from reliable web sites. The data is still in the github repo, but is downloaded automatically when needed (which is only working in the package source tree.)
 * stopped exporting 'parts' functions, as these complicate the namespace and are unlikely to end-user, but still available with `icd:::icd9PartsToShort` etc.
 * code clean-up with excellent `lintr` package from @jimhester
 * bug fixes
 * more tests, with coverage at about 85% with the full test suite
 * included first thousand patients from a public domain dataset from Vermont, available as `vermont_dx`.

# Version 1.1

 * C++ OpenMP parallel processing of comorbidities for further many-fold speed improvement, scaling to cores available in a machine, allocating about a million rows of comorbidities in a second or two, on a moderate workstation. Some speed is lost by (optionally) allowing disordered input visit IDs.
 * C++ optimization using simpler, faster STL structures, working with R factors (converted automatically) so comorbidity assignment is done using integer logic only
 * drop unwanted old vignette from the build. Fixes #42
 * refinement of many functions to specifically accept factors or character vectors, but not integers in most cases. Fixes #38
 * more natural use of matrix for comorbidities, with option to return data frames. Allow matrix or data.frame in fuctions which accept comorbidities
 * bug fixes with thanks to @wmurhpyrd. Fixes #44, fixes #46
 * fixes #41
 * extensive use of the excellent and very efficient checkmate package for validating function arguments
 * improved test coverage, demonstrated by the excellent covr package and coveralls.io . Fixes #4
 
# Version 1.0

* Calculate Charlson scores
* Sum distinct comorbidities or diagnoses by patient
* Core rewrite in C++ for 50+ times speed improvement. 100,000 patients assigned comorbidities in ~2 seconds.
* Simplified handling validation of codes. No longer done in every function.
* Most functions now guess the ICD-9 code type automatically (e.g. 00321 vs 003.21)
* Reduced external dependencies down to Rcpp and checkmate (a very lightweight and fast function argument checker)
* Bug fixes (see [github](https://github.com/jackwasey/icd/issues?q=is%3Aissue+is%3Aclosed))
* API changes
    - no more validation except in the icd9IsValidXxx functions. Removed stopIfInvalidIcd9, icd9InvalidActions
    - internalized utility functions. They are also packaged and tested in [jwutil](https://github.com/jackwasey/jwutil)
    - deprecated icd9ValidXxx in favour of icd9IsValidXxx
    - deprecated icd9ComorbditiesXxx replacing with briefer icd9ComorbidXxx
    - stopped exporting benchmarking and SAS code processing.

# Version 0.5

* Filter icd9 data for validity or existence. Fixes issue #27
* Guess whether a code is short or decimal. Fixes #22
* Enable ranges for Exxx codes. Fixes #9
* Explain and condense long lists of ICD-9 codes using maximum number of higher-level descriptions without being overly broad. Fixes #3
* Include high level descriptions, chapters, majors, which do not have CMS descriptions because they are not themselves billable codes. Fixes #2
* E codes <800 can be valid in 2014. Fixes #1
* Bug fixes, vignette and other documentation updated.
* Test suite ever more comprehensive.
* Rename Elixhauser arguments and functions to "Elix" for clarity of code, and ease of typing.

# Version 0.4

* Present-on-arrival field handled gracefully
* Revised co-morbidity handling, with functions specific to each mapping
* Standardized naming of fields across Elixhauser and Charlson based mappings for clarity, consistency and comparaibility
* Handle mild and severe co-morbidities without double-counting. Fixes issue #23
* Enable magrittr-style piping as option to clarify chains of operations. Issue #5
* many more test cases, including specific tests for individual values in each computed mapping, and also for the new comorbidity functions
* Vignette updated to demonstrate clearer new functions

# Version 0.3

* Addition of several utility functions, mostly kept for internal use, but which clarify the code
* Consistent behavior with invalid codes, allowing ignore, silent conversion to NA, warn with conversion to NA, or stopping.
* Outline of code to read top level ICD-9 definitions from canonical CDC RTF file.
* Progress to
* Made functions magrittr friendly
* Bug fixes, refactoring, more tests
* Include parsing of hierarchy
* Internal function which reads a file from a zip at a URL
* The included pre-parsed data are slightly updated to include some missing top-level numbers when all the children were also present. ("043" %i9s% "04499" should equal "043" %i9s% "044")

# Version 0.2.1

* trivial changes to appease CRAN

# Version 0.2

* Added more co-morbidity mappings, Elixhauser original and Quan/Elixhauser.
* Exposed more functions to public API
* Better conversion of ICD-9 codes to textual representation
* Expanded vignette
* Bug fixes and test cases
* Improved documentation

# Version 0.1.1

* Include missing import for memoise
* optionally process AHRQ comorbidity subcategories into parent categories
* include the processed binary data. Unlikely to change often, and not too big.


# Version 0.1

* First release. Fully functional, no known bugs, but will benefit from addition of planned features described in README.
