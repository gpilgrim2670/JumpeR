# JumpeR 0.3.0 November 16th, 2021

* CRAN release containing all dev versions back to v0.1.4

# JumpeR 0.2.14 November 12th, 2021

* Bug fix where horizontal jump hytek results would have split_attempts applied
erroneously

# JumpeR 0.2.13 October 8th, 2021

* Changes to `hytek_parse` and `flash_parse` such that they don't hang on heat
sheets without results

# JumpeR 0.2.12 October 6th, 2021

* Changes to `tf_parse` so that it plays nicely with `purrr::map` and
`purrr::safely` when applied to Hytek results

# JumpeR 0.2.11 October 4th, 2021

* Changes to `tf_parse` so that it plays nicely with `purrr::map` and
`purrr::safely` when applied to Flash Tables

# JumpeR 0.2.10 October 1st, 2021

* Bug fixing for Flash Table results

# JumpeR 0.2.9 September 30th, 2021

* `attempts_split_long` for Flash Table results

# JumpeR 0.2.8 September 30th, 2021

* Renamed "Flight" columns to "Round" for Hytek style results to better match
Flash style

# JumpeR 0.2.7 September 20th, 2021

* Pulls wind values by round out of Hytek results

# JumpeR 0.2.6 September 20th, 2021

* Pulls wind values by round out of Flash table results

# JumpeR 0.2.5 September 15th, 2021

* Adding splits capabilities to `tf_parse` and `hytek_parse`

# JumpeR 0.2.4 September 15th, 2021

* Integration of `hytek_parse` inside `tf_parse`

# JumpeR 0.2.3 September 15th, 2021

* Integration of `flash_parse_table` inside `tf_parse` and `read_results`

# JumpeR 0.2.2 September 13th, 2021

* Bug fixes etc. for Hytek results on Delta Timing

# JumpeR 0.2.1 June 23rd, 2021

* Further integration of tools for scraping flashresults.com
* Sprint splits for flash results
* Wide format split renaming for flash results
* Minor bug fixes

# JumpeR 0.2.0 March 30th, 2021

* Integration of tools for scraping flashresults.com
* Minor bug fixes

# JumpeR 0.1.4 March 5th, 2021

* Resubmission due to removal from CRAN
* Minor bug fixes

# JumpeR 0.1.3 February 22nd, 2021

* Improved compliance with CRAN policies re: external resource testing
* Minor bug fixes

# JumpeR 0.1.2 February 18th, 2021

* Improved documentation for CRAN submission

# JumpeR 0.1.1 February 8th, 2021

* First release
* Added a `NEWS.md` file to track changes to the package.
