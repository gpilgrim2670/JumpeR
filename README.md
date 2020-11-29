# JumpeR
JumpeR R package

### Latest Development Version from Github

`devtools::install_github("gpilgrim2670/JumpeR")`
Package is still under heavy development so development versions will be unstable.
Please wait for an upcoming CRAN release to acquire a stable version.

# Usage

Version 0.0.1 of `JumpeR` reads track and field results into `R`, similar to what the `SwimmeR` package does for swimming results.

## Importing Results

`JumpeR` reads track and field results into `R` and outputs tidy dataframes.  `JumpeR` uses `read_results` to read in either a PDF or HTML file (like a url) and the `tf_parse` (for track and field) function to convert the read file to a tidy dataframe.  

`read_results` has two arguments, `file`, which is the file path to read in, and `node`, required only for HTML files, this is a CSS selector node where the results reside.  `node` defaults to `"pre"`, which has been correct in every instance tested thus far.

`tf_parse` has three arguments as of version 0.0.1.

`file` is the output of `read_results` and is required.

`avoid` is a list of strings.  Rows in `file` containing any of those strings will not be included.  `avoid` is optional.  Incorrectly specifying it may lead to nonsense rows in the final dataframe, but will not cause an error.  Nonsense rows can be removed after import.  

`typo` and `replacement` work together to fix typos, by replacing them with replacements.  Strings in `typo` will be replaced by strings in `replacement` in element index order - that is the first element of `typo` will be replaced everywhere it appears by the first element of `replacement`.  Typos can cause lost data and nonsense rows.

See `?tf_parse` or the package vignette for more information.

## Example

```r
tf_parse(
    read_results(
      "http://results.yentiming.com/2020/Indoor/2-29-20-MOC.htm"
    )
  )
```

## Getting help

You're welcome to contact me with bug reports, feature requests, etc. for `JumpeR`.

If you find bug, please provide a minimal reproducible example at [github](https://github.com/gpilgrim2670/JumpeR).

`JumpeR` is conceptually very similar to the `SwimmeR` package, which I also developed and maintain.  I do a lot of demos on how to use `SwimmeR` at my blog [Swimming + Data Science](https://pilgrim.netlify.app/).  `SwimmeR` also has a vignette.  Call `vignette("SwimmeR")`.  If you download from github don't forget to set `build_vignettes = TRUE`.
