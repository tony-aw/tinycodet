
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tinycodet

<!-- badges: start -->

[![R build
status](https://github.com/tony-aw/tinycodet/workflows/R-CMD-check/badge.svg)](https://github.com/tony-aw/tinycodet/actions)
[![Project Status: Active - The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![](https://img.shields.io/badge/ORCID-0000--0001--9498--8379-green.svg)](https://orcid.org/0000-0001-9498-8379)
[![](https://img.shields.io/badge/github--pages-tony--aw.github.io/tinycodet-purple.svg)](https://tony-aw.github.io/tinycodet)
<!-- badges: end -->

[![](man/figures/tinycodet.svg)](https://github.com/tony-aw/tinycodet)

 

## Description & Overview

The `tinycodet` R-package is a tiny little R package that adds a few
functions to help in your coding etiquette. It primarily focuses on 4
things:

1)  Safer decimal (in)equality testing, safer atomic conversions, and
    other functions for safer coding.
2)  A new package import system, that attempts to combine the benefits
    of using a package without attaching, with the benefits of attaching
    a package.
3)  Extending the string manipulation capabilities of the `stringi` R
    package.
4)  Reducing repetitive code.

The `tinycodet` R-package has only one dependency, namely `stringi`.
Most functions in this R-package are fully vectorized and optimized, and
have been well documented.

 

Although this is a relatively small R package, I do understand you may
not want to go through all the articles and help files of `tinycodet`
without knowing if the R package is even worthy of your time. Therefore,
to get a quick glimpse of what is possible in this R package, I invite
you to take a look at the [Get
Started](https://tony-aw.github.io/tinycodet/articles/tinycodet.html)
page on the website
(<https://tony-aw.github.io/tinycodet/articles/tinycodet.html>).

 

## Installation

One can install `tinycodet` from github like so:

``` r
remotes::install_github("https://github.com/tony-aw/tinycodet")
```

<!-- Or from CRAN (once released) like so: -->
<!-- ```{r eval=FALSE} -->
<!-- install.packages("tinycodet") -->
<!-- ``` -->

And attach the package - thus exposing its functions to the namespace -
using:

``` r
library(tinycodet)
```

And one can open the introduction help page of the `tinycodet` package
using:

``` r
tinycodet::tinycodet_help()
```

 

## Reporting issues and giving suggestions

When you coming across an issue with the `tinycodet` R package, you may
want to report it in the “Issues” tab on the GitHub page
(<https://github.com/tony-aw/tinycodet/issues>). If relevant, please
provide reproducible R code, as that will make it easier to diagnose the
issue. Please keep issue reports polite, professional, and to the point.

 

If you have questions or suggestions, please submit them in the
“Discussion” tab on the GitHub page
(<https://github.com/tony-aw/tinycodet/discussions>).

 
