
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tidyged.utils <img src='man/figures/logo.png' align="right" height="138" />

<!-- badges: start -->

[![R-CMD-check](https://github.com/jl5000/tidyged.utils/workflows/R-CMD-check/badge.svg)](https://github.com/jl5000/tidyged.utils/actions)
[![](https://codecov.io/gh/jl5000/tidyged.utils/branch/main/graph/badge.svg)](https://codecov.io/gh/jl5000/tidyged.utils)
[![CodeFactor](https://www.codefactor.io/repository/github/jl5000/tidyged.utils/badge)](https://www.codefactor.io/repository/github/jl5000/tidyged.utils)
[![](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

Various utilities to maintain and clean family tree GEDCOM files.

The package is part of the `gedcompendium` ecosystem of packages. This
ecosystem enables the handling of `tidyged` objects (tibble
representations of GEDCOM files), and the main package of this ecosystem
is [`tidyged`](https://jl5000.github.io/tidyged/).

<img src="man/figures/allhex.png" width="65%" style="display: block; margin: auto;" />
<br> For an introduction to GEDCOM files, head to the
<a href="https://jl5000.github.io/tidyged/articles/intro_to_gedcom.html">website</a>.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("jl5000/tidyged.utils")
```

## Features

GEDCOM files can get very large and unwieldy, resulting in
inefficiencies and file bloat. The `tidyged.utils` package offers
functions to automate the cleaning and handling of GEDCOM files. It
includes:

-   Splitting files
-   Merging files
-   Removal of ‘change date’ structures
-   Consolidating duplicate notes
-   Finding unreferenced records
-   Arranging records by type
-   Inserting explicit death subrecords
-   Arranging children by date of birth
-   Redaction of living individuals
-   Functionality to guess the current age of individuals without dates
    of birth
-   Automatically creating parent/ancestor placeholder records en masse

Examples are given at the
[website](https://jl5000.github.io/tidyged.utils/).
