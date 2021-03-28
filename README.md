
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

  - Removal of ‘change date’ structures
  - Consolidating duplicate notes
  - Finding unreferenced records
  - Arranging records by type
  - Inserting explicit death subrecords
  - Arranging children by date of birth
  - Splitting files
  - Merging files

## Change dates

All top level records in a GEDCOM file can record the date and time they
were last modified. The `tidyged` package (the main package for creating
and summarising GEDCOM files) includes change dates (today’s date) by
default every time a record is created or modified. Since the time is
very unlikely to be useful in such a context, the package ignores this
by default. We illustrate by loading the `tidyged` and `tidyged.utils`
packages, and creating an example object.

``` r
library(tidyged)
library(tidyged.utils)

gedcom(subm("Me")) %>% 
  knitr::kable()
```

| level | record | tag   | value                                    |
| ----: | :----- | :---- | :--------------------------------------- |
|     0 | HD     | HEAD  |                                          |
|     1 | HD     | GEDC  |                                          |
|     2 | HD     | VERS  | 5.5.5                                    |
|     2 | HD     | FORM  | LINEAGE-LINKED                           |
|     3 | HD     | VERS  | 5.5.5                                    |
|     1 | HD     | CHAR  | UTF-8                                    |
|     1 | HD     | DEST  | tidyged                                  |
|     1 | HD     | SOUR  | tidyged                                  |
|     2 | HD     | VERS  | 0.0.0                                    |
|     2 | HD     | NAME  | The ‘tidyged’ package for the R language |
|     2 | HD     | CORP  | Jamie Lendrum                            |
|     3 | HD     | ADDR  |                                          |
|     3 | HD     | EMAIL | <jalendrum@gmail.com>                    |
|     3 | HD     | WWW   | <https://jl5000.github.io/tidyged/>      |
|     1 | HD     | DATE  | 28 MAR 2021                              |
|     1 | HD     | LANG  | English                                  |
|     1 | HD     | SUBM  | @U1@                                     |
|     0 | @U1@   | SUBM  |                                          |
|     1 | @U1@   | NAME  | Me                                       |
|     1 | @U1@   | CHAN  |                                          |
|     2 | @U1@   | DATE  | 28 MAR 2021                              |
|     0 | TR     | TRLR  |                                          |

See row 20 and the row after for the change date for the submitter
record.

For GEDCOM files with thousands of records, including change dates can
add considerable bloat. For this reason it is possible to remove all
change date structures with the `remove_change_dates()` function:

``` r
gedcom(subm("Me")) %>% 
  remove_change_dates() %>% 
  knitr::kable()
```

| level | record | tag   | value                                    |
| ----: | :----- | :---- | :--------------------------------------- |
|     0 | HD     | HEAD  |                                          |
|     1 | HD     | GEDC  |                                          |
|     2 | HD     | VERS  | 5.5.5                                    |
|     2 | HD     | FORM  | LINEAGE-LINKED                           |
|     3 | HD     | VERS  | 5.5.5                                    |
|     1 | HD     | CHAR  | UTF-8                                    |
|     1 | HD     | DEST  | tidyged                                  |
|     1 | HD     | SOUR  | tidyged                                  |
|     2 | HD     | VERS  | 0.0.0                                    |
|     2 | HD     | NAME  | The ‘tidyged’ package for the R language |
|     2 | HD     | CORP  | Jamie Lendrum                            |
|     3 | HD     | ADDR  |                                          |
|     3 | HD     | EMAIL | <jalendrum@gmail.com>                    |
|     3 | HD     | WWW   | <https://jl5000.github.io/tidyged/>      |
|     1 | HD     | DATE  | 28 MAR 2021                              |
|     1 | HD     | LANG  | English                                  |
|     1 | HD     | SUBM  | @U1@                                     |
|     0 | @U1@   | SUBM  |                                          |
|     1 | @U1@   | NAME  | Me                                       |
|     0 | TR     | TRLR  |                                          |

## Duplicate notes

With all records and many subrecords, it’s possible to include custom
notes to augment the information provided. These can either be the notes
themselves, or a pointer to a top level Note record:

``` r
notes <- gedcom(subm("Me")) %>%
  add_note("This is a generic note.") %>% 
  add_indi(indi_notes = c("This is a bespoke note.", "This is a generic note.")) %>% 
  add_repo("My repository", repo_notes = c("This is a bespoke note.", "This is a generic note."))

knitr::kable(notes)
```

| level | record | tag   | value                                    |
| ----: | :----- | :---- | :--------------------------------------- |
|     0 | HD     | HEAD  |                                          |
|     1 | HD     | GEDC  |                                          |
|     2 | HD     | VERS  | 5.5.5                                    |
|     2 | HD     | FORM  | LINEAGE-LINKED                           |
|     3 | HD     | VERS  | 5.5.5                                    |
|     1 | HD     | CHAR  | UTF-8                                    |
|     1 | HD     | DEST  | tidyged                                  |
|     1 | HD     | SOUR  | tidyged                                  |
|     2 | HD     | VERS  | 0.0.0                                    |
|     2 | HD     | NAME  | The ‘tidyged’ package for the R language |
|     2 | HD     | CORP  | Jamie Lendrum                            |
|     3 | HD     | ADDR  |                                          |
|     3 | HD     | EMAIL | <jalendrum@gmail.com>                    |
|     3 | HD     | WWW   | <https://jl5000.github.io/tidyged/>      |
|     1 | HD     | DATE  | 28 MAR 2021                              |
|     1 | HD     | LANG  | English                                  |
|     1 | HD     | SUBM  | @U1@                                     |
|     0 | @U1@   | SUBM  |                                          |
|     1 | @U1@   | NAME  | Me                                       |
|     1 | @U1@   | CHAN  |                                          |
|     2 | @U1@   | DATE  | 28 MAR 2021                              |
|     0 | @N1@   | NOTE  | This is a generic note.                  |
|     1 | @N1@   | CHAN  |                                          |
|     2 | @N1@   | DATE  | 28 MAR 2021                              |
|     0 | @I1@   | INDI  |                                          |
|     1 | @I1@   | SEX   | U                                        |
|     1 | @I1@   | CHAN  |                                          |
|     2 | @I1@   | DATE  | 28 MAR 2021                              |
|     1 | @I1@   | NOTE  | This is a bespoke note.                  |
|     1 | @I1@   | NOTE  | This is a generic note.                  |
|     0 | @R1@   | REPO  |                                          |
|     1 | @R1@   | NAME  | My repository                            |
|     1 | @R1@   | NOTE  | This is a bespoke note.                  |
|     1 | @R1@   | NOTE  | This is a generic note.                  |
|     1 | @R1@   | CHAN  |                                          |
|     2 | @R1@   | DATE  | 28 MAR 2021                              |
|     0 | TR     | TRLR  |                                          |

In the above example, there is a generic note recorded in a top level
Note record. This same note message has been used for the individual and
repository defined, but they have been repeated rather than pointing to
the Note record. There is also a repeated bespoke note given in the
Individual and Repository records.

The `consolidate_notes()` function will simplify the file, replacing
note values with pointers to top level Note records (creating them if
necessary) if they are repeated:

``` r
consolidate_notes(notes) %>% 
  knitr::kable()
```

| level | record | tag   | value                                    |
| ----: | :----- | :---- | :--------------------------------------- |
|     0 | HD     | HEAD  |                                          |
|     1 | HD     | GEDC  |                                          |
|     2 | HD     | VERS  | 5.5.5                                    |
|     2 | HD     | FORM  | LINEAGE-LINKED                           |
|     3 | HD     | VERS  | 5.5.5                                    |
|     1 | HD     | CHAR  | UTF-8                                    |
|     1 | HD     | DEST  | tidyged                                  |
|     1 | HD     | SOUR  | tidyged                                  |
|     2 | HD     | VERS  | 0.0.0                                    |
|     2 | HD     | NAME  | The ‘tidyged’ package for the R language |
|     2 | HD     | CORP  | Jamie Lendrum                            |
|     3 | HD     | ADDR  |                                          |
|     3 | HD     | EMAIL | <jalendrum@gmail.com>                    |
|     3 | HD     | WWW   | <https://jl5000.github.io/tidyged/>      |
|     1 | HD     | DATE  | 28 MAR 2021                              |
|     1 | HD     | LANG  | English                                  |
|     1 | HD     | SUBM  | @U1@                                     |
|     0 | @U1@   | SUBM  |                                          |
|     1 | @U1@   | NAME  | Me                                       |
|     1 | @U1@   | CHAN  |                                          |
|     2 | @U1@   | DATE  | 28 MAR 2021                              |
|     0 | @N1@   | NOTE  | This is a generic note.                  |
|     1 | @N1@   | CHAN  |                                          |
|     2 | @N1@   | DATE  | 28 MAR 2021                              |
|     0 | @I1@   | INDI  |                                          |
|     1 | @I1@   | SEX   | U                                        |
|     1 | @I1@   | CHAN  |                                          |
|     2 | @I1@   | DATE  | 28 MAR 2021                              |
|     1 | @I1@   | NOTE  | @N2@                                     |
|     1 | @I1@   | NOTE  | @N1@                                     |
|     0 | @R1@   | REPO  |                                          |
|     1 | @R1@   | NAME  | My repository                            |
|     1 | @R1@   | NOTE  | @N2@                                     |
|     1 | @R1@   | NOTE  | @N1@                                     |
|     1 | @R1@   | CHAN  |                                          |
|     2 | @R1@   | DATE  | 28 MAR 2021                              |
|     0 | @N2@   | NOTE  | This is a bespoke note.                  |
|     1 | @N2@   | CHAN  |                                          |
|     2 | @N2@   | DATE  | 28 MAR 2021                              |
|     0 | TR     | TRLR  |                                          |

## Unreferenced records

If there are any records that are not referenced anywhere else, they can
be found with the `identify_unused_records()` function. In the example
below we create 6 family group records, half with members, half without,
and also an unreferenced Repository record:

``` r
some_unref <- gedcom(subm("Me")) %>% 
  add_indi(qn = "Tom Smith") %>% 
  add_indi(qn = "Tammy Smith") %>% 
  add_indi(qn = "Alice White") %>% 
  add_indi(qn = "Phil Brown") %>% 
  add_famg(husband = "Tom", wife = "Tammy") %>% 
  add_famg() %>% 
  add_famg(husband = "Phil") %>% 
  add_famg() %>% 
  add_famg(children = "Alice") %>% 
  add_famg() %>% 
  add_repo("Test repo") 
  
identify_unused_records(some_unref)
#> [1] "@F2@" "@F4@" "@F6@" "@R1@"
```

We can find out more about these xrefs by using the `describe_records()`
function from the `tidyged` package:

``` r
identify_unused_records(some_unref) %>% 
  describe_records(gedcom = some_unref)
#> [1] "Family @F2@, headed by no individuals, and no children"
#> [2] "Family @F4@, headed by no individuals, and no children"
#> [3] "Family @F6@, headed by no individuals, and no children"
#> [4] "Repository @R1@, Test repo"
```

## Arranging records

The ability to order records by type in a GEDCOM file is a task purely
done for aesthetics - it has no functional value, other than perhaps
making records easier to find. We use the example above, where we can
initially see the records are given in the order they were defined:

``` r
unique(some_unref$record)
#>  [1] "HD"   "@U1@" "@I1@" "@I2@" "@I3@" "@I4@" "@F1@" "@F2@" "@F3@" "@F4@"
#> [11] "@F5@" "@F6@" "@R1@" "TR"
```

We now use the `arrange_recrds()` function to arrange them in a specific
order, given by a character string giving the first initial of each
record type (header, trailer, and submitter records do not move):

``` r
ordered <- arrange_records(some_unref, "RIFSNM")

unique(ordered$record)
#>  [1] "HD"   "@U1@" "@R1@" "@I1@" "@I2@" "@I3@" "@I4@" "@F1@" "@F2@" "@F3@"
#> [11] "@F4@" "@F5@" "@F6@" "TR"
```

## Inserting explicit death subrecords

Sometimes individuals are defined with a date of birth but no death
subrecord because there is no information about the death to warrant
creating one. However, including an empty death subrecord is valuable in
its own right to indicate the individual is dead. The
`insert_explicit_death_subrecords()` function allows you to
automatically insert these subrecords for individuals who would be too
old to still be alive.

We illustrate with the following example of five individuals with
various dates of birth (including a missing birth subrecord for person
5):

``` r
people <- gedcom(subm("Me")) %>% 
  add_indi(qn = "Person 1") %>% 
  add_indi_fact("birth", date = date_calendar(1900, 4, 4)) %>% 
  add_indi(qn = "Person 2") %>% 
  add_indi_fact("birth", date = date_calendar(1888)) %>%
  add_indi(qn = "Person 3") %>% 
  add_indi_fact("birth", date = date_calendar(1885, 6)) %>%
  add_indi(qn = "Person 4") %>% 
  add_indi_fact("birth", date = date_calendar(1905)) %>%
  add_indi(qn = "Person 5")

dplyr::filter(people, tag == "DEAT")
#> # A tibble: 0 x 4
#> # … with 4 variables: level <dbl>, record <chr>, tag <chr>, value <chr>
```

If we were to assume a maximum age of 120 years (the default for the
function) we would want a death subrecord for the first three
individuals.

``` r
people_wds <- insert_explicit_death_subrecords(people, max_age = 120)

dplyr::filter(people_wds, tag == "DEAT")
#> # A tibble: 3 x 4
#>   level record tag   value
#>   <dbl> <chr>  <chr> <chr>
#> 1     1 @I1@   DEAT  Y    
#> 2     1 @I2@   DEAT  Y    
#> 3     1 @I3@   DEAT  Y
```

If an individual has no date of birth defined, then no judgement can be
made and the record is left as-is.

## Arranging children by date of birth

The `order_famg_children_all()` function ensures all children are
represented in Family Group records in order of date of birth.
