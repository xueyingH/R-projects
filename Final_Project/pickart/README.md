
<!-- README.md is generated from README.Rmd. Please edit that file -->

# pickart

<!-- badges: start -->

<!-- badges: end -->

## Purpose

The goal of pickart is to help users get detailed information of
artworks in the Metropolitan Museum.

## Installation

You can install the released version of pickart from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("pickart")
```

## Example

This several examples which show you how to use the package:

  - Example 1: how many and what kinds of departments the Metropolitan
    Museum has?

<!-- end list -->

``` r
library(pickart)
## basic example code
get_departments()
#>    id                                department
#> 1   1                  American Decorative Arts
#> 2   3                  Ancient Near Eastern Art
#> 3   4                            Arms and Armor
#> 4   5 Arts of Africa, Oceania, and the Americas
#> 5   6                                 Asian Art
#> 6   7                             The Cloisters
#> 7   8                     The Costume Institute
#> 8   9                       Drawings and Prints
#> 9  10                              Egyptian Art
#> 10 11                        European Paintings
#> 11 12    European Sculpture and Decorative Arts
#> 12 13                       Greek and Roman Art
#> 13 14                               Islamic Art
#> 14 15              The Robert Lehman Collection
#> 15 16                             The Libraries
#> 16 17                              Medieval Art
#> 17 18                       Musical Instruments
#> 18 19                               Photographs
#> 19 21                                Modern Art
```

## Functionality

There are multiple functions existed in pickart package, these are some
examples:

  - **get\_search\_results()** allows users give a certain theme or
    topic for searching, and will return the details of all artworks
    match the query.

  - **find\_represent\_dep()**, **find\_represent\_artists()**,
    **get\_map()**, **get\_related\_artworks()** and
    **download\_details()** can be used for data analysis and crawl for
    details of artworks after using the get\_search\_results() function.

  - **get\_artworks\_in\_dep()** allows users to get details of artworks
    in certain departments they are interested in.

  - **get\_popular\_perc()** and **get\_popular\_items()** should be
    used after calling **get\_artworks\_in\_dep()**, they can calculate
    the percentaeg of popular artworks in each department users choose
    and tell you what are those popular items.

  - **get\_art\_age()** allows users to search a type of artworks during
    a period. Users can specify the type of artworks, the start and the
    end of the period.

  - **sort\_time\_nation()** and **get\_distribution\_country()** should
    be used after **get\_art\_age()**, these functions can achieve
    sorting according to nationality and make summary statistics about
    the distribution in different countries.
