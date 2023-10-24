# focalutils

<!-- badges: start -->
<!-- badges: end -->

Provides a function (`make_focal_matrix`) to create matrices of circular or
sector neighbourhoods for use with the `focal` function (package `terra`). Also
has some utility functions for some common tasks with compass bearings such as
converting bearings to and from Cartesian angles and determining the direction
of shortest rotation between two bearings.

## Installation

``` r
install.packages("remotes") # if you don't already have it
remotes::install_github("mbedward/focalutils")
```

## Examples

Create a circular focal matrix of radius 50 cells, where neighbourhood cells
have a value of 1 and outside cells have `NA`:

``` r
library(focalutils)
m <- make_focal_matrix(50)
plot_focal_matrix(m)
```

Create the same neighbourhood but exclude the centre cell:
```r
m <- make_focal_matrix(50, include_centre = FALSE)
plot_focal_matrix(m)
```

Create a sector neighbourhood spanning 90 degrees and facing west:
```r
m <- make_focal_matrix(50, start_bearing = 225, end_bearing = 315)
plot_focal_matrix(m)
```

Create the complement of the last neighbourhood: a circular neighbourhood
that excludes the 90 degree sector facing west (Pacman!)
```r
m <- make_focal_matrix(50, start_bearing = 315, end_bearing = 225)
plot_focal_matrix(m)
```
