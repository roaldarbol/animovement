
<!-- README.md is generated from README.Rmd. Please edit that file -->

# trackballr

<!-- badges: start -->

[![DOI](https://zenodo.org/badge/773406370.svg)](https://zenodo.org/doi/10.5281/zenodo.13235277)
[![R-CMD-check](https://github.com/roaldarbol/trackballr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/roaldarbol/trackballr/actions/workflows/R-CMD-check.yaml)
[![trackballr status
badge](https://roaldarbol.r-universe.dev/badges/trackballr)](https://roaldarbol.r-universe.dev)
[![Codecov test
coverage](https://codecov.io/gh/roaldarbol/trackballr/graph/badge.svg)](https://app.codecov.io/gh/roaldarbol/trackballr)
<!-- badges: end -->

*Run the trackball!*

Trackball experiments, in which animals are tethered/restrained atop a
(most commonly) styrofoam ball are common experiments within animal
behaviour and neuroscience.

The primary goal of the *trackballr* package is to enable a
*tidyverse*-friendly analysis of trackball data and use a standardised
tidy data format.

## Installation

You can install the development version of trackballr with:

| Type | Source | Command |
|----|----|----|
| Development | R-universe | `install.packages("trackballr", repos = "https://roaldarbol.r-universe.dev")` |

Once you have installed the package, you can load it with:

``` r
library("trackballr")
```

## Documentation

The trackball analysis workflow is simple, and generally follows the
same steps as other movement data sets (e.g. pose estimation, centroid
tracking). See our docs to go through the steps, one-by-one:

- [Read
  data](https://www.roald-arboel.com/trackballr/articles/Read-data.html)
- [Clean
  tracks](https://www.roald-arboel.com/trackballr/articles/Clean-tracks.html)
- [Compute
  kinematics](https://www.roald-arboel.com/trackballr/articles/Compute-kinematics.html)
- [Compute movement
  statistics](https://www.roald-arboel.com/trackballr/articles/Compute-movement-statistics.html)

## Status

> \[!Warning\] 🏗️ The package is currently in early development and the
> interface is subject to change. Feel free to play around and provide
> feedback.

## Roadmap

We envision this package to develop mostly into providing functions for
reading trackball data. We hope to support your experimental cases, such
as:

- ✅ Support for Optical mouse sensors, free configuration
- ⏳ Support for Optical mouse sensors, fixed configuration
- ⏳ Support FicTrac
- ⏳ More tests

If you are running experiments with trackballs, we would love to get a
sample of your data to support it!

For computing kinematics and summary statistics, we are currently
implementing those, but they might be transferred to a different package
that does so for a broad array of movement data, so **stay put to hear
more**!

## Citation

To cite *trackballr* in publications use:

``` r
citation("trackballr")
#> To cite package 'trackballr' in publications use:
#> 
#>   Roald-Arbøl M (2024). "trackballr: Read trackball files."
#>   doi:10.5281/zenodo.13235278
#>   <https://doi.org/10.5281/zenodo.13235278>,
#>   <http://www.roald-arboel.com/trackballr/>.
#> 
#> A BibTeX entry for LaTeX users is
#> 
#>   @Misc{roaldarbol:2024,
#>     title = {trackballr: Read trackball files},
#>     author = {Mikkel Roald-Arbøl},
#>     year = {2024},
#>     doi = {10.5281/zenodo.13235278},
#>     url = {http://www.roald-arboel.com/trackballr/},
#>     abstract = {Read trackball data into a standard format},
#>     version = {0.1.1},
#>   }
```
