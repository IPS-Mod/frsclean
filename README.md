






<!-- README.md is generated from README.Rmd. Please edit that file -->

# Family Resources Survey Data Cleaning <img src="hex-frsclean.png" align="right" style="padding-left:10px;background-color:white;" width="100" height="100"/>

<!-- badges: start -->

[![Project Status: WIP – Initial development is in progress, but there
has not yet been a stable, usable release suitable for the
public.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![](https://img.shields.io/badge/doi-10.17605/OSF.IO/GNSEJ-green.svg)](https://doi.org/10.17605/OSF.IO/GNSEJ)

<!-- badges: end -->

## Citation

Morris, D. (2023). frsclean: An R package for cleaning the Family
Resources Survey data. R package version \[x.x.x\]. University of
Sheffield. doi: <https://doi.org/10.17605/OSF.IO/GNSEJ>

## Motivation

The motivation for `frsclean` is to develop a set of standard functions
for processing raw data from the Family Resources Survey (FRS), which is
a survey of households representative of the UK population primarily
collecting detailed information on income, taxes, and benefits.

The code in this package is adapted from stata code used to clean the
FRS data to provide inputs to the
[UKMOD](https://www.microsimulation.ac.uk/ukmod/) tax/benefit
micro-simulation model (Richiardi 2021). Variable names are consistent
with those used in the UKMOD input data.

## Usage

The `frsclean` package contains functions which read in the raw data
files, process them into clean output variables, and combines all data
files into a single output data table. The functions also create
real-terms values for nominally valued monetary variables (earnings and
wages), allowing the user to select either the CPIH or RPI.

The **inputs** are the raw FRS data files obtained from the [UK Data
Service](https://ukdataservice.ac.uk/). These must be the tab delimited
versions of the data, **not Stata or SPSS**, and must be placed together
in a single directory.

A typical workflow for using the package looks as follows, with the read
data function for each individual year contained within a global
cleaning function which applies each cleaning function in turn. The
collection of global cleaning functions for each year are then wrapped
in a function to combine years of data.

``` r

### Define arguments 

root <- "C:/"
file <- "Documents/Datasets/Family Resources Survey/tab"
ages <- 16:64
years <- 2020
keep_vars <- NULL
complete_vars <- NULL

#########################################
### Read in and combine years of data ###

data <- frsclean(root = root,
                 file = file,
                 ages = ages,
                 years = years,
                 keep_vars = keep_vars,
                 complete_vars = complete_vars)
```

The **output** of these functions are a single data table of processed
FRS data

## Installation

`frsclean` is available on GitHub. If you are on a Windows machine you
will also need to [install
Rtools](https://www.rdocumentation.org/packages/installr/versions/0.22.0/topics/install.Rtools).  
Once that is sorted, you can install the latest version or a specified
version from GitHub with:

``` r
#install.packages("devtools")
#install.packages("getPass")
#install.packages("git2r")

devtools::install_git(
  "https://github.com/djmorris1989/frsclean.git", 
  ref = "x.x.x",
  build_vignettes = TRUE
)
```

## References

<div id="refs" class="references csl-bib-body hanging-indent">

<div id="ref-ukmod-2021" class="csl-entry">

Richiardi, D. Popova, M. Collado. 2021. “UKMOD a New Tax-Benefit Model
for the Four Nations of the UK.” *International Journal of
Microsimulation* 14 (1): 92–101. <https://doi.org/10.34196/ijm.00231>.

</div>

</div>
