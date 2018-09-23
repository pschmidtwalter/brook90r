
<!-- README.md is generated from README.Rmd. Please edit that file -->
brook90r: run the LWF-BROOK90 hydrological model from within R

Motivation
==========

In hydrology, many R-packages exist that deal with pre- and post-processing of input data and results of hydrological process models. In addition, many ready-to-use algorithms exist in R providing automatic calibration, sensitivity analysis, and parallelisation techniques. In order to make the vast resources of R directly available to the 1D-SVAT model [LWF-BROOK90](https://www.lwf.bayern.de/boden-klima/wasserhaushalt/index.php), *brook90r* was developed to serve as an interface to the LWF-BROOK90 executable commandline program.

Basicly, brook90r features the following functionality in one function call:

-   write input files from climate driving data, model control options and parameters,
-   start the commandline-tool LWF-BROOK90,
-   read and return the created output files.

The model control options thereby let you select different functions for defining aboveground stand dynamics, phenology, and root length density depth distributions. Additionally, a set of pedotransfer functions is provided to derive hydraulic parameters from soil physical properties.

Example
=======

Load brook90r

``` r
library(brook90r)
#> Loading required package: data.table
#> Warning: package 'data.table' was built under R version 3.3.3
```

Load sample meteo and soil data

``` r
data("meteo_slb1")
data("soil_slb1")
```

Set up lists containing default model control options and model parameters

``` r
param.b90 <- MakeParam.B90()
options.b90 <- MakeOptions.B90()
```

Set new start and end dates in model control options

``` r
options.b90$startdate <- as.Date("2002-01-01")
options.b90$enddate <- as.Date("2004-12-31")
```

Derive soil hydraulic properties from soil physical properties using a pedotransfer function

``` r
soil <- cbind(soil_slb1, hydpar_puh2(clay = soil_slb1$clay,
                                     silt = soil_slb1$silt,
                                     sand = soil_slb1$sand,
                                     bd = soil_slb1$bd,
                                     oc.pct = soil_slb1$c_org))
```

Run LWF-Brook90 and store the results in b90.results.slb1

``` r
b90.results.slb1 <- Run.B90(project.dir = "example_run_b90/",
                            param.b90 = param.b90,
                            options.b90 = options.b90,
                            soil = soil,
                            climate = meteo_slb1,
                            path_b90.exe = "b90.exe"
                            )
```

Status
======

The package works as intended and is fully documented.

Usage
=====

The package is not on CRAN, so please use the devtools-package to install directly from github.com:

``` r
devtools::install_github("pschmidtwalter/brook90r")
```

Additionally, you will need to install the *data.table* package. Also important, though not required is Robert Nuske's *vegperiod* package.

``` r
install.packages("data.table", repos="https://cran.rstudio.com/")
install.packages("vegperiod", repos="https://www.nw-fva.de/r-pkgs")
```

Requirements
============

You can use the package's functions without carrying out any water balance simulations. However, the central function *Run.B90* will only work with the windows commandline tool 'b90.exe' which is not publicly available. The interested user can obtain 'b90.exe' directly from the [Bavarian State Institute of Forestry (LWF)](http://www.lwf.bayern.de/), [Departement Soil and Climate.](https://www.lwf.bayern.de/boden-klima/wasserhaushalt/index.php)

Author
======

Paul Schmidt-Walter

License
=======

GPL-3 for the package. License for LWF-BROOK90 is unknown.
