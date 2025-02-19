<!-- README.md is generated from README.Rmd. Please edit that file -->

# `aplot` for decorating a plot with associated information

[![License:
Artistic-2.0](https://img.shields.io/badge/license-Artistic--2.0-blue.svg)](https://cran.r-project.org/web/licenses/Artistic-2.0)
[![](https://img.shields.io/badge/devel%20version-0.2.5-blue.svg)](https://github.com/YuLab-SMU/aplot)
[![](https://img.shields.io/github/languages/code-size/YuLab-SMU/aplot.svg)](https://github.com/YuLab-SMU/aplot)
[![](https://img.shields.io/github/last-commit/YuLab-SMU/aplot.svg)](https://github.com/YuLab-SMU/aplot/commits/master)
<br>
[![](https://www.r-pkg.org/badges/version/aplot?color=green)](https://cran.r-project.org/package=aplot)
[![](http://cranlogs.r-pkg.org/badges/grand-total/aplot?color=green)](https://cran.r-project.org/package=aplot)
[![R build
status](https://github.com/YuLab-SMU/aplot/workflows/rworkflows/badge.svg)](https://github.com/YuLab-SMU/aplot/actions)
[![](https://codecov.io/gh/YuLab-SMU/aplot/branch/master/graph/badge.svg)](https://app.codecov.io/gh/YuLab-SMU/aplot)

For many times, we are not just aligning plots as what ‘cowplot’ and
‘patchwork’ did. Users would like to align associated information that
requires axes to be exactly matched in subplots, e.g. hierarchical
clustering with a heatmap. Inspired by the ‘Method 2’ in ‘ggtree’ (G Yu
(2018) <doi:10.1093/molbev/msy194>), ‘aplot’ provides utilities to
aligns associated subplots to a main plot at different sides (left,
right, top and bottom) with axes exactly matched.

## :writing_hand: Authors

Guangchuang YU

School of Basic Medical Sciences, Southern Medical University

<https://yulab-smu.top>

## :arrow_double_down: Installation

Get the released version from CRAN:

``` r
install.packages("aplot")
```

Or the development version from github:

``` r
## install.packages("remotes")
remotes::install_github("YuLab-SMU/aplot")
```

## :book: Vignette

For more details, please refer to the [online
vignette](https://yulab-smu.top/aplot).
