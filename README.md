---
output: github_document
---

<!-- README.md is generated from README.Rmd. Please edit that file -->



# nwmTools <img src="man/figures/logo.png" width=130 height = 130 align="right" />

<!-- badges: start -->
[![Build Status](https://travis-ci.org/mikejohnson51/nwmHistoric.svg?branch=master)](https://travis-ci.org/mikejohnson51/nwmHistoric)
[![experimental](http://badges.github.io/stability-badges/dist/experimental.svg)](http://github.com/badges/stability-badges)
[![Dependencies](https://img.shields.io/badge/dependencies-9/33-orange?style=flat)](#)
[![LifeCycle](man/figures/lifecycle/lifecycle-experimental.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![Website deployment](https://github.com/mikejohnson51/nwmTools/actions/workflows/pkgdown.yaml/badge.svg)](https://github.com/mikejohnson51/nwmTools/actions/workflows/pkgdown.yaml)
[![R CMD Check](https://github.com/mikejohnson51/nwmTools/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/mikejohnson51/nwmTools/actions/workflows/R-CMD-check.yaml)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://choosealicense.com/licenses/mit/)
[![Project Status: Concept](https://www.repostatus.org/badges/latest/concept.svg)](https://www.repostatus.org/#concept)
<!-- badges: end -->

# Description

`nwmTools` facilitates access to NOAA National Water Model operational and historic data through a convenient API.

# Features

 * Query operational NWM channel output and format for timeseries access
 * Access historic NWM Reanalysis v1.2 or 2.0 data by feature 
 * Data requests can be constrained temporally, and adjusted for timezone
 * Functions for finding appropriate NHD and NWIS Identifiers
 * Family of aggregate functions to group and summarize data to new time periods
 * On-call shiny app for data exploration (in development)
 
## Installation


```r
remotes::install_github('mikejohnson51/nwmTools')
```


# Collaborators:

[Mike Johnson](https://mikejohnson51.github.io/) 
# Data Documentation:

[HydroShare](https://www.hydroshare.org/resource/89b0952512dd4b378dc5be8d2093310f/)

# Support:

This effort is supported by the Consortium of Universities for the Advancement of Hydrologic Science, Inc. under the 2020 HydroInformatics Fellowship. See program [here](https://www.cuahsi.org/data-models/hydroinformatics-innovation-fellowship/)

 <img src="man/figures/cuahsi-logo.png" width=180 height = 40 align="right" />
