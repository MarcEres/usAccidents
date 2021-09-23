---
output: github_document
---

<!-- README.md is generated from README.Rmd. Please edit that file -->



# usAccidents

<!-- badges: start -->
<!-- badges: end -->



# US FARS (Fatality Analysis Recording System)

This package sole objective is to produce graphics of the accidents in the US.

## The `make_filename` function

This function outputs a filename.

## The `fars_read` function

Based on the files present of US FARS, the function will read the files matching
filename.

## The `fars_read_years` function

This functions uses `make_filename` and `fars_read` to generate a list.

## The `fars_summarize_years` function

This function outputs a summary.

## The `fars_map_state` function

Plot a graphic showing the accidents in each State of US.

# Dependencies

* dplyr
* magrittr
* tidyr
* rlang
* graphics
* maps
