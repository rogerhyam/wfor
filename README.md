
<!-- README.md is generated from README.Rmd. Please edit that file -->

# wfor - The World Flora Online Plant List R Package

<!-- badges: start -->
<!-- badges: end -->

## Purpose

This R package facilitates the reconciliation of multiple data sets
containing plant names to each other via standardised World Flora Online
name IDs. This enables the merging of data based on a global, consensus
taxonomy in a reproducible way. It will also report on provenience of
the name matching process and current taxonomy of the names used in the
analysis for subsequent publication.

Think Reference Manager for plant names!

There is an existing WFO R package in CRAN for standardizing plant names
(<https://cran.r-project.org/web/packages/WorldFlora/index.html>). It
differs from wfor in that it requires a download of the complete name
list in Darwin Core format whilst wfor relies on calling the API of the
latest version of the list. The existing package can also use other
lists of names in Darwin Core format to match against. The two packages
are complementary. Hopefully we will see yet more packages released that
work with WFO data in different ways.

## Status

One round of development has created this minimum viable product. It
still needs more work but you are welcome to have a play. We plan to
submit an improved version to CRAN by the end of 2023 In sh’Allah!

## Installation

You can install the development version of wfor from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("rogerhyam/wfor")
```

## Workflow

You have one or more data.frames containing observational or other data.
One of the columns contains the names of plant species as strings of
characters. They might be quite variable and come from a variety of
sources. You need to standardize these strings so that they map to a
shared lookup table. You will then be able to map between different data
sets (merge data.frames) in your own study and those in other people’s
studies in an explicit and repeatable way.

The function wfo_match_df_names takes your data.frame and returns a new
version with four columns added:

1.  wfo_id contains the WFO ID of a name in the WFO Plant list.
2.  wfo_name contains the string of the full name as WFO has it.
3.  wfo_method contains a flag of how the name match was made,
    automatically or manually.
4.  wfo_path contains the taxonomic path to the name in the current WFO
    classification. This is a sanity check to be sure you haven’t
    matched your orchid name against a bryophyte by mistake!

You may have to run a data.frame through the function several times and
do some manual checking to get 100% coverage. Once that is done you can
keep a copy of the name cache which is a mapping between the strings
found in your data and the WFO IDs. This can be used to speedup the
processing of subsequent datasets and prevent you from making the same
manual selection twice.

Coming soon: Once you have marked up your data with WFO IDs there are
various functions that allow you to generate reports on your data for
inclusion in publications. These might include species lists in
alphabetical and/or taxonomic order with full details of place of
publication, taxonomic sources and type specimens when available.

## Example

This is a basic example which shows you how to solve a common problem:

    library(wfor)

    # auto match the Belgian Magnoliopsida test data
    `mags_example <- wfo_match_df_names(Belgian_Magnoliopsida_sp_ssp_var_2011, name_col="scientific_name", authors_col="authorship", interactive=FALSE)`
    # check how many  have been matched using the [wfo_stat_matches()] function.
    `wfo_stat_matches(mags_example)`
    # do a second pass interactively to resolve unmatched names (or until you get tired)
    `mags_example <- wfo_match_df_names(mags_example, name_col="scientific_name", authors_col="authorship", interactive=TRUE)`
    # do a third pass matching names to the nearest genus
    `mags_example <- wfo_match_df_names(mags_example, name_col="scientific_name", authors_col= "authorship", fallback_to_genus=TRUE)`
