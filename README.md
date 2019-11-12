
<!-- README.md is generated from README.Rmd. Please edit that file -->

# gensci.stm

<!-- badges: start -->

<!-- badges: end -->

This package bundles functions to generate topic models describing
gender in science reports using the `stm` package.

## Installation

You can install this package from [GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("dtburk/gensci.stm")
```

This will also install a few packages from CRAN which this package uses.
However, this package also uses another package that is not on CRAN
called `texanaaid` (TEXt ANAlysis AIDe). Use the same function as above
to install `texanaaid` from GitHub.

``` r
remotes::install_github("dtburk/texanaaid")
```

For more information on `texanaaid`, check out the [GitHub
repository](https://github.com/dtburk/texanaaid).

## Usage

This package includes an R Markdown template that lays out all the steps
needed to generate a topic model. To use this template, first make sure
you have installed the package, then in RStudio, navigate to File \> New
File \> R Markdown…, click on “From Template” on the left side of the dialog
box, select “Construct a topic model of gender in science reports”, and
click OK.

This will open a new R Markdown file that contains all the code
necessary to generate a topic model for gender in science reports, from
extracting text from the PDF reports, all the way to producing output to
help interpret the model. Before knitting the R Markdown file with
RStudio’s “Knit” button, however, you must set the file paths and
analysis parameters listed in the section “Set file paths and
parameters” of the template.

A good workflow for analysis would be to create a new directory for each
new topic model you want to generate with different parameters, then use
the included template to create an analysis script in that directory.
This will ensure that the code used to generate a topic model is kept
tightly bundled with the model output.

Note: The packages `gensci.stm` and `texanaaid` hard-code many
data-cleaning and analysis decisions that were made iteratively as the
analysis of gender in science reports developed. These decisions may
need to be revisited if the analysis is extended beyond the original set
of documents. Changes can be made by forking the package repositories on
GitHub and editing the functions found in the “R” directory of each
package.
