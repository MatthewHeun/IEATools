---
output: 
  html_document: 
    keep_md: yes
---

<!-- Do not edit CONTRIBUTING.md.  -->
<!-- Be sure to edit CONTRIBUTING.Rmd, which is used to create CONTRIBUTING.md -->



# Contributing to `IEATools`

This document outlines how to propose a change to the `IEATools` package.

## Fixing typos

You can fix typos, spelling mistakes, or grammatical errors in the documentation directly 
using the GitHub web interface, so long as the changes are made in the _source_ file.
This generally means you'll need to edit
[roxygen2 comments](https://roxygen2.r-lib.org/articles/roxygen2.html) in a `.R`, not a `.Rd` file.
You can find the `.R` file that generates the `.Rd` by reading the comment in the first line.

## Bigger changes

If you want to make a change bigger than fixing typos,
first file an issue on GitHub and make sure someone from the team agrees that your proposed change is needed.
If you’ve found a bug, please file an issue that illustrates the bug with a minimal
[reprex](https://www.tidyverse.org/help/#reprex).

### Pull request process

* Fork the package and clone onto your computer.
* Install all development dependencies with `devtools::install_dev_deps()`, and 
  make sure the package passes R CMD check by running `devtools::check()`.
  If R CMD check doesn't pass cleanly, it's a good idea to ask for help before continuing.
* Create a Git branch for your pull request (PR).
  We recommend using `usethis::pr_init("brief-description-of-change")`.
* Make your changes, commit to git, and then create a PR by running `usethis::pr_push()`.
  Follow the prompts in your browser.
  The title of your PR should briefly describe the change.
  The body of your PR should contain `Fixes #issue-number`.
* For user-facing changes, add a bullet to the top of `NEWS.md` 
  (i.e., beneath "which always resolves to the latest release.").

### Code style

* New code should be written in a style that matches the rest of the code in `IEATools`. 
* We use [roxygen2](https://cran.r-project.org/package=roxygen2), 
  with [Markdown syntax](https://cran.r-project.org/web/packages/roxygen2/vignettes/rd-formatting.html), 
  for documentation.
  
### Tests

* We use [testthat](https://cran.r-project.org/package=testthat) for unit tests.
* We maintain 100% test coverage. Contributions that reduce test coverage to less than 100% will be rejected.

## Code of Conduct

Please note that the RCLabels project is released with a
[Contributor Code of Conduct](CODE_OF_CONDUCT.md). 
By contributing to this
project you agree to abide by its terms.

## Attribution

This guide to contributing was adapted from the
[Contributing guide for the `usethis` package](https://usethis.r-lib.org/CONTRIBUTING.html).
