# CogLearn: Analysis R package

R package with generic analysis tools for CogLearn data generated with the CogLearn Unity package. Brought to you by [Bachlab](http://bachlab.org) at [Universität Bonn](https://www.uni-bonn.de/en) and [University College London](https://www.ucl.ac.uk)

Documentation: https://bachlab.gitlab.io/coglearn/coglearn/

# How to install the package from GitLab:

You can install the development version of CogLearn by running the following code

```r
# install.packages("devtools")
devtools::install_git("git@gitlab.com:bachlab/coglearn/coglearn.git", ref = "main")

```

## NOTE 
(1)the CogLearn package is not currently available on CRAN as it heavily uses 
tidyverse-style non-standard evaluation which is not accepted on CRAN. 
This may change in the future.

(2) After installation, run the following code to use the CogLearn package

```r
library(tidyverse)
library(CogLearn)

```

## Scripts

Scripts containing the functions for this package are located in the `R` directory. All functions here are added and available to use when you install the package.

## Testing

So far no unit tests have been developed.

## Development

Several shortcut functions for development are in the `shortcuts.R` script. You can use these to build documentation, the documentation website, run tests, etc.

General information on package development: https://r-pkgs.org/man.html

How to use tidyverse in a package: https://tidyr.tidyverse.org/articles/in-packages.html

Quick tips:

* Functions should be in scripts stored in the `R` folder.
* If you need to use functions from another package, you should call them explicitly (e.g. `dplyr::filter(...)`). Dependencies should be added to the imports in the DESCRIPTION file.
* You can use RStudio to [generate documentation skeleton for functions](https://stackoverflow.com/a/30675146/5024009).
* Follow the [Tidyverse Style Guide](https://style.tidyverse.org/).

## Some types of functions and naming conventions

tbc
