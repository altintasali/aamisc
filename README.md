<!-- badges: start -->
[![R-CMD-check](https://github.com/altintasali/aamisc/workflows/R-CMD-check/badge.svg)](https://github.com/altintasali/aamisc/actions)
<!-- badges: end -->

# aamisc

Miscellaneous R functions by Ali Altintas. The functions with a good use will be first published in this package. Who knows, maybe they will magically appear in future packages as well. Until that day, this is their home. 

## Install

Please install `devtools` if you haven't yet.

```r
install.packages("devtools")
```

Currently, there are some issues with the CRAN vs. Bioconductor package compatibility and some packages are archived at CRAN. Therefore, we need to do a manual installation for those packages. I know that this is not ideal, but I have no time to fix it properly at the moment. Alright, let's continue anyways since you heard my confession... 

Bioconductor packages:

```r
if (!require("BiocManager", quietly = TRUE))
    install.packages("BiocManager")

BiocManager::install(c("qvalue", "rain", "limma"))
```

Archived CRAN packages:

```r
url <- "https://cran.r-project.org/src/contrib/Archive/HarmonicRegression/HarmonicRegression_1.0.tar.gz"
pkgFile <- "HarmonicRegression_1.0.tar.gz"
download.file(url = url, destfile = pkgFile)
install.packages(pkgs=pkgFile, type="source", repos=NULL)
file.remove(pkgFile)
```

Then, you finally install the `aamisc` package by using:
```r
library(devtools)
install_github("altintasali/aamisc")
```
