# PubMLST.org webpage scraper



## Description

This R script allows
  - listing all of the available organisms on [PubMLST.org](https://www.pubmlst.org/)
  - listing all of the available schemes for a given organisms (including dates of last changes to schemes and numbers of profiles included in each of the schemes)
  - downloading specified scheme
  - downloading specified scheme profiles

## Requirements
- [tidyverse](https://cran.r-project.org/web/packages/tidyverse/index.html)
- [rvest](https://cran.r-project.org/web/packages/rvest/index.html)
- [knitr](https://cran.r-project.org/web/packages/knitr/index.html)
- [optparse](https://cran.r-project.org/web/packages/optparse/index.html)

In case you would like to use this script you can easily install all the required packages by running the code below in your R session: 

```R
# Listing required packages
required_packages <- c("tidyverse", "rvest", "knitr", "optparse")

# Check if required packages are installed
missing_packages <- setdiff(required_packages, installed.packages()[,"Package"])

# Install missing packages
if (length(missing_packages) > 0) {
  message("Installing missing packages: ", paste(missing_packages, collapse = ", "))
  install.packages(missing_packages)
}
```

## Usage

The script can be used directly from command line. 


To see help

```bash
Rscript --vanilla pubMLST_scheme_scraper.R --help
```

To list available organisms on PubMLST

```bash
Rscript --vanilla pubMLST_scheme_scraper.R -f list_organisms
```

To list available schemes for a given organism

```bash
Rscript --vanilla pubMLST_scheme_scraper.R -f list_organism_schemes -o abaumannii
```

To download scheme profiles for a given organism and scheme ID

```bash
Rscript --vanilla pubMLST_scheme_scraper.R -f download_scheme_profiles -o abaumannii -s 1
```

To download scheme

```bash
Rscript --vanilla pubMLST_scheme_scraper.R -f download_scheme -o abaumannii -s 1
```