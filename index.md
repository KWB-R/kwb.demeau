[![Appveyor build Status](https://ci.appveyor.com/api/projects/status/yt945q4xcxbixfy5/branch/master?svg=true)](https://ci.appveyor.com/project/KWB-R/kwb-demeau/branch/master)
[![Travis build Status](https://travis-ci.org/KWB-R/kwb.demeau.svg?branch=master)](https://travis-ci.org/KWB-R/kwb.demeau)
[![codecov](https://codecov.io/github/KWB-R/kwb.demeau/branch/master/graphs/badge.svg)](https://codecov.io/github/KWB-R/kwb.demeau)
[![Project Status](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/kwb.demeau)]()

Heat tracer study for SVH site using USGS VS2DH model.


## Installation

For details on how to install KWB-R packages checkout our [installation tutorial](https://kwb-r.github.io/kwb.pkgbuild/articles/install.html).

```r
### Optionally: specify GitHub Personal Access Token (GITHUB_PAT)
### See here why this might be important for you:
### https://kwb-r.github.io/kwb.pkgbuild/articles/install.html#set-your-github_pat

# Sys.setenv(GITHUB_PAT = "mysecret_access_token")

# Install package "remotes" from CRAN
if (! require("remotes")) {
  install.packages("remotes", repos = "https://cloud.r-project.org")
}

# Install KWB package 'kwb.demeau' from GitHub"

remotes::install_github("kwb-r/kwb.demeau") 
```
