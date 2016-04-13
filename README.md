[![Travis-CI Build Status](https://travis-ci.org/epiviz/epivizrData.svg?branch=master)](https://travis-ci.org/epiviz/epivizrData)

# epivizrData

The `epivizrData` packages includes methods supporting serving data 
for visualization applications of data from R/Bioconductor objects. It is primarily used to serve data from interactive R/Bioconductor sessions to the epiviz JS application [http://epiviz.github.io](http://epiviz.github.io). These functions have been extracted from the `epivizr` [Bioconductor package](http://bioconductor.org/packages/release/bioc/html/epivizr.html) into its own package for easier use and maintenance.

It is designed to receive and send requests through WebSocket connections provided by the [`epivizrServer` package](http://github.com/epiviz/epivizrServer).

## Installation

You can install epivizrData from github with:

```R
# install.packages("devtools")
devtools::install_github("epiviz/epivizrData")
```

## Example

See package vignette for more detail.