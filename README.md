# Nori: Wrappers and add-ons for BentoBox functions

## Overview

BentoBox gives users the ability to create and precisely control genomic plots in R. Nori extends BentoBox with useful wrappers and additional functionality.

## Additional Features

APA (Aggregate Peak Analysis) Tools:

  1. `makeBedpe()`: Computes all potential paired interactions from a BED file.
  
  2. `filterInterBedpe()`: Filters out interchromosomal interactions that would result in an APA plot that intersects the diagonal.
  
  3. `computeApa()`: Efficiently extracts BEDPE interactions from a `.hic` matrix.
  
  4. `bb_plotApa()`: BentoBox-compatible APA plotting function.
  
Wrapper Functions (coming soon):

  1. `bb_plotMulti()`: Convenience wrapper function for plotting multiple bb_plots.
  
  3. `bb_pageLayout()`: Create a complex, reusable layouts for placing plots.

## Dependencies

1. Currently `rtracklayer` needs to be installed manually. To install

  ```
  if (!requireNamespace("BiocManager", quietly = TRUE))
      install.packages("BiocManager")
  
  BiocManager::install("rtracklayer")
  ```

2. Package BentoBox:

  ```
  remotes::install_github("PhanstielLab/BentoBox", force = T)
  ```

## Installation

To install Nori, use the following code:

```{r}
remotes::install_github(repo = "EricSDavis/Nori", ref = "Fork-Nori", force = T)
```

## Usage

Coming Soon

## Contributors

* [Nicole Kramer](https://github.com/nekramer)
* [Eric Davis](https://github.com/EricSDavis)
* [Sarah Parker](https://github.com/sarmapar)
* [Doug Phanstiel](https://github.com/dphansti)

## License
TBD
