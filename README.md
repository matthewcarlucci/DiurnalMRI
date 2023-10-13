# Overview

Source code for “Diurnal oscillations of MRI metrics in the brains of male participants”. Applies the “Data Analysis and Statistical Methods” steps to the processed MRI data to produce results and figures. The executed notebooks are contained in `docs/` and the latest version of this can be viewed online [here](https://matthewcarlucci.github.io/DiurnalMRI).

# Statistical code demonstration

The main data analysis procedures are implemented in `R/cosinor.R`. Methods are demonstrated in a notebook (`code/methods/methods_walkthrough.Rmd`) with simple examples on simulated datasets. All code can be executed on standard hardware. Installation of the required software would take approximately 5-30 minutes. The software was tested on macOS 12.4 and Windows 11. 

The dependencies for running these methods can be installed in R >4.1.3 by:

```
# Install the remotes package to allow installation of a local package
install.packages("remotes")
# Replace below path with local path to the source code to install the DiurnalMRI package and dependencies
remotes::install_github("matthewcarlucci/DiurnalMRI")
```

# Data availability

As indicated in the article, processed MRI data are available upon request. 

>All processed data generated in this study are available as a dataset on the Zenodo platform (https://doi.org/10.5281/zenodo.8360149). This dataset contains subject-level: processed whole brain and ROI MRI data, body-weight data, and processed actigraphy data. Anonymized subject level data are available to other investigators under restricted access in compliance with institutional ethics and privacy policies. Access requests can be submitted via Zenodo. The raw MRI data are protected and are not available due to institutional ethics and privacy restrictions. Source Data are provided with this paper where privacy and consent restrictions allow.

This Zenodo dataset is expected (by the first steps in `code/`) to be found within the currently empty `data/available_upon_request/` directory. Available data are further described in `data/`.

# Analysis archive

The notebooks found within the `code/` directory read in the `data/` and produce the results presented in the article. They are divided into:

- `code/results/` - Obtaining statistics derived from whole brain, ROI, and body-weight data.
- `code/display_items/` - Figures and tables are generated here.
- `code/methods/` - Demonstrations of methods and methods code.

## Installing all R packages

After cloning this repository and obtaining the `data/available_upon_request/` the following R packages must be installed:

```r
c("rmarkdown", "ggplot2", "stats", "ggseg", "ggsegGlasser", "ggseg3d", 
"ggsegICBM", "plotly", "ggpubr", "patchwork", "circular", "viridis", 
"lubridate", "knitr", "flextable", "htmltools", "cosinor2", "data.table", 
"DiscoRhythm", "dplyr", "SummarizedExperiment", "metap", "here", 
"rstatix", "broom", "readr", "reshape2", "tidyverse", "matrixStats", 
"readxl", "scales")
```

To ensure exact versions are used, [renv](https://rstudio.github.io/renv/index.html) `restore()` can be used from the project root which uses the `renv.lock` file to install exact versions of these packages and their dependencies. This may take approximately 1 hour and require dependencies to be installed.

## Order of execution

All code is expected to execute with the working directory set to the root of the source code, e.g. with `setwd('path/to/DiurnalMRI_source_code')`. Many notebooks depend on the outputs of other notebooks. In general, `code/display_items/` were executed after the `code/results/`, however, specific dependence between notebook executions are described in the `scikick.yml` configuration file. 

For example, executing notebooks in the following order should result in a successful run.

```
code/results/WB_S-cosinor.Rmd
code/results/WB_G-cosinor.Rmd
code/results/WB_acrophase_agnostic_tests.Rmd
code/results/ROI_S-cosinor.Rmd
code/results/ROI_G-cosinor.Rmd
code/results/ROI_acrophase_agnostic_tests.Rmd
code/results/body_weight.Rmd
code/results/WB_cosinor_stats_BPD.Rmd
code/results/WB_BPD_diffs.Rmd
code/results/ROI_cosinor_stats_BPD.Rmd
code/display_items/table_S3_adjweight.Rmd
code/display_items/figure_S5_actigraphy.Rmd
code/display_items/table_1.Rmd
code/display_items/table_2.Rmd
code/display_items/figure_2.Rmd
code/display_items/figure_3.Rmd
code/display_items/figure_4.Rmd
code/display_items/table_S1_techvar.Rmd
code/display_items/figure_S4_spatialG.Rmd
code/display_items/figure_S6_phasePSQI.Rmd
code/methods/methods_walkthrough.Rmd
code/methods/Gcosinor_implementation_check.R
```

The computations take approximately 30 minutes. No individual page should take more than 2 minutes.

Note: The `data/private/` for `table_1.Rmd` and `figure_S6_phasePSQI.Rmd` are not available and therefore have been modified to exit without error when the data are not found.

### Automated archive build

The configured order of execution can be automated and archived with [scikick](https://github.com/matthewcarlucci/scikick) (`sk run`) to produce the website found within `docs/`.

# Glossary

WB: Whole-brain

ROI: Region-of-interest

BPD: Participants diagnosed with bipolar disorder

