# Environment which is set at the start of all pages

# All packages are loaded here in every notebook for ease
library(data.table)
library(ggplot2)
library(patchwork)
library(dplyr)
library(tidyverse)
library(reshape2)
library(broom)
library(readr)
library(knitr)
library(DiscoRhythm)

### Loading DiurnalMRI custom functions and variables
# Stats implementations and other generic helper functions
source("R/cosinor.R")
source("R/utils.R")
source("R/plotting_spatial.R")
# Objects and functions that are specific to the DiurnalMRI dataset
source("R/plotting.R")
source("R/fns.R")

# Ensure exe relative to project root
here::i_am("code/setup.R") # May be more robust than here::here()?

theme_set(theme_bw()) # ggplot theme

# Ensure output dirs exists
dir.create("output", showWarnings = FALSE)
dir.create("output/source_data", showWarnings = FALSE)

# Use to track down warnings and messages.
# Direct to log files by commenting out.
# knitr::opts_chunk$set(warning = T,message = T)

# To postprocess figures, pdfs are always generated alongside pngs
knitr::opts_chunk$set(dev = c("png", "pdf"))

### Analysis FLAGS and global variables
# default number of permutations
Nperm <- 1e2 # permutations for interactive development
if (!interactive()) Nperm <- 1e4

