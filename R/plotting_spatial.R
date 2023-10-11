# https://github.com/ggseg/ggsegGlasser/issues/1
# lh_L_10pp is not displayed until/unless this bug is fixed.

# To join to results data frames before ggseg
get_glasser_spatial_meta <- function() {
  meta <- readxl::read_xlsx("data/reference_files/MMP1_CSV_file.xlsx")

  # 7Pl and 7pl in meta and is 7PL in our data and ggseg
  meta <- meta %>%
    mutate(region = ifelse(region == "7Pl", "7PL", region)) %>%
    mutate(regionName = ifelse(regionName == "7Pl_L", "7PL_L", regionName)) %>%
    mutate(regionName = ifelse(regionName == "7Pl_R", "7PL_R", regionName))

  meta$roi <- paste(meta$LR, gsub("_[LR]", "", meta$regionName), "ROI", sep = "_")
  meta$hemi <- ifelse(meta$LR == "L", "left", "right")
  return(meta)
}

# To join to results data frames before ggseg
get_icbm_spatial_meta <- function() {
  meta <- readxl::read_xlsx("data/reference_files/JHU_ICBM81.xlsx")
  meta <- meta[-c(1:2), ]
  meta$region <- meta$label %>%
    gsub(" ", "_", .) %>%
    gsub("\\(", "", .) %>%
    gsub("\\)", "", .) %>%
    gsub("\\/", "or", .)
  meta$roi <- meta$region
  meta <-
    meta %>%
    mutate(region = gsub("\ R$", "", label)) %>%
    mutate(region = gsub("\ L$", "", region)) %>%
    filter(region != "Unclassified")
  return(meta)
}

# To apply after joining spatial meta and before ggseg
icbm_preprocess <- function(df) {
  df %>%
    # This is missing from icbm_3d for some reason
    filter(region != "Fornix (column and body of fornix)") %>%
    mutate(roi = str_pad(index, 3, pad = "0"))
}

get_subcor_ggseg_atlas <- function() {
  library(ggseg)
  library(ggsegGlasser)
  meta <- get_glasser_spatial_meta()
  # get ROI names for CBF/GM-MD/FA
  # i.e. ROIs not in Glasser are subcortical
  pdf <- 
    read_csv("data/available_upon_request/processed_ROI_data.csv") %>% 
    filter(measure=="cbf") |> select(roi) |> unique() |> 
    #
    full_join(meta) %>%
    filter(is.na(`z-cog`)) %>% # To remove non-subcortical
    rename(label = roi) %>%
    ungroup() %>%
    select(label)

  myaseg <- aseg
  myaseg$data <- myaseg$data %>% filter(label %in% pdf$label)

  return(myaseg)
}
