
# Dataset descriptions

## `private/`

Subject-level participant characteristics.

## `reference_files/`

Expanded metadata files for ICBM and HCP_MMP1 atlas ROIs derived from: 
https://neuroimaging-core-docs.readthedocs.io/en/latest/pages/atlases.html

## `available_upon_request/` 

Contains subject-level data which can be provided upon request (see Data Availability).

### File descriptions

Contains the following files:

-   `whole_brain_data.csv` - average of each MRI metric across the
    relevant atlas for each time point.
-   `processed_ROI_data.csv` - average of each MRI metric within each
    atlas ROI for each time point.
-   `whole_brain_techvar_data.csv` - average of each MRI metric across
    the relevant atlas for each time point and back-to-back replicate.
-   `weights_kg.csv` - body weight at each time point.
-   `actigraphy_3day.csv` - steps registered by the FitBit device each
    minute for the 3 days of observation.
-   `actigraphy_sleep.csv` - sleep intervals registered by the FitBit
    device during the 3 days of observation.

### File content descriptions

In all CSVs `subject` is a text field with anonymized subject identifiers. 

MRI metric names map to the published names as follows:

```r
# For converting legacy metric names
int2extlab <- c(
  "cbf" = "CBF", 
  "qt1" = "GM-qT1", 
  "gm_md" = "GM-MD", 
  "fa" = "WM-FA",
  "wm_qt1" = "WM-qT1", 
  "md" = "WM-MD", 
  "ct" = "CT", 
  "sa" = "SA", 
  "vol" = "GMV",
  "weight" = "Weight"
)
```

#### Additional `whole_brain_data.csv` columns:

-   `measure` - text, MRI metric
-   `session` - integer, sequential session identifier for each subject
-   `dx` - integer, 0 indicates participant is a control, 1 indicates
    participant has a bipolar disorder diagnosis.
-   `ltime` - numeric, "linear" time in hours since midnight of day 1
    (of 2)
-   `time` - numeric, time-of-day in hours since midnight
-   `value` - MRI metric value in the reported units

#### Additional `processed_ROI_data.csv` columns:

-   `measure` - text, MRI metric
-   `roi` - ROI name for the given atlas
-   `session` - integer, sequential session identifier for each subject
-   `dx` - integer, 0 indicates participant is a control, 1 indicates
    participant has a bipolar disorder diagnosis.
-   `ltime` - numeric, "linear" time in hours since midnight of day 1
    (of 2)
-   `time` - numeric, time-of-day in hours since midnight
-   `value` - MRI metric value in the reported units

#### Additional `whole_brain_techvar_data.csv` columns:

-   `measure` - text, MRI metric
-   `session` - integer, sequential session identifier for each subject
-   `value` - MRI metric value in the reported units
-   `repeat` - Back-to-back replicate identifier

#### Additional `weights_kg.csv` columns:

-   `session` - integer, sequential session identifier for each subject
-   `dx` - integer, 0 indicates participant is a control, 1 indicates
    participant has a bipolar disorder diagnosis.
-   `ltime` - numeric, "linear" time in hours since midnight of day 1
    (of 2)
-   `time` - numeric, time-of-day in hours since midnight
-   `weight` - body weight in

#### Additional `actigraphy_3day.csv` columns:

-   `hours` - numeric, time in hours since midnight of day 1 (of 3)
-   `value` - integer, steps registered by the FitBit device each
    minute for the 3 days of observation
-   `group` - text, indicates whether participant is a control or has a bipolar disorder diagnosis

#### Additional `actigraphy_sleep.csv` columns:

-   `group` - text, indicates whether participant is a control or has a bipolar disorder diagnosis
-   `start_time` - numeric, sleep start time in hours after midnight on
-   `end_time` - numeric, sleep end time in hours after midnight on
    day 1 (of 3)
