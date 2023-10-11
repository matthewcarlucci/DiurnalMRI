#### Data Importing and Wrangling

# For consistent ordering of measures 
fig1_modord <- c("cbf", "qt1", "gm_md", "fa", "wm_qt1", "md", "ct", "sa", "vol")

# For converting legacy metric names
int2extlab <- c(
  "cbf" = "CBF", "qt1" = "GM-qT1", "gm_md" = "GM-MD", "fa" = "WM-FA",
  "wm_qt1" = "WM-qT1", "md" = "WM-MD", "ct" = "CT", "sa" = "SA", "vol" = "GMV",
  "weight" = "Weight"
)

# Ensuring dx0 subjects are not included
subsinstud <- c(
  "C001", "C005", "C006", "C008", "C014", "C024", "C025", "C026",
  "C027", "C028", "C029", "C031", "C037", "C040", "C041", "C053"
)
# dx01
subsinstuddx01 <- c(
  "C001", "C005", "C006", "C008", "C014", "C024", "C025", "C026",
  "C027", "C028", "C029", "C031", "C037", "C040", "C041", "C053",
  "C050", "C054", "C055", "C056", "C057", "C060", "C063", "C068"
)

# Condensed for plots. Is this used?
int2extsubs <- paste0("C", substr(subsinstuddx01, 3, 4))
names(int2extsubs) <- subsinstuddx01

# Not used yet?
munits <- c(
  "cbf" = "ml.g^-1.min^-1",
  "qt1" = "ms",
  "gm_md" = "0.1 mm^2s^-1",
  "fa" = "1",
  "wm_qt1" = "ms",
  "md" = "0.1 mm^2s^-1",
  "ct" = "mm",
  "sa" = "10^3 mm^2",
  "vol" = "10^3 mm^3",
  "weight" = "kg"
)

# Find sessions that happen after 24 on day 1.
# An additional filter of time<12 helps with solve sessions with times before and after 24
session_is_day2 <- function(session, time) {
  (session >= 6 & session <= 9) & time < 12
}
