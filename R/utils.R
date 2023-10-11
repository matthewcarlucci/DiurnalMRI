# avoid data.table::dcast warnings
dcast <- reshape2::dcast

#' Summary Of Pvalues
#'
#' Returns a summarized table showing the counts and percentages
#' for 4 different levels of p-values: 0.05, 0.01, FDR and Bonferroni.
#'
#' @param p a vector of raw p-values
#' @param as_kable return results for nice Rmarkdown formatting
#'
#' @export
summaryPvalTable <- function(p, as_kable = FALSE) {
  ps <- list()
  ps[[1]] <- p < 0.05
  ps[[2]] <- p < 0.01
  ps[[3]] <- p.adjust(p, method = "fdr") < 0.05
  ps[[4]] <- p.adjust(p, method = "bonferroni") < 0.05
  
  levs <- c("p < 0.05", "p < 0.01", "FDR", "Bonferroni")
  perc <- sapply(ps, function(p) round(mean(p, na.rm = TRUE), 4)) * 100
  sums <- sapply(ps, function(p) sum(p, na.rm = TRUE))
  
  ret <- data.frame(
    level = levs, count = sums, percent = paste0(perc, "%"),
    stringsAsFactors = FALSE
  )
  
  if (as_kable) ret <- knitr::kable(ret)
  return(ret)
}

#' Summarize p-values for multiple groups
#'
#' @param dat tidy format data.frame, with pvalue and grp columns
#'
#' @import dplyr
#' @importFrom tidyr unnest
#'
#' @export
groupWiseSummaryPvalTable <- function(dat, as_kable = FALSE) {
  ret <- dat %>%
    group_by(grp) %>%
    summarize(res = list(summaryPvalTable(pvalue))) %>%
    unnest(res) %>%
    mutate(val = paste0(percent, " (", count, ")")) %>%
    reshape2::dcast(level ~ grp, value.var = "val")
  if (as_kable) ret <- knitr::kable(ret)
  return(ret)
}