#' Fast Multi-Feature Population-Mean Cosinor
#' 
#' Applies the population-mean cosinor to a set of cosinor results across many features 
#' and many individuals to obtain the population-mean statistics (across the individuals) for each feature. 
#' Expects the output of \code{DiscoRhythm::discoODAs(se,method="CS")$CS} cosinor implementation as input, 
#' however, any cosinor implementation can be used which provides sincoef/coscoef/mesor columns.
#' 
#' @param x list, cosinor data.frames from \code{discoODAs(method="CS")$CS} each with
#' identical number of rows and rows matched by feature. If x is a data.frame it is instead assumed
#' to be a single feature where rows represent subjects.
#' 
#' @return data.frame containing the following resulting columns of statistics with one row per feature:
#'  \item{\code{F}}{F statistic of the zero amplitude test}
#'  \item{\code{df1}}{Model degrees of freedom}
#'  \item{\code{df2}}{Residual degrees of freedom}
#'  \item{\code{pvalue}}{P value of the F-test}
#'  \item{\code{qvalue}}{FDR q-value across all population mean tests (i.e. \code{p.adjust(pvalue,method = "fdr")}}
#'  \item{\code{MESOR}}{Midline Estimating Statistic Of Rhythm}
#'  \item{\code{amplitude}}{Population-mean amplitude estimate}
#'  \item{\code{acrophase}}{Population-mean acrophase estimate}
#'  \item{\code{MESOR_l}}{MESOR lower CI}
#'  \item{\code{MESOR_u}}{MESOR upper CI}
#'  \item{\code{amp_l}}{Amplitude lower CI}
#'  \item{\code{amp_u}}{Amplitude upper CI}
#'  \item{\code{acro_l}}{Acrophase lower CI}
#'  \item{\code{acro_u}}{Acrophase upper CI}
#' 
#' @references Cornélissen, G. (2014). Cosinor-Based Rhythmometry. 
#' \emph{Theoretical Biology and Medical Modeling}, \emph{11}, Article 16.
#' @references Bingham, C., Arbogast, B., Guillaume Cornélissen, G., Lee, J.K. &
#'  Halberg, F. (1982). Inferential Statistical Methods for Estimating and Comparing
#'  Cosinor Parameters. \emph{Chronobiologia}, \emph{9(4)}, 397-439.
#' @export
discoPMcos <- function(x,
                       period = 24,
                       idname = NULL, # a row identifier column to include in output
                       alpha = 0.05
) {
  # For single feature inputs, data.frame input can be more convenient
  if (is.data.frame(x)) x <- split(x, 1:nrow(x))

  # duplicate single row inputs for ease
  isdup <- F
  if (nrow(x[[1]]) == 1) {
    isdup <- T
    x <- lapply(x, \(y) rbind(y, y))
  }

  # Construct matrices of each coef
  betas <- sapply(x, \(a) a$sincoef)
  gammas <- sapply(x, \(a) a$coscoef)
  mesors <- sapply(x, \(a) a$mesor)

  ### Estimates
  # All rows must have same N and no missing values
  beta <- rowMeans(betas)
  gamma <- rowMeans(gammas)
  MESOR <- rowMeans(mesors)

  # peak-to-peak
  amplitude <- sqrt(beta^2 + gamma^2)
  acrophase_rad <- atan2(beta, gamma)

  ### Tests and CIs from Bingham et al. 1982
  sdm <- matrixStats::rowSds(mesors)
  sdb <- matrixStats::rowSds(betas)
  sdy <- matrixStats::rowSds(gammas)
  covby <- diag(cov(t(betas), t(gammas)))
  k <- ncol(betas)
  denom <- amplitude^2 * k
  c22 <- (sdb^2 * beta^2 + 2 * covby * beta * gamma + sdy^2 * gamma^2) / denom
  c23 <- ((-1 * (sdb^2 - sdy^2)) * (beta * gamma) + covby * (beta^2 - gamma^2)) / denom
  c33 <- (sdb^2 * gamma^2 - 2 * covby * beta * gamma + sdy^2 * beta^2) / denom

  ### Confidence intervals
  # critical t
  t <- abs(qt(alpha / 2, df = k - 1))
  mesoru <- MESOR + ((t * sdm) / sqrt(k))
  mesorl <- MESOR - ((t * sdm) / sqrt(k))
  ampu <- amplitude + (t * sqrt(c22))
  ampl <- amplitude - (t * sqrt(c22))

  # Acrophase CI calculation is conditional on amp CI
  tmp <- vapply(seq_along(ampu), function(i) {
    if (ampu[i] > 0 & ampl[i] < 0) {
      return(c(NA, NA))
    }
    dn <- amplitude[i]^2 - c22[i] * t^2
    crit <- t * sqrt(c33[i]) * sqrt(amplitude[i]^2 - (c22[i] * c33[i] - c23[i]^2) * (t^2 / c33[i]))
    return(c(
      acrophase_rad[i] + atan((c23[i] * t^2 + crit) / dn),
      acrophase_rad[i] + atan((c23[i] * t^2 - crit) / dn)
    ))
  }, numeric(2))
  fiu <- tmp[1, ]
  fil <- tmp[2, ]

  # Zero amplitude F test
  r <- diag(cor(t(betas), t(gammas)))
  frac1 <- (k * (k - 2)) / (2 * (k - 1))
  frac2 <- 1 / (1 - r^2)
  frac3 <- beta^2 / sdb^2
  frac4 <- (beta * gamma) / (sdb * sdy)
  frac5 <- gamma^2 / sdy^2
  brack <- frac3 - 2 * r * frac4 + frac5
  Fvalue <- frac1 * frac2 * brack
  df2 <- k - 2
  pvalue <- pf(q = Fvalue, df1 = 2, df2 = df2, lower.tail = F)

  ret <- data.frame(
    F = Fvalue,
    df1 = 2,
    df2 = k - 2,
    pvalue,
    qvalue = p.adjust(pvalue, "fdr"),
    MESOR,
    amplitude = amplitude,
    acrophase = (acrophase_rad / 2 / pi * period + period) %% period,
    MESOR_l = mesorl,
    MESOR_u = mesoru,
    amp_l = ampl,
    amp_u = ampu,
    acro_l = (fil / 2 / pi * period + period) %% period,
    acro_u = (fiu / 2 / pi * period + period) %% period
  )

  # Wrap acro around clock to ease visualization/lengths etc.
  idx <- which(ret$acro_l > ret$acro_u)
  ret$acro_l[idx] <- ret$acro_l[idx] - period

  idx <- which((ret$acro_l < ret$acro & ret$acro_u < ret$acro))
  ret$acro_l[idx] <- ret$acro_l[idx] + period
  ret$acro_u[idx] <- ret$acro_u[idx] + period

  idx <- which((ret$acro_l > ret$acro & ret$acro_u > ret$acro))
  ret$acro_l[idx] <- ret$acro_l[idx] - period
  ret$acro_u[idx] <- ret$acro_u[idx] - period

  # Add back rowids from first df if they exist
  if (!is.null(idname)) {
    rownames(ret) <- x[[1]][[idname]]
  } else {
    rownames(ret) <- rownames(x[[1]])
  }

  if (isdup) ret <- ret[1, ]

  return(ret)
}

#' Get R squared for Cosinor fits
#' 
#' Joins together cosinor results (e.g. discoPMcos) and the original input data
#' to calculate the two R squared statistics.
#'
#' @param pmr data.frame, population-mean results from discoPMcos, with roi and measure columns used for joining
#' @param dat data.frame original data used for S-cosinor, with roi and measure columns used for joining
#' 
#' @return the original pmr data.frame with two additional columns
#'  \item{\code{rsq}}{Unnormalized R2 - Referred to as R2un throughout the study}
#'  \item{\code{rsqdm}}{Demeaned R2 - Referred to as R2dm throughout the study}
#'
#' @import dplyr
#' @export
rsqGcos <- function(pmr, dat) {
  predat2 <- pmr |> 
    inner_join(dat)  |> 
    group_by(measure, roi, subject)  |> 
    mutate(dmval = value - mean(value))  |> 
    group_by(measure, roi)

  # Performing the two calculations together has performance benefits
  rsqdf <-
    predat2 |> 
    # Rsqun
    mutate(sr1 = (value - cosinor_pred(time, amplitude, acrophase, MESOR))^2)  |> 
    mutate(ts1 = (value - MESOR)^2)  |> 
    # Subject demeaning for Rsqdm
    mutate(sr2 = (dmval - cosinor_pred(time, amplitude, acrophase, 0))^2) |> 
    mutate(ts2 = (dmval - 0)^2) |> 
    summarize(
      rsq = 1 - sum(sr1) / sum(ts1),
      rsqdm = 1 - sum(sr2) / sum(ts2)
    )
  
  # join rsq back to pmr results
  return(inner_join(pmr, rsqdf))
}

#' Predicted values given the parameters of any cosinor model
#' 
#' @param time numeric, time values to predict
#' @param A numeric, cosinor amplitude
#' @param acro numeric, cosinor acrophase
#' @param mesor numeric, cosinor mesor
#' @param per numeric, cosinor period
#' 
#' @export
cosinor_pred <- function(time, A, acro, mesor = 0, per = 24) {
  mesor + A * cos((time - acro) * 2 * pi / per)
}

# Accessing a DiscoRhythm internal function for convenient cosinor usage
# while ensuring methods are identical for larger scale tests (i.e. those
# run with discoODAs() which also calls on this function.
#' @export
discoCosinor <- DiscoRhythm:::lmCSmat

#' @name cosinor-utils
#' @rdname cosinor-utils
#' 
#' @title Cosinor Utility Functions
#'
#' @description  Applying some useful simple transformations for working with 
#' linear and nonlinear form cosinor parameters.
#'
# Amplitudes are peak-to-peak.
#'
#' @param sin sin beta coefficient from a cosinor model
#' @param cos cos beta coefficient from a cosinor model
#' @param per period of oscillation that was fitted (default is 24)
#' @param amp numeric, amplitude of a cosinor model
#' @param acro numeric, acrophase of a cosinor model
#'
#' @note 
#' The sin/cos in this context refer to 
#' coefficients (often denoted as beta/gamma) in literature (e.g. Cornelissen 2014).
#'
#' @return The amplitude/acrophase of the oscillation or their corresponding linear form coefficients
#'
#' @author Matthew Carlucci
#' 
#' @references Cornélissen, G. (2014). Cosinor-Based Rhythmometry. 
#' \emph{Theoretical Biology and Medical Modeling}, \emph{11}, Article 16.
NULL

#' @describeIn cosinor-utils Get amplitude from betas
#' @export
sincos2amp <- DiscoRhythm:::sincos2amp # borrow from DiscoRhythm
#' @describeIn cosinor-utils Get acrophase from betas 
#' @export
sincos2acro <- DiscoRhythm:::sincos2acr # borrow from DiscoRhythm
#' @describeIn cosinor-utils Get cosinor term from acrophase/amplitude 
#' @export
ampacro2cos <- function(amp, acro, per = 24) amp * cos(acro / per * 2 * pi)
#' @describeIn cosinor-utils Get sin term from acrophase/amplitude 
#' @export
ampacro2sin <- function(amp, acro, per = 24) amp * sin(acro / per * 2 * pi)
#' @describeIn cosinor-utils Set acrophase as complex for circular mean and variance calculations
#' @export
acro2complex <- function(acro, per = 24) {
  z <- complex(real = cos(acro / per * 2 * pi), imaginary = sin(acro / per * 2 * pi))
  return(z)
}

#' Mean of circular quantities
#' 
#' Calculates the mean of
#' circular quantities for 24hr period phases.
#'
#' @param x numeric vector representing time-of-day in hours
#'
#' @return a numeric of time-of-day in hours (0-24)
#'
#' @author Matthew Carlucci
#' @export
circamean <- function(x, ...) {
  ret <-
    acro2complex(x) |>
    mean() |>
    Arg() |>
    {\(x) (x*24/(2*pi))%%24}()
  return(ret)
}

#' Acrophase distance calculation
#'
#' Determine the shortest distance along the unit circle between the two 
#' acrophases (minimum arc-length). 
#'
#' @param acro1 vector of acrophases with period=per
#' @param acro2 vector of acrophases with period=per
#' @param per numeric length 1. Period the acrophase corresponds to.
#'
#' @return shortest distance between pairs of acro1 and acro2
#'
#' @author Matthew Carlucci
#' @export
circdist <- function(acro1, acro2, per = 24) {
  tmp <- (acro1 - acro2) %% per
  idx <- which(tmp > (per / 2))
  tmp[idx] <- tmp[idx] - per
  return(tmp)
}

