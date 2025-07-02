#' \code{rain} wrapper with amplitude/phase estimations from \code{HarmonicRegression}
#' @description This function runs [rain] and calculates the amplitude and phases using \code{HarmonicRegression}. Input can be log2 transformed data (\code{logged = TRUE}) and if so, the relative amplitudes will be calculated.
#'
#' @param x Numeric matrix. Rows = times, Columns = Gene names. See [rain] for details. Rows (times) should be sorted
#' @param times Numeric vector for times. Length should be equal to the row length of \code{x}
#' @param Period Period of [harmonic.regression]. Default = 24 (hours).
#' @param Method Can be either \code{independent} or \code{longitudinal}. See [rain] for details.
#' @param pAdjMethod P-value adjustment method. Default is \code{BH}. See [p.adjust] for details.
#' @param logged Logical. Is input matrix, \code{x}, log2 transformed?
#' @param trend Logical. Eliminate trend? See [harmonic.regression]'s \code{trend.eliminate}
#' @param trend.pol.degree Degree of polynomial. Numeric. If \code{trend = TRUE}, a polynomial parameter will be added to harmonic regression. See [harmonic.regression]'s \code{trend.degree}
#' @param threads Number of threads for parallel processing. Experimental at the moment.
#' @param ... Further parameters can be passed to [rain], [harmonic.regression]
#'
#' @return Table with the circadians results
#' * Tags: Gene names (corresponds to the column names of input matrix \code{x})
#' * pVal: p-value calculated by [rain]
#' * phase: phase calculated by [rain]
#' * peak.shape: peak.shape calculated by [rain]
#' * period: period of the rhythmicity
#' * pVal.adj: [rain] p-value adjusted for multiple testing
#' * peak: peak time calculated from [rain]
#' * trough: trough time calculated by [rain]
#' * hr.amplitude: relative amplitude calculated by [harmonic.regression]
#' * hr.phase: phase (peak time) calculated by [harmonic.regression]
#' * hr.pVal: p-value calculated by [harmonic.regression]
#' * hr.qVal: q-value calculated by [harmonic.regression]
#'
#'
#' @import rain
#' @import HarmonicRegression
#' @importFrom parallel detectCores
#' @importFrom data.table data.table setDT
#' @export
#' @seealso [logAmp2relAmp()]
#' @examples
#' ##------------------------------------------------------------------------
#' ## create a circadian dataset with different noise levels
#' ##------------------------------------------------------------------------
#' noise_levels <- c(0, 0.02, 0.05) # range is [0,1]
#' timeStart <- 0
#' timeEnd <- 48
#' time <- seq(timeStart, timeEnd, by = 2)
#' mesor <- 1 # baseline
#' relative_amplitude <- 0.4
#' amplitude <- mesor * relative_amplitude
#' period <- 24
#'
#' ##-----------------------------------------------
#' ## Linear scale
#' ##-----------------------------------------------
#' test_linear <- apply(matrix(noise_levels, nrow = 1), 2, function(noise){
#'   timecourse = mesor + amplitude * cos((time) / period * 2 * pi) + rnorm(length(time), 0, noise)
#' })
#' colnames(test_linear) <- paste("Gene", 1:ncol(test_linear), sep = "_")
#' rownames(test_linear) <- paste("Time", 1:nrow(test_linear), sep = "_")
#'
#'
#' ## Plot the time series (linear scale)
#' plot(time, test_linear[,1], xlab = "Time", ylab = "Linear Scale",
#'      sub = paste("Relative amplitude is", relative_amplitude))
#'
#' res_linear <- rainAmp(x = test_linear,
#'                       times = time,
#'                       Period = period,
#'                       Method = "independent",
#'                       pAdjMethod = "BH",
#'                       logged = FALSE, # Note that this is linear scale
#'                       trend = FALSE)
#'
#' ## Note that amplitude estimation (hr.amplitude) is ~0.4, which is what we set)
#' res_linear[1,1:10]
#'
#' ##-----------------------------------------------
#' ## Log2 scale
#' ##-----------------------------------------------
#' test_log2 <- log2(test_linear)
#'
#' ## Plot the time series (log2 scale)
#' plot(time, test_log2[,1], xlab = "Time", ylab = "Log2 Scale",
#'      sub = paste("Relative amplitude is", relative_amplitude))
#'
#' res_log2 <- rainAmp(x = test_log2,
#'                     times = time,
#'                     Period = period,
#'                     Method = "independent",
#'                     pAdjMethod = "BH",
#'                     logged = TRUE, # Note that this is log2 scale
#'                     trend = FALSE)
#'
#' ## Note that amplitude estimation (hr.amplitude) is ~0.4, which is what we set)
#' res_log2[1,1:10]
rainAmp <- function(x,
                    times,
                    Period = 24,
                    Method = c("independent", "longitudinal")[2],
                    pAdjMethod = "BH",
                    logged = FALSE,
                    trend = TRUE,
                    trend.pol.degree = 1,
                    threads = NULL, ...){

  ##------------------------------------------------------------------------
  ## Fix global variables
  ##------------------------------------------------------------------------
  detectCores <- pVal.adj <- pVal <- peak <- phase <- trough <- peak.shape <- `:=` <- NULL

  ##------------------------------------------------------------------------
  ## Function
  ##------------------------------------------------------------------------
  # thread management
  # TODO: implement this later if necessary
  if(is.null(threads)){
    mcores <- parallel::detectCores()
  } else{
    mcores <- threads
  }

  # calculate DeltaT
  ut <- unique(times)
  lu <- length(ut)
  DeltaT <- ut[2:lu] - ut[1:lu-1]
  DeltaT <- unique(DeltaT)
  if(length(DeltaT) > 1){
    stop("deltaT must be a unique")
  }

  if(any(is.na(x))){
    removeNA <- TRUE
    message("Detected NAs in the input. Switching to na.rm=TRUE in rain(). The analysis may take a while")
  } else {
    removeNA <- FALSE
  }

  # run rain
  rainOut <- rain(x,
                  period = Period,
                  measure.sequence = table(times),
                  deltat = DeltaT,
                  method = Method,
                  na.rm = removeNA,
                  verbose=T)

  # make data.table
  rainOut <- data.frame(Tags = rownames(rainOut), rainOut)
  rainOut <- data.table::setDT(rainOut)

  # adjust p-value
  rainOut <- rainOut[, pVal.adj := p.adjust(pVal, method = pAdjMethod)]

  # calculate peak times
  message("Calculating real peaks")
  rainOut[,peak := ut[phase/DeltaT]]
  rainOut[,peak := mod(peak, Period)]
  rainOut[peak == 0, peak := Period]

  # calculate trough times
  message("Calculating real troughs")
  rainOut[,trough := peak + peak.shape]
  rainOut[,trough := mod(trough, Period)]
  rainOut[trough == 0, trough := Period]

  # calculate amplitudes
  message("Calculating amplitudes")
  # #options(digits=9) #due to zapsmall in harmonic.regression while normalizing the waves (default = 7)
  # if(logged){
  #   x.hr <- scale(x, center = TRUE, scale = FALSE)
  # } else{
  #   x.hr <- x
  # }
  x.hr <- x
  hreg <- harmonic.regression(inputts = x.hr,
                              inputtime = times,
                              Tau = Period,
                              normalize = !logged,
                              norm.pol = F,
                              norm.pol.degree = 1,
                              trend.eliminate = trend,
                              trend.degree = trend.pol.degree)
  if(logged){
    rainOut$hr.amplitude <- logAmp2relAmp(hreg$pars$amp)
  }else{
    rainOut$hr.amplitude <- hreg$pars$amp
  }

  rainOut$hr.phase <- rad2hrs(hreg$pars$phi)
  rainOut$hr.pVal <- hreg$pvals
  rainOut$hr.qVal <- hreg$qvals

  # Merge results
  finalOut <- rainOut
  return(finalOut)
}
