#' \code{rain} wrapper with amplitude/phase estimations from \code{HarmonicRegression}
#' @description This function runs [rain] and calculates the amplitude and phases using [HarmonicRegression]. Input can be log2 transformed data (\code{logged = TRUE}) and if so, the relative amplitudes will be calculated.
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
#'
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
#' @export
#'
#' @examples
#' ##------------------------------------------------------------------------
#' ## create a circadian dataset with different noise levels
#' ##------------------------------------------------------------------------
#' noise.levels <- c(0, 0.02, 0.05) # range is [0,1]
#' timeStart <- 0
#' timeEnd <- 48
#' time <- timeStart:timeEnd
#' amplitude <- 0.4 # this is relative amplitude
#' baseline <- 1
#' period <- 24
#'
#' ##-----------------------------------------------
#' ## Linear scale
#' ##-----------------------------------------------
#' test_linear <- apply(matrix(noise.levels, nrow = 1), 2, function(noise){
#'   timecourse = baseline + amplitude * cos((time) / period * 2 * pi) + rnorm(timeEnd, 0, noise)
#' })
#' colnames(test_linear) <- paste("Gene", 1:ncol(test_linear), sep = "_")
#' rownames(test_linear) <- paste("Time", 1:nrow(test_linear), sep = "_")
#'
#' ## Plot the time series (linear scale)
#' plot(time, test_linear[,1], xlab = "Time", ylab = "Linear Scale",
#'      sub = paste("Relative amplitude is", amplitude))
#'
#' res_linear <- rainAmp(x = test_linear,
#'                       times = time,
#'                       Period = period,
#'                       Method = "independent",
#'                       pAdjMethod = "BH",
#'                       logged = FALSE, # Note that this is linear scale
#'                       trend = FALSE)
#' res_linear[1,]
#' ## Note that amplitude estimation (hr.amplitude) is ~0.4, which is what we set)
#' ##     Tags         pVal phase peak.shape period     pVal.adj peak trough hr.amplitude hr.phase      hr.pVal      hr.qVal
#' ## 1: Gene_1 1.022744e-45     1         12     24 3.068233e-45    2     14    0.3967611 24.00000 0.000000e+00 0.000000e+00
#'
#' ##-----------------------------------------------
#' ## Log2 cale
#' ##-----------------------------------------------
#' test_log2 <- log2(test_linear)
#'
#' ## Plot the time series (log2 scale)
#' plot(time, test_log2[,1], xlab = "Time", ylab = "Log2 Scale",
#'      sub = paste("Relative amplitude is", amplitude))
#'
#' res_log2 <- rainAmp(x = test_log2,
#'                     times = time,
#'                     Period = period,
#'                     Method = "independent",
#'                     pAdjMethod = "BH",
#'                     logged = TRUE, # Note that this is log2 scale
#'                     trend = FALSE)
#' res_log2[1,]
#' ## Note that amplitude estimation (hr.amplitude) is ~0.4, which is what we set)
#' ##      Tags         pVal phase peak.shape period     pVal.adj peak trough hr.amplitude hr.phase      hr.pVal      hr.qVal
#' ## 1: Gene_1 1.022744e-45     1         12     24 3.068233e-45    2     14    0.3934892       24 8.006058e-46 2.401817e-45
#'
rainAmp <- function(x,
                    times,
                    Period = 24,
                    Method = c("independent", "longitudinal")[2],
                    pAdjMethod = "BH",
                    logged = FALSE,
                    trend = TRUE,
                    trend.pol.degree = 1,
                    threads = NULL, ...){


  # thread management
  # TODO: implement this later if necessary
  if(is.null(threads)){
    mcores <- detectCores()
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
  rainOut <- rainOut %>% setDT

  # adjust p-value
  rainOut <- rainOut[, pVal.adj := p.adjust(pVal, method = pAdjMethod)]

  # calculate peak times
  message("Calculating real peaks")
  rainOut[,peak := phase + DeltaT]
  rainOut[,peak := mod(peak, Period)]
  rainOut[peak == 0, peak := Period]

  # calculate trough times
  message("Calculating real troughs")
  rainOut[,trough := phase + peak.shape + DeltaT]
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
    rainOut$hr.amplitude <- hreg$pars$amp %>% logAmp2relAmp
  }else{
    rainOut$hr.amplitude <- hreg$pars$amp
  }

  rainOut$hr.phase <- hreg$pars$phi %>% rad2hrs
  rainOut$hr.pVal <- hreg$pvals
  rainOut$hr.qVal <- hreg$qvals

  # Merge results
  finalOut <- rainOut
  return(finalOut)
}
