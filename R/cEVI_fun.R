#' Calculation of the convergence Epidemic Volatility Index
#'
#' This function produces the convergence Epidemic Volatility Index based on input data.
#'
#' @param cases the time series of the newly observed cases per unit of time (ideally per day).
#' @param lag_n Integer. Restriction of the window size for the rolling window size.
#' @param c_n threshold alpha-level value (0 <= c <= 0.5) for issuing an early warning. If cevi <= c_n an early warning is issued and otherwise is not.
#'
#' For each time point the stored variables are:
#' @return \itemize{
#' \item{Dates: the date for each time point (with origin 01-01-1970).}
#'
#' \item{Days: the serial number for each time point.}
#'
#' \item{EVI: the estimated EVI for each time point.}}
#'
#' @references
#' Pateras K, Meletis E, Kostoulas P, The convergence epidemic volatility index an early warning tool for indentifying waves in an epidemic, 2022

cEVI_fun<-function(cases,lag_n,c_n){

  cevi <- rep(NA, length(cases))
  for(k in (lag_n-1):(length(cases)-(lag_n+1))){
    enu=mean(cases[(k+2):(k+lag_n+1)]-cases[(k+1):(k-(lag_n-2))],na.rm = T)
    den1=sd(cases[(k+1):(k-(lag_n-2))])^2/(length(cases[(k+1):(k-(lag_n-2))]))
    den2=sd(cases[(k+2):(k+lag_n+1)])^2/(length(cases[(k):(k+lag_n+1)]))

    # Spectral variances more appropriate but more time consuming
    #den1=spectrum0.ar(cases[(i+1):(i+w_s)])$spec/(length(cases[(i+1):(i+w_s)])) # Spectral variances
    #den2=spectrum0.ar(cases[(i):(i-(w_s-1))])$spec/(length(cases[(i):(i-(w_s-1))]))
    test=enu/sqrt(den1+den2)
    cevi[k+lag_n+1]=as.numeric((1-pnorm(test))<=c_n)#*as.numeric(evi[i] >= rate)
  }
  return(cevi)
}

