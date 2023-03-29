#' This function produces the Epidemic Volatility Index based output data (beta)
#'
#' This is a secondary function of the \code{\link[EVI:EVI-package]{EVI-package}} that you should use to analyze a time series of observed cases per unit of time (ideally per day). This function is based on ongoing work and should be used with caution.
#'
#' For each time point the stored variables are:
#'@return \itemize{
#' \item{Dates: the date for each time point (with origin 01-01-1970).}
#'
#'\item{Days: the serial number for each time point.}
#'
#'\item{EVI: the estimated EVI for each time point.}
#'
#'\item{Cases: the rolling average of the newly observed cases for each time point. A 7-day rolling average is calculated by default (i.e., r_a=7). The user has the option to change this by modifying r_a.}
#'
#'\item{Index: takes values 1 or 0 for the issuance of an early warning or not, respectively.}
#'
#'\item{ppv: the positive predictive value for each time point.}
#'
#'\item{npv: the negative predictive value for each time point.}
#'
#'\item{lag_all: the selected rolling window size for EVI calculation for each time point.}
#'
#'\item{c_all: the selected cut-off for issuing an early warning for each time point.}
#'
#'\item{se_all: the sensitivity (Se) of EVI up to this time point.}
#'
#'\item{sp_all: the specificity (Sp) of EVI up to this time point.}}



#' @param new_cases the time series of the newly observed cases per unit of time (ideally per day).
#' @param cum TRUE or FALSE; TRUE if the time series is recorded as the cumulative number of the reported cases and FALSE (the default) if newly reported cases per unit of time are recorded.
#' @param r_a The window size for the moving average that will be analyzed. If set to 1 the actual observations are analyzed. However, due to the variability of the reported cases between working days and weekends it is recommended that the 7-day moving average is analyzed (i.e. r_a = 7), which is the default for this argument. Users could prefer a longer interval of 14 days or one month (i.e., r_a=14 or 30, respectively).
#' @param r Definition for the minimum difference in the mean number of cases, one week before and after each time point that, if present, should be detected. This is the case definition and the default is 0.2 (with 0 <= r <= 1). A value of r=0.2 means that we have a case when the mean number of the newly observed cases in the next 7 days is at least 20% higher than the mean number of the newly observed cases in the past 7 days.
#' @param lag_max Integer. Restriction of the maximum window size for the rolling window size. The default is set to one month (lag_max=30) to prevent excess volatility of past epidemic waves from affecting the most recent volatility estimates and the ability of EVI to warn for upcoming waves that may be smaller and of lower volatility than previous ones.
#' @param method either "EVI" or "cEVI", default equals to "EVI".
#'
#' @examples
#' data("Italy")
#' deviant(new_cases=Italy$Cases, cum=FALSE, r_a=7, r=0.2, lag_max=30,method="EVI")
#' #This step should take some time and the time elapsed will be printed
#'
#' @importFrom stats sd
#'
#' @export
#'
#' @references
#' Pateras K., Meletis, E., Denwood M., et al. The convergence epidemic index (cEVI) an early warning tool for identifying waves in an epidemic. Inf Dis Mod, (2023)
#' Kostoulas, P., Meletis, E., Pateras, K. et al. The epidemic volatility index, a novel early warning tool for identifying new waves in an epidemic. Sci Rep 11, 23775 (2021). \doi{10.1038/s41598-021-02622-3}
#'
deviant=function(new_cases, cum = FALSE, r_a=7, r=0.2, lag_max=40, method="EVI"){
  if(method=="EVI"){
    start_time = Sys.time()
    start_cases = 14
    lag_1 = 7
    c_1 = 0.01
    w_s = 7
    if (cum == TRUE)
      new_cases = c(new_cases[1], diff(new_cases))
    cases = mova(new_cases, r_a)
    roll = rollsd(cases[1:start_cases], lag_1)
    ev = evi(roll)
    ind = indic(evi = ev,cut =  c_1, cases = cases[1:start_cases],method="EVI")
    status = status(cases[1:start_cases], r)
    ppv = rep(NA, length(cases))
    npv = rep(NA, length(cases))
    lag_all = rep(NA, start_cases)
    c_all = rep(NA, start_cases)
    se_all = rep(NA, start_cases)
    sp_all = rep(NA, start_cases)
    lag_all[1:start_cases] = lag_1
    c_all[1:start_cases] = c_1
    for (i in (start_cases + 1):length(cases)) {
      case_t = cases[1:i]
      lag_s = seq(lag_1, min(lag_max, (length(case_t) - 1)),
                  1)
      c_s = seq(0.01, 0.5, 0.01)
      all_lag = NA
      all_cut = NA
      all_se = NA
      all_sp = NA
      for (j in lag_s) {
        roll_t = rollsd(case_t, j)
        ev_t = evi(roll_t)
        for (l in c_s) {
          evicut_t = evifcut(evi = ev_t, cases = case_t, cut = l, r = r,method="EVI")
          new_j = j
          new_l = l
          new_se = evicut_t$sens
          new_sp = evicut_t$spec
          all_lag[[length(all_lag) + 1]] <- new_j
          all_cut[[length(all_cut) + 1]] <- new_l
          all_se[[length(all_se) + 1]] <- new_se
          all_sp[[length(all_sp) + 1]] <- new_sp
        }
      }
      sesp = as.data.frame(cbind(all_lag, all_cut, all_se,
                                 all_sp))
      index = which.max(sesp$all_se + sesp$all_sp - 1)
      print(i)
      print(sesp[index, ])
      lag_n = sesp$all_lag[index]
      c_n = sesp$all_cut[index]
      roll_n = rollsd(cases[1:i], lag_n)
      ev_n = evi(roll_n)
      ind_n = indic(evi = ev_n, cut = c_n, cases = case_t,method="EVI")
      evicut_n = evifcut(evi = ev_n, cases = case_t, cut = c_n, r = r,method="EVI")
      roll = c(roll, roll_n[i])
      ev = c(ev, ev_n[i])
      ind = c(ind, ind_n[i])
      lag_all = c(lag_all, lag_n)
      c_all = c(c_all, c_n)
      se_all = c(se_all, all_se[index])
      sp_all = c(sp_all, all_sp[index])
      ppv[i] = evicut_n$prev * all_se[index]/(evicut_n$prev *
                                                all_se[index] + (1 - evicut_n$prev) * (1 - all_sp[index]))
      npv[i] = (1 - evicut_n$prev) * all_sp[index]/((1 - evicut_n$prev) *
                                                      all_sp[index] + evicut_n$prev * (1 - all_se[index]))
    }
    Days = (1:length(cases))
    EVI = ev
    Cases = cases
    Index = ind
  }

  if(method=="cEVI"){
    start_time = Sys.time()
    start_cases=18
    lag_1=3
    c_1=0.001
    w_s=7
    if (cum == TRUE){
      new_cases = c(new_cases[1], diff(new_cases))
    }
    cases=mova(new_cases, r_a)
    #cases=new_cases
    cevi=cEVI_fun(cases = cases[1:(start_cases)],lag_n = lag_1, c_n = c_1)
    ind=indic(cevi=cevi, cases=cases[1:start_cases], method="cEVI")
    status=status(cases[1:start_cases],r)

    #initiate chain for positive predictive value
    ppv=rep(NA, length(cases))
    #initiate chain for negative predictive value
    npv=rep(NA, length(cases))

    lag_all=rep(NA, start_cases)
    c_all=rep(NA, start_cases)
    se_all=rep(NA, start_cases)
    sp_all=rep(NA, start_cases)
    lag_all[1:start_cases]=lag_1
    c_all[1:start_cases]=c_1

    for (i in (start_cases+1): length(cases)){

      case_t=cases[1:i]
      lag_s=seq(lag_1,min(lag_max,(i-i/2-4)), 2)
      c_s=seq(0.001,0.5, 0.06)
      all_lag=NA
      all_cut=NA
      all_se=NA
      all_sp=NA

      for (l in c_s) {
        for (j in lag_s) {
          # roll_t <- rollsd(case_t,j)
          #  ev_t <- evi(roll_t)
          cevi <- rep(NA, length(case_t))
          for(k in (j+1):(length(case_t)-(j+1))){
            enu=mean(case_t[(k+2):(k+j+1)]-case_t[(k):(k-(j-1))],na.rm = T)
            den1=sd(case_t[(k):(k-(j-1))])^2/(length(case_t[(k):(k-(j-1))]))
            den2=sd(case_t[(k+2):(k+j+1)])^2/(length(case_t[(k+2):(k+j+1)]))
            # Spectral variances more appropriate but more time consuming
            #den1=spectrum0.ar(case_t[(i+1):(i+w_s)])$spec/(length(case_t[(i+1):(i+w_s)])) # Spectral variances
            #den2=spectrum0.ar(case_t[(i):(i-(w_s-1))])$spec/(length(case_t[(i):(i-(w_s-1))]))
            teststat=enu/sqrt(den1+den2)

            #cevi[k+j+1]=as.numeric((1-pt(q = test,df = j))<=l)#*as.numeric(evi[i] >= rate)
            #cevi[k+j+1]=as.numeric((1-pnorm(test))<=l)#*as.numeric(evi[i] >= rate)
            #cevi[k+j+1]=as.numeric((1-pnorm(test))<=l)#*as.numeric(evi[i] >= rate)
            Nn=length((k+1):(k+j))
           # if(test=="ztest"){
            #  cevi[k+j+1]<<-as.numeric((1-pnorm(q = teststat))<=l)#*as.numeric(evi[i] >= rate)
            #}
            #if(test=="ttest"){

              cevi[k+j+1]<<-as.numeric((1-pt(q = test,df = Nn))<=l)
              #}

            }
          evicut_t <- evifcut(cevi=cevi,cases = case_t, r = r,method = "cEVI")
          all_lag[[length(all_lag) + 1]] <- j
          all_cut[[length(all_cut) + 1]] <- l
          all_se[[length(all_se) + 1]] <- evicut_t[[1]]
          all_sp[[length(all_sp) + 1]] <- evicut_t[[2]]
        }
      }

      sesp=as.data.frame(cbind(all_lag,all_cut,all_se,all_sp))
      index=which.max(sesp$all_se+sesp$all_sp-1)

      print(i)
      print(sesp[index,])

      lag_n=sesp$all_lag[index]
      c_n=sesp$all_cut[index]

      cevi=cEVI_fun(cases = cases[1:i],lag_n = lag_n, c_n = c_n,test = test1) #
      ind_n=indic(cevi = cevi, cases = case_t, method="cEVI") #
      evicut_n=evifcut(cevi = cevi, cases = case_t, r = r, method="cEVI") #

      ind=c(ind, ind_n[i])
      lag_all=c(lag_all,lag_n)
      c_all=c(c_all,c_n)

      se_all=c(se_all,all_se[index])
      sp_all=c(sp_all,all_sp[index])

      ppv[i]=evicut_n$prev*all_se[index]/
        (evicut_n$prev*all_se[index]+(1-evicut_n$prev)*(1-all_sp[index]))
      npv[i]=(1-evicut_n$prev)*all_sp[index]/
        ((1-evicut_n$prev)*all_sp[index]+evicut_n$prev*(1-all_se[index]))
    }

    Days=(1:length(cases))
    EVI=cevi
    Cases=cases
    Index=ind
  }

  EVI_out = as.data.frame(cbind(Days, EVI, Cases, Index, ppv,
                                npv, lag_all, c_all, se_all, sp_all, new_cases))
  EVI_output = EVI_out
  EVI_output <<- (EVI_output)
  end_time = Sys.time()
  time_elapsed = end_time - start_time
  print(time_elapsed)
  return(EVI_output)

}
