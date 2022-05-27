#' Combining multiple epidemic volatility indices into a multi-early warning index plot
#'
#' This function returns an evi.graph containing two indices.
#' 1. If index number 1 results in an early warning,then a yellow point will be shown.
#' 2. If index number 2 results in an early warning, then a orange point will be shown.
#' 3. If both indexes result in early warnings, then a red point will be shown.
#'
#' For each time point the stored variables are:
#'@return \itemize{
#' \item{comb.evi.graph: A graph with two EVI indexes shown simulteounsly.}}

#' @param EVI1_output e.g. output of the \code{\link[EVI:deviant]{deviant()}} function
#' @param EVI2_output e.g. output of the \code{\link[EVI:deviant]{deviant_cEVI()}} function
#' @param ln  TRUE or FALSE; If TRUE (the default) the output of the graph will be presented on the logarithmic scale. IF FALSE the output data will be presented on the original scale.
#' @param type By default, points are plotted on EVI graphs. In cases where, changes are very sudden or data sparsely available, type="l" introduces lines on top of points for the "EVI" type of graph.
#'
#' @examples
#'
#' # Run basic functions to acquire early warnings results
#' EVI_output=deviant(new_cases=Italy$Cases, cum=FALSE)
#' cEVI_output=deviant_cEVI(new_cases=Italy$Cases, cum=FALSE)
#'
#' # Plot the EVI combination graph
#' evi.graphs.comb(EVI1_output=EVI_output, EVI2_output=cEVI_output, ln=T)
#' evi.graphs.comb(EVI1_output=EVI_output, EVI2_output=cEVI_output, ln=T, type="l") # For the line EVI plot
#' @export
#'
#' @import ggplot2
#' @import cowplot
#'
#'
#' @references
#' Pateras K, Meletis E, Kostoulas P, The convergence epidemic volatility index an early warning tool for indentifying waves in an epidemic, 2022


evi.graphs.comb <- function(EVI1_output,EVI2_output, ln=T, type="p",size.evi=1,
                            EVI1.lab="EVI1",EVI2.lab="EVI2",EVI3.lab="EVI+",EVI.country=NULL) {

  #EVI_output=temp
  EVI1_output$Index=EVI1_output$Index
  EVI2_output$Index=EVI2_output$Index*2
  EVI_output=EVI1_output
  EVI_output$Index=EVI_output$Index+EVI2_output$Index
  EVI_output$cases_1=EVI_output$Cases*EVI_output$Index
  EVI_output$cases_1[EVI_output$cases_1 == 0] <- NA
  EVI_output$cases_0=EVI_output$Cases*(1-EVI_output$Index)
  EVI_output$cases_0[EVI_output$cases_0 == 0] <- NA

  EVI_output$npv=EVI_output$npv*(1-EVI_output$Index)
  EVI_output$npv[EVI_output$npv == 0] <- NA
  EVI_output$ppv=EVI_output$ppv*EVI_output$Index
  EVI_output$ppv[EVI_output$ppv == 0] <- NA
  EVI_output$variable<-"x"
  EVI_output$Index[is.na(EVI_output$Index)]<-0
  EVI_output$Index<-factor(EVI_output$Index,labels = c("No warning",paste(EVI1.lab,"alone"),paste(EVI2.lab,"alone"),EVI3.lab))
  if (ln==F) {
    sp3<-ggplot(EVI_output, aes_string(x="Days",group="variable"))+
      list(
        geom_point(aes_string(y=("Cases"), color="Index"), size=size.evi),
        scale_color_manual(values=c("grey69", "yellow3", "orange3", "red4")),
        theme(legend.position = "none"),
        labs(title = paste0("Graph combining outputs ",EVI1.lab,", ", EVI2.lab," and ", EVI3.lab," - ",EVI.country), y = "Cases", x="Days"),
        theme(legend.position = "bottom",
              legend.title = element_text(size=10),
              legend.text = element_text(size=8),
              legend.key.height = unit(0, 'cm')),
        if (type=="l")  geom_path(aes_string(y="Cases",colour="Index"),size=size.evi)
      )
  }

  if (ln==T) {
    sp3<-ggplot(EVI_output, aes_string(x="Days",group="variable"))+
      list(
        geom_point(aes_string(y="log(Cases)", color="Index"), size=size.evi),
        scale_color_manual(values=c("grey69", "yellow3", "orange3", "red4")),
        theme(legend.position = "none"),
        labs(title = paste0("Graph combining outputs ",EVI1.lab,", ",EVI2.lab," and ",EVI3.lab," - ",EVI.country), y = "log(Cases)", x="Days"),
        theme(legend.position = "bottom",
              legend.title = element_text(size=10),
              legend.text = element_text(size=8),
              legend.key.height = unit(0, 'cm')),
        if (type=="l")  geom_path(aes_string(y="log(Cases)",colour="Index"), size=size.evi)
      )
  }
  print(sp3)

}
