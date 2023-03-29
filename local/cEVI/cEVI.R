# Load functions and rest of EVI package #####
remotes::install_github("kpatera/EVI-cEVI",force=T)
require(EVI)
library(cEVIplus)
require(gridExtra)
require(ggplot2)

# source("cEVI/deviant_cEVI.R")
# source("cEVI/evifcut_cEVI.R")
# source("cEVI/indic_cEVI.R")
# source("cEVI/cEVI_fun.R")
# source("cEVI/cEVI_fun.R")
# source("cEVI/evi.graphs.comb.R")

# Load the mot example

# Run cEVI for the first cases of Italy
#tmp_EVI_at=deviant(new_cases = Austria$ncases)
#tmp_cEVI_at=deviant_plus(new_cases = Austria$ncases,lag_max = 40)
# 2 min for Austria 150
# 2 min for Italy 150

#tmp_EVI_ita=deviant(new_cases = Italy$Cases)
#tmp_cEVI_ita=deviant_plus(new_cases = Italy$Cases,lag_max = 40)

  library(readr)
#  Afghanistan2 <- data.frame(read_csv("local/Afghanistan_2022-07-07_file.csv"))
  Colombia2 <- data.frame(read_csv("local/Colombia_2023-03-09_file.csv"))
  India2 <- data.frame(read_csv("local/India_2023-03-09_file.csv"))
  France2 <- data.frame(read_csv("local/France_2023-03-09_file.csv"))
  US2 <- data.frame(read_csv("local/US_2023-03-09_file.csv"))
  SouthAfrica2 <- data.frame(read_csv("local/SouthAfrica_2023-03-09_file.csv"))
#
#  dim(Colombia2)
#  dim(India2)
#  dim(France2)
#  dim(US2)
#  dim(SouthAfrica2)
#

library(RCurl)
#x <- getURL("")
#y <- read.csv(text = x)
gs_27522=read.csv(curl::curl("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"))
head(gs_27522)

#library(readr)
#gs_27522 <- as.data.frame(read_csv("local/globalcases.csv"))
US<-SAfrica<-Colombia<-India<-France<-NULL
SAfrica<-as.vector(t(gs_27522[gs_27522$Country.Region=="South Africa",][5:dim(gs_27522)[2]]))
US<-as.vector(t(gs_27522[gs_27522$Country.Region=="US",][5:dim(gs_27522)[2]]))
Colombia<-as.vector(t(gs_27522[gs_27522$Country.Region=="Colombia",][5:dim(gs_27522)[2]]))
India<-as.vector(t(gs_27522[gs_27522$Country.Region=="India",][5:dim(gs_27522)[2]]))
France<-as.vector(t(gs_27522[gs_27522$Country.Region=="France",][5:dim(gs_27522)[2]][12,]))

N=dim(Colombia2)[1]

### Run EVI cEVI for afghanistan


# COLOMBIA #####
tmp_EVI_co=deviant(new_cases = Colombia,cum = TRUE,method = "EVI")
save(tmp_EVI_co,file = "tmp_EVI_co_new.rdata")
load("tmp_EVI_co.rdata")
#tmp_cEVI_co=deviant(new_cases = Colombia, cum = TRUE, lag_max = 40, method="cEVI")
#save(tmp_cEVI_co, file = "tmp_cEVI_co_new_ttest.rdata")
#load("tmp_cEVI_co_new_ttest.rdata")

Colombia=Colombia[-1]-Colombia
# Farrington
x_temp=temp[[1]]
salmonella.agona$observed<-Colombia
salmonella.agona$state<-rep(0,length(Colombia))
salmonella.agona$freq<-1
salmonella.agona$start<-c(2018,1)
n <- length(salmonella.agona$observed)
control <- list(b=2,w=2,range=(n-900):n,reweight=TRUE, verbose=FALSE,alpha=0.05)
res_far_col <- algo.farrington(salmonella.agona,control=control)
plot(res_far_col)

names(Colombia2)[7:8]<-c("ppv","npv")

N=dim(Colombia2)[1]
graph_co<-evi.graphs.comb(tmp_cEVI_co[1:N,],Colombia2,ln = T,
                          EVI1.lab = "cEVI", EVI2.lab = " EVI", EVI3.lab = "cEVI+",
                          EVI.country = "Colombia")

par(mfrow=c(1,2))
plot(1:N,Colombia2$ppv,type = 'l',lwd=3)
lines(1:N,tmp_cEVI_co$ppv[1:N],type = 'l',lty=3,lwd=3)
plot(1:N,Colombia2$npv,type = 'l',lwd=3)
lines(1:N,tmp_cEVI_co$npv[1:N],type = 'l',lty=3,lwd=3)



# FRANCE #####
#tmp_EVI_fr=deviant(new_cases = France,cum = TRUE)
#save(tmp_EVI_fr,file = "tmp_EVI_fr_new.rdata")
#load("tmp_EVI_fr_new.rdata")
tmp_cEVI_fr=deviant(new_cases = France,lag_max = 40,cum = TRUE, method = "cEVI")
save(tmp_cEVI_fr,file = "tmp_cEVI_fr2_new_ttest.rdata")
load("tmp_cEVI_fr_new_ttest.rdata")
N=dim(France2)[1]

plot(1:dim(France2)[1],France2$pvs,type = 'l',lwd=3,ylab = "France",ylim=c(0,1),xlab="")
lines(1:dim(tmp_cEVI_fr)[1],tmp_cEVI_fr$ppv,type = 'l',lty=3,lwd=3)
lines(1:dim(tmp_cEVI_fr2)[1],tmp_cEVI_fr2$ppv,type = 'l',lty=2,lwd=3, col="red")
plot(1:dim(France2)[1],France2$pvn,type = 'l',lwd=3,ylab = "",ylim=c(0,1),xlab="")
lines(1:dim(tmp_cEVI_fr)[1],tmp_cEVI_fr$npv,type = 'l',lty=3,lwd=3)
lines(1:dim(tmp_cEVI_fr2)[1],tmp_cEVI_fr2$npv,type = 'l',lty=2,lwd=3, col="red")


graph_fr<-EVI.compare(tmp_cEVI_fr[1:N,],France2,lg = F,EVI1.lab = "cEVI", EVI2.lab = "EVI",EVI3.lab = "cEVI+",EVI.country = "France")

par(mfrow=c(1,2))
plot(1:N,France2$pvs,type = 'l',lwd=3,ylim = c(0.5,1))
lines(1:N,tmp_cEVI_fr$ppv[1:N],type = 'l',lty=3,lwd=3)
plot(1:N,France2$pvn,type = 'l',lwd=3,ylim = c(0.5,1))
lines(1:N,tmp_cEVI_fr$npv[1:N],type = 'l',lty=3,lwd=3)


# INDIA ####
#tmp_EVI_in=deviant(new_cases = India, cum = TRUE,method = "EVI")
#save(tmp_EVI_in,file = "tmp_EVI_in_new.rdata")
#load("tmp_EVI_in_new.rdata")
tmp_cEVI_in=deviant(new_cases = India,lag_max = 40, cum=TRUE,method = "cEVI")
save(tmp_cEVI_in,file = "tmp_cEVI_in_new_ttest.rdata")
load("tmp_cEVI_in_new_ttest.rdata")

N=dim(India2)[1]
graph_in<-EVI.compare(EVI1_output = tmp_cEVI_in[1:N,],EVI2_output = India2,lg = F,EVI1.lab = "cEVI", EVI2.lab = "EVI",EVI3.lab = "cEVI+",EVI.country = "India")

par(mfrow=c(1,2))
plot(1:N,India2$ppv,type = 'l',lwd=3)
lines(1:N,tmp_cEVI_in$ppv[1:N],type = 'l',lty=3,lwd=3)
plot(1:N,India2$npv,type = 'l',lwd=3)
lines(1:N,tmp_cEVI_in$npv[1:N],type = 'l',lty=3,lwd=3)

# SAfrica #####
#tmp_EVI_sa=deviant(new_cases = SAfrica,cum = TRUE,method = "EVI")
#save(tmp_EVI_sa,file = "tmp_EVI_sa_new.rdata")
#load("tmp_EVI_sa_new.rdata")
tmp_cEVI_sa=deviant(new_cases = SAfrica,cum = TRUE, lag_max = 40,method = "cEVI")
save(tmp_cEVI_sa,file = "tmp_cEVI_sa_new_ttest.rdata")
load("tmp_cEVI_sa_new_ttest.rdata")

names(SAfrica)[7:8]<-c("ppv","npv")
N=dim(SouthAfrica2)[1]
graph_sa<-EVI.compare(tmp_cEVI_sa[1:N,],SouthAfrica2,ln = T,lg = F,EVI1.lab = "cEVI", EVI2.lab = " EVI", EVI3.lab = "cEVI+",EVI.country = "South Africa")

tmp_EVI_sa$case_def<-c(round(tmp_EVI_sa$EVI[-1]/tmp_EVI_sa$EVI[-length((tmp_EVI_sa$EVI))]-1,3)*100>20,NA)
tmp_cEVI_sa$case_def<-c(round(tmp_cEVI_sa$EVI[-1]/tmp_cEVI_sa$EVI[-length((tmp_cEVI_sa$EVI))]-1,3)*100>20,NA)

par(mfrow=c(1,2))
plot(1:N,SouthAfrica2$pvs,type = 'l',lwd=3)
lines(1:N,tmp_cEVI_sa$ppv,type = 'l',lty=3,lwd=3)
plot(1:N,SouthAfrica2$pvn,type = 'l',lwd=3)
lines(1:N,tmp_cEVI_sa$npv,type = 'l',lty=3,lwd=3)

# US #####

#tmp_EVI_us=deviant(new_cases = US,cum = TRUE,method = "EVI")
#save(tmp_EVI_us,file = "tmp_EVI_us_new.rdata")
#load("tmp_EVI_us_new.rdata")
tmp_cEVI_us=deviant(new_cases = US,cum = TRUE, lag_max = 40,method = "cEVI")
save(tmp_cEVI_us,file = "tmp_cEVI_us_new_ttest.rdata")
load("tmp_cEVI_us_new_ttest.rdata")

names(US)[7:8]<-c("ppv","npv")
N=dim(US2)[1]
graph_us<-EVI.compare(tmp_cEVI_us[1:N,],US2,ln = T,lg = F,size.evi = 1,EVI1.lab = "cEVI", EVI2.lab = " EVI", EVI3.lab = "cEVI+",EVI.country = "United States")

tmp_EVI_us$case_def<-c(round(tmp_EVI_us$EVI[-1]/tmp_EVI_us$EVI[-length((tmp_EVI_us$EVI))]-1,3)*100>20,NA)
tmp_cEVI_us$case_def<-c(round(tmp_cEVI_us$EVI[-1]/tmp_cEVI_us$EVI[-length((tmp_cEVI_us$EVI))]-1,3)*100>20,NA)

par(mfrow=c(1,2))
plot(1:N,US2$pvs,type = 'l',lwd=3,ylim = c(0,1))
lines(1:N,tmp_cEVI_us$ppv,type = 'l',lty=3,lwd=3)
plot(1:N,US2$pvn,type = 'l',lwd=3,ylim = c(0,1))
lines(1:N,tmp_cEVI_us$npv,type = 'l',lty=3,lwd=3)



##### (In) Total Figures #####

gglist<-list(graph_fr,graph_in,graph_sa,graph_us)

pdf("Figure1_col_new.pdf",width = 7,height = 7)
p1<-grid.arrange(graph_fr,graph_in,graph_sa,graph_us)
dev.off()

lim=700
graph_frlim<-evi.compare(tmp_cEVI_fr[1:lim,],France2[1:lim,],lg = F,EVI1.lab = "cEVI", EVI2.lab = "EVI",EVI3.lab = "cEVI-",EVI.country = "France")
graph_inlim<-evi.compare(tmp_cEVI_in[1:lim,],India2[1:lim,],lg = F,EVI1.lab = "cEVI", EVI2.lab = "EVI",EVI3.lab = "cEVI-",EVI.country = "India")
graph_salim<-evi.compare(tmp_cEVI_sa[1:lim,],SouthAfrica2[1:lim,],ln = T,lg = F,EVI1.lab = "cEVI", EVI2.lab = " EVI", EVI3.lab = "cEVI-",EVI.country = "South Africa")
graph_uslim<-evi.compare(tmp_cEVI_us[1:lim,],US2[1:lim,],ln = T,lg = F,size.evi = 1,EVI1.lab = "cEVI", EVI2.lab = " EVI", EVI3.lab = "cEVI-",EVI.country = "United States")

gglist1lim<-list(graph_frlim,graph_inlim)
gglist2lim<-list(graph_salim,graph_uslim)

pdf("Figure1_col_new_lim1.pdf",width = 6,height = 6)
p1<-grid.arrange(grobs = c(gglist1lim), ncol = 1, as.table = FALSE)
dev.off()

pdf("Figure1_col_new_lim2.pdf",width = 6,height = 6)
p1<-grid.arrange(grobs = c(gglist2lim), ncol = 1, as.table = FALSE)
dev.off()


pdf("Figure1_bw.pdf",width = 12,height = 12)
graph_fr<-EVI.compare(tmp_cEVI_fr[1:N,],France2,lg = F,gray = T, EVI1.lab = "cEVI", EVI2.lab = "EVI",EVI3.lab = "cEVI-",EVI.country = "France")
graph_in<-EVI.compare(tmp_cEVI_in[1:N,],India2,lg = F,gray = T, EVI1.lab = "cEVI", EVI2.lab = "EVI",EVI3.lab = "cEVI-",EVI.country = "India")
graph_sa<-EVI.compare(tmp_cEVI_sa[1:N,],SouthAfrica2,lg = F, gray = T, EVI1.lab = "cEVI", EVI2.lab = " EVI", EVI3.lab = "cEVI-",EVI.country = "South Africa")
graph_us<-EVI.compare(tmp_cEVI_us[1:N,],US2,lg = F, gray = T, size.evi = 1,EVI1.lab = "cEVI", EVI2.lab = " EVI", EVI3.lab = "cEVI-",EVI.country = "United States")
grid.arrange(graph_in,graph_fr,graph_sa,graph_us)
dev.off()

pdf("Figure1_colombia.pdf",width = 12,height = 12)
grid.arrange(graph_co,graph_in,graph_fr,graph_us)
dev.off()


graph_fr_raw<-EVI.compare(tmp_cEVI_fr[1:dim(France2)[1],],France2,ln = F,lg = F,EVI1.lab = "cEVI", EVI2.lab = "EVI",EVI3.lab = "cEVI-",EVI.country = "France")
graph_in_raw<-EVI.compare(tmp_cEVI_in[1:dim(India2)[1],],India2,ln = F,lg = F,EVI1.lab = "cEVI", EVI2.lab = "EVI",EVI3.lab = "cEVI-",EVI.country = "India")
graph_co_raw<-EVI.compare(tmp_cEVI_co[1:dim(Colombia2)[1],],Colombia2,ln = F,lg = F,EVI1.lab = "cEVI", EVI2.lab = "EVI",EVI3.lab = "cEVI-",EVI.country = "Colombia")
graph_sa_raw<-EVI.compare(tmp_cEVI_sa[1:dim(SouthAfrica2)[1],],SouthAfrica2,ln = F,lg = F,EVI1.lab = "cEVI", EVI2.lab = "EVI",EVI3.lab = "cEVI-",EVI.country = "South Africa")
graph_us_raw<-EVI.compare(tmp_cEVI_us[1:dim(US2)[1],],US2,ln = F,lg = F,EVI1.lab = "cEVI", EVI2.lab = "EVI",EVI3.lab = "cEVI-",EVI.country = "United States")

pdf("Figure1_Colombia_raw.pdf",width = 12,height = 12)
grid.arrange(graph_co_raw,graph_in_raw,graph_fr_raw,graph_us_raw)
dev.off()

pdf("Figure1_raw.pdf",width = 12,height = 12)
grid.arrange(graph_in_raw,graph_fr_raw,graph_sa_raw,graph_us_raw)
dev.off()


# COLOMBIA with smaller ratio #####

# COLOMBIA
#tmp_EVI_cosmallr=deviant(new_cases = Colombia$Cases[1:300])
#save(tmp_EVI_cosmallr,file = "tmp_EVI_co.rdata")
load("tmp_EVI_cosmallr.rdata")
#tmp_cEVI_cosmallr=deviant_cEVI(new_cases = Colombia$Cases[1:300],lag_max = 40)
#save(tmp_cEVI_cosmallr,file = "tmp_cEVI_co.rdata")
load("tmp_cEVI_cosmallr.rdata")

graph_cosmallr<-evi.graphs.comb(tmp_EVI_cosmallr,tmp_cEVI_cosmallr,EVI1.lab = "cEVI", EVI2.lab = "EVI - Colombia (small r)")

par(mfrow=c(1,2))
plot(1:300,tmp_EVI_cosmallr$ppv,type = 'l',lwd=3)
lines(1:300,tmp_cEVI_cosmallr$ppv,type = 'l',lty=3,lwd=3)
plot(1:300,tmp_EVI_cosmallr$npv,type = 'l',lwd=3)
lines(1:300,tmp_cEVI_cosmallr$npv,type = 'l',lty=3,lwd=3)

# (In) Total plot of NPV PPV #####

pdf("TotalNPVPPV.pdf",height = 15,width = 8)
par(mfrow=c(4,2))
#plot(1:856,tmp_EVI_af$ppv,type = 'l',lwd=3,ylab = "Afghanistan",ylim=c(0,1))
#lines(1:856,tmp_cEVI_af$ppv,type = 'l',lty=3,lwd=3)
#plot(1:856,tmp_EVI_af$npv,type = 'l',lwd=3,ylab = "",ylim=c(0,1))
#lines(1:856,tmp_cEVI_af$npv,type = 'l',lty=3,lwd=3)
#plot(1:856,tmp_EVI_co$ppv,type = 'l',lwd=3,ylab = "Colombia",ylim=c(0,1))
#lines(1:856,tmp_cEVI_co$ppv,type = 'l',lty=3,lwd=3)
#plot(1:856,tmp_EVI_co$npv,type = 'l',lwd=3,ylab = "",ylim=c(0,1))
#lines(1:856,tmp_cEVI_co$npv,type = 'l',lty=3,lwd=3)
plot(1:dim(India2)[1],India2$pvs,type = 'l',lwd=3,ylab="India",ylim=c(0,1),xlab="",main = "Positive Predictive Value (PPV)")
lines(1:dim(tmp_cEVI_in)[1],tmp_cEVI_in$ppv,type = 'l',lty=3,lwd=3)
plot(1:dim(India2)[1],India2$pvn,type = 'l',lwd=3,ylab = "",ylim=c(0,1),xlab="",main = "Negative Predictive Value (NPV)")
lines(1:dim(tmp_cEVI_in)[1],tmp_cEVI_in$npv,type = 'l',lty=3,lwd=3)
plot(1:dim(France2)[1],France2$pvs,type = 'l',lwd=3,ylab = "France",ylim=c(0,1),xlab="")
lines(1:dim(tmp_cEVI_fr)[1],tmp_cEVI_fr$ppv,type = 'l',lty=3,lwd=3)
plot(1:dim(France2)[1],France2$pvn,type = 'l',lwd=3,ylab = "",ylim=c(0,1),xlab="")
lines(1:dim(tmp_cEVI_fr)[1],tmp_cEVI_fr$npv,type = 'l',lty=3,lwd=3)
plot(1:dim(SouthAfrica2)[1],SouthAfrica2$pvs,type = 'l',lwd=3,ylab="South Africa",ylim=c(0,1),xlab="")
lines(1:dim(tmp_cEVI_sa)[1],tmp_cEVI_sa$ppv,type = 'l',lty=3,lwd=3)
plot(1:dim(SouthAfrica2)[1],SouthAfrica2$pvn,type = 'l',lwd=3,ylab = "",ylim=c(0,1),xlab="")
lines(1:dim(tmp_cEVI_sa)[1],tmp_cEVI_sa$npv,type = 'l',lty=3,lwd=3)
plot(1:dim(US2)[1],US2$pvs,type = 'l',lwd=3,ylab = "United States",ylim=c(0,1),xlab="Time point")
lines(1:dim(tmp_cEVI_us)[1],tmp_cEVI_us$ppv,type = 'l',lty=3,lwd=3)
plot(1:dim(US2)[1],US2$pvn,type = 'l',lwd=3,ylab = "",ylim=c(0,1),xlab="Time point")
lines(1:dim(tmp_cEVI_us)[1],tmp_cEVI_us$npv,type = 'l',lty=3,lwd=3)
dev.off()


pdf("TotalNPVPPV_Col.pdf",height = 15,width = 8)
par(mfrow=c(4,2))
plot(1:dim(Colombia2)[1],Colombia2$ppv,type = 'l',lwd=3,ylab = "Colombia",ylim=c(0,1))
lines(1:dim(tmp_cEVI_co)[1],tmp_cEVI_co$ppv,type = 'l',lty=3,lwd=3)
plot(1:dim(Colombia2)[1],Colombia2$npv,type = 'l',lwd=3,ylab = "",ylim=c(0,1))
lines(1:dim(tmp_cEVI_co)[1],tmp_cEVI_co$npv,type = 'l',lty=3,lwd=3)
plot(1:dim(India2)[1],India2$pvs,type = 'l',lwd=3,ylab="India",ylim=c(0,1))
lines(1:dim(tmp_cEVI_in)[1],tmp_cEVI_in$ppv,type = 'l',lty=3,lwd=3)
plot(1:dim(India2)[1],India2$pvn,type = 'l',lwd=3,ylab = "",ylim=c(0,1))
lines(1:dim(tmp_cEVI_in)[1],tmp_cEVI_in$npv,type = 'l',lty=3,lwd=3)
plot(1:dim(France2)[1],France2$pvs,type = 'l',lwd=3,ylab = "France",ylim=c(0,1))
lines(1:dim(tmp_cEVI_fr)[1],tmp_cEVI_fr$ppv,type = 'l',lty=3,lwd=3)
plot(1:dim(France2)[1],France2$pvn,type = 'l',lwd=3,ylab = "",ylim=c(0,1))
lines(1:dim(tmp_cEVI_fr)[1],tmp_cEVI_fr$npv,type = 'l',lty=3,lwd=3)
plot(1:dim(SouthAfrica2)[1],SouthAfrica2$pvs,type = 'l',lwd=3,ylab="South Africa",ylim=c(0,1))
lines(1:dim(tmp_cEVI_sa)[1],tmp_cEVI_sa$ppv,type = 'l',lty=3,lwd=3)
plot(1:dim(SouthAfrica2)[1],SouthAfrica2$pvn,type = 'l',lwd=3,ylab = "",ylim=c(0,1))
lines(1:dim(tmp_cEVI_sa)[1],tmp_cEVI_sa$npv,type = 'l',lty=3,lwd=3)
plot(1:dim(US2)[1],US2$pvs,type = 'l',lwd=3,ylab = "United States",ylim=c(0,1))
lines(1:dim(tmp_cEVI_us)[1],tmp_cEVI_us$ppv,type = 'l',lty=3,lwd=3)
plot(1:dim(US2)[1],US2$pvn,type = 'l',lwd=3,ylab = "",ylim=c(0,1))
lines(1:dim(tmp_cEVI_us)[1],tmp_cEVI_us$npv,type = 'l',lty=3,lwd=3)
dev.off()

# (Out) Old examples ####
tmp_EVI_ita=deviant(new_cases = Italy$Cases)
tmp_cEVI_ita=deviant_cEVI(new_cases = Italy$Cases, lag_max = 40)
>>>>>>> Stashed changes
# 1.60 min for Austria 150
# 1.61 min for Italy 150

# Plot Austria example first cases
pdf("EVI_Austria150.pdf",width=6,height=6)
evi.graphs(tmp_EVI_at,ln = T,type = "l")
dev.off()

pdf("cEVI_Austria150.pdf",width=6,height=6)
evi.graphs(tmp_cEVI_at,ln = T,type = "l")
dev.off()

# Plot Italy example first cases
pdf("EVI_Italy150.pdf",width=6,height=6)
evi.graphs(tmp_EVI_ita,ln = T,type = "l")
dev.off()

pdf("cEVI_Italy150.pdf",width=6,height=6)
evi.graphs(tmp_cEVI_ita,ln = T,type = "l")
dev.off()


# (out) Ex_other example ####
library(readr)
dg <- data.frame(read_csv("~/GitHub/dg.csv"))
tmp_EVI_dg<-deviant(new_cases = dg$total_cases[1:250])
tmp_cEVI_dg<-deviant_plus(new_cases = dg$total_cases[1:250],lag_max = 40)
pdf("ex_other_EVI_250.pdf",width=6,height=6)
evi.graphs(tmp_EVI_dg,ln = T, type="l")
dev.off()

pdf("ex_other_cEVI_250.pdf",width=6,height=6)
evi.graphs(tmp_cEVI_dg,ln = T, type="l")
dev.off()


# (Out) Dungue with updated code and figure. #####

library(readr)
dg <- data.frame(read_csv("~/GitHub/dg.csv"))
#tmp_EVI_dg_all<-deviant(new_cases = dg$total_cases)
#save(tmp_EVI_dg_all,file = "tmp_EVI_dg_all.rdata")
load("tmp_EVI_dg_all.rdata")
#tmp_cEVI_dg_all<-deviant_plus(new_cases = dg$total_cases,lag_max = 50)
#save(tmp_cEVI_dg_all,file = "tmp_cEVI_dg_all.rdata")
load("tmp_cEVI_dg_all.rdata")


pdf("ex_other_all_EVIcEVI_500.pdf",width=6,height=6)
evi.graphs.comb(tmp_EVI_dg_all)
dev.off()



# Fanington algorithm #####

require(surveillance)
data("salmonella.agona")
salmonella.agona$observed<-US2$Cases
salmonella.agona$state<-rep(0,length(US2$Cases))
salmonella.agona$freq<-1
salmonella.agona$start<-c(2020,1)
#Do surveillance for the last 42 weeks
n <- length(salmonella.agona$observed)
control <- list(b=1,w=1,range=(n-42):n,reweight=TRUE, verbose=FALSE,alpha=0.01)
res <- algo.farrington(salmonella.agona,control=control)
plot(res)

#Generate Poisson counts and create an "sts" object
set.seed(123)
x <- rpois(520,lambda=1)
stsObj <- sts(observed=x, frequency=52)


#Compare timing of the two possible fitters for algo.farrington
range <- 312:520
system.time( sts1 <- farrington(stsObj, control=list(range=range,
                                                     fitFun="algo.farrington.fitGLM.fast"), verbose=FALSE))
system.time( sts2 <- farrington(stsObj, control=list(range=range,
                                                     fitFun="algo.farrington.fitGLM"), verbose=FALSE))
#Check if results are the same

# (work) Supplementary table Far cEVI, EVI cEVI+ 1,2 #####

# US out #####
us.fun<-function(rho){
require(surveillance)
data("salmonella.agona")
US<-US[1:1140]
new_cases = c(US[1], diff(US))
new_cases[new_cases<0]<-0
salmonella.agona$observed<-new_cases

salmonella.agona$state<-rep(0,length(new_cases))
salmonella.agona$freq<-1
salmonella.agona$start<-c(2020,1)
#Do surveillance for the last 42 weeks
n <- length(salmonella.agona$observed)
control <- list(b=2,w=1,range=(n-1100):n,reweight=TRUE, verbose=FALSE,alpha=0.2)
res <- algo.farrington(salmonella.agona,control=control)
plot(res)

k=1;bench_us=c(F,F,F,F,F,F,F)
for(i in 7:length(new_cases)){
  bench_us[i]<-mean(new_cases[(i - (7 - 1)):i]) <= 1/(1+rho) * mean(new_cases[(i + 1):(i + 7)],na.rm=T)
}

far<-c(rep(0,39),res$alarm)
far_us=as.numeric(table(bench_us,far))
evi_us=as.numeric(table(bench_us,US2$Index))
lg=length(tmp_cEVI_us$Index)
cevi_us=as.numeric(table(bench_us,tmp_cEVI_us$Index[1:1140]))
ceviplus_us.d=as.numeric(table(bench_us,pmax(tmp_cEVI_us$Index[1:1140],US2$Index[1:1140])))
ceviplus_us.c=as.numeric(table(bench_us,pmin(tmp_cEVI_us$Index[1:1140],US2$Index[1:1140])))

US_out<-as.data.frame(rbind(far_us,evi_us,cevi_us,ceviplus_us.d,ceviplus_us.c))
names(US_out)<-c("FF","FT","TF","TT")
US_out
}
US010<-us.fun(0.10)
US020<-us.fun(0.20)
US040<-us.fun(0.40)

# India out #####
in.fun<-function(rho){
require(surveillance)
data("salmonella.agona")
India<-India[1:1140]
new_cases = c(India[1], diff(India))
new_cases[new_cases<0]<-0
salmonella.agona$observed<-new_cases

salmonella.agona$state<-rep(0,length(new_cases))
salmonella.agona$freq<-1
salmonella.agona$start<-c(2020,1)
#Do surveillance for the last 42 weeks
n <- length(salmonella.agona$observed)
control <- list(b=2,w=1,range=(n-1100):n,reweight=TRUE, verbose=FALSE,alpha=0.1)
res <- algo.farrington(salmonella.agona,control=control)
plot(res)

k=1;bench_in=c(F,F,F,F,F,F,F)
for(i in 7:length(new_cases)){
  bench_in[i]<-mean(new_cases[(i - (7 - 1)):i]) <= 1/(1+rho) * mean(new_cases[(i + 1):(i + 7)],na.rm=T)
}

far<-c(rep(0,38),res$alarm)
far_in=as.numeric(table(bench_in[1:1139],far))
evi_in=as.numeric(table(bench_in[1:1139],India2$Index))
lg=length(tmp_cEVI_in$Index)
cevi_in=as.numeric(table(bench_in[1:1139],tmp_cEVI_in$Index[-(lg:(lg-2))]))
ceviplus_in.d=as.numeric(table(bench_in[1:1139],pmax(tmp_cEVI_in$Index[-(lg:(lg-2))],India2$Index)))
ceviplus_in.c=as.numeric(table(bench_in[1:1139],pmin(tmp_cEVI_in$Index[-(lg:(lg-2))],India2$Index)))

IN_out<-as.data.frame(rbind(far_in,evi_in,cevi_in,ceviplus_in.d,ceviplus_in.c))
names(IN_out)<-c("FF","FT","TF","TT")
IN_out
}
IN010<-in.fun(0.10)
IN020<-in.fun(0.20)
IN040<-in.fun(0.40)

# South Africa #####
sa.fun<-function(rho){
  require(surveillance)
  data("salmonella.agona")
  SAfrica<-SAfrica[1:1140]
  new_cases = c(SAfrica[1], diff(SAfrica))
  new_cases[new_cases<0]<-0
  salmonella.agona$observed<-new_cases

  salmonella.agona$state<-rep(0,length(new_cases))
  salmonella.agona$freq<-1
  salmonella.agona$start<-c(2020,1)
  #Do surveillance for the last 42 weeks
  n <- length(salmonella.agona$observed)
  control <- list(b=2,w=1,range=(n-1100):n,reweight=TRUE, verbose=FALSE,alpha=0.1)
  res <- algo.farrington(salmonella.agona,control=control)
  plot(res)

  k=1;bench_sa=c(F,F,F,F,F,F,F)
  for(i in 7:length(new_cases)){
    bench_sa[i]<-mean(new_cases[(i - (7 - 1)):i]) <= 1/(1+rho) * mean(new_cases[(i + 1):(i + 7)],na.rm=T)
  }

  far<-c(rep(0,38),res$alarm)
  far_sa=as.numeric(table(bench_sa[1:1139],far))
  evi_sa=as.numeric(table(bench_sa[1:1139],SouthAfrica2$Index[1:1139]))
  lg=length(tmp_cEVI_sa$Index)
  cevi_sa=as.numeric(table(bench_sa[1:1139],tmp_cEVI_sa$Index[-(lg:(lg-2))]))
  ceviplus_sa.d=as.numeric(table(bench_sa[1:1139],pmax(tmp_cEVI_sa$Index[-(lg:(lg-2))],SouthAfrica2$Index[1:1139])))
  ceviplus_sa.c=as.numeric(table(bench_sa[1:1139],pmin(tmp_cEVI_sa$Index[-(lg:(lg-2))],SouthAfrica2$Index[1:1139])))

  SA_out<-as.data.frame(rbind(far_sa,evi_sa,cevi_sa,ceviplus_sa.d,ceviplus_sa.c))
  names(SA_out)<-c("FF","FT","TF","TT")
  SA_out
}
SA010<-sa.fun(0.10)
SA020<-sa.fun(0.20)
SA040<-sa.fun(0.40)


# France #####

fr.fun<-function(rho){
  require(surveillance)
  data("salmonella.agona")
  France<-France[1:1140]
  new_cases = c(France[1], diff(France))
  new_cases[new_cases<0]<-0
  salmonella.agona$observed<-new_cases

  salmonella.agona$state<-rep(0,length(new_cases))
  salmonella.agona$freq<-1
  salmonella.agona$start<-c(2020,1)
  #Do surveillance for the last 42 weeks
  n <- length(salmonella.agona$observed)
  control <- list(b=2,w=1,range=(n-1100):n,reweight=TRUE, verbose=FALSE,alpha=0.1)
  res <- algo.farrington(salmonella.agona,control=control)
  plot(res)

  k=1;bench_fr=c(F,F,F,F,F,F,F)
  for(i in 7:length(new_cases)){
    bench_fr[i]<-mean(new_cases[(i - (7 - 1)):i]) <= 1/(1+rho) * mean(new_cases[(i + 1):(i + 7)],na.rm=T)
  }

  far<-c(rep(0,38),res$alarm)
  far_fr=as.numeric(table(bench_fr[1:1139],far))
  evi_fr=as.numeric(table(bench_fr[1:1139],France2$Index[1:1139]))
  lg=length(tmp_cEVI_fr$Index)
  cevi_fr=as.numeric(table(bench_fr[1:1139],tmp_cEVI_fr$Index[-(lg:(lg-2))]))
  ceviplus_fr.d=as.numeric(table(bench_fr[1:1139],pmax(tmp_cEVI_fr$Index[-(lg:(lg-2))],France2$Index[1:1139])))
  ceviplus_fr.c=as.numeric(table(bench_fr[1:1139],pmin(tmp_cEVI_fr$Index[-(lg:(lg-2))],France2$Index[1:1139])))

  FR_out<-as.data.frame(rbind(far_fr,evi_fr,cevi_fr,ceviplus_fr.d,ceviplus_fr.c))
  names(FR_out)<-c("FF","FT","TF","TT")
  FR_out
}
FR010<-fr.fun(0.10)
FR020<-fr.fun(0.20)
FR040<-fr.fun(0.40)



# (IN - Appendix Table A1) Total Out #####

total.out<-rbind(US010,US020,US040,
                 IN010,IN020,IN040,
                 SA010,SA020,SA040,
                 FR010,FR020,FR040)
total.out<-cbind(scenario=rep(rep(c("10","20","40"),each=5),4),total.out)
names(total.out)<-c("Scenario","TN","FP","FN","TP")
total.out<-as.data.frame(total.out)
total.out$NPV<-round(100*total.out$TN/(total.out$FN+total.out$TN),1)
total.out$PPV<-round(100*total.out$TP/(total.out$FP+total.out$TP),1)
total.out

us.fun<-function(rho,N=1140,far){
k=1;bench_us=c(F,F,F,F,F,F,F)
for(i in 7:length(new_cases)){
  bench_us[i]<-mean(new_cases[(i - (7 - 1)):i]) <= 1/(1+rho) * mean(new_cases[(i + 1):(i + 7)],na.rm=T)
}

far_us=as.numeric(table(bench_us[1:N],far[1:N]))
evi_us=as.numeric(table(bench_us[1:N],US2$Index[1:N]))
lg=length(tmp_cEVI_us$Index)
cevi_us=as.numeric(table(bench_us[1:N],tmp_cEVI_us$Index[1:N]))
ceviplus_us.d=as.numeric(table(bench_us[1:N],pmax(tmp_cEVI_us$Index[1:N],US2$Index[1:N])))
ceviplus_us.c=as.numeric(table(bench_us[1:N],pmin(tmp_cEVI_us$Index[1:N],US2$Index[1:N])))

US_out<-as.data.frame(rbind(far_us,evi_us,cevi_us,ceviplus_us.d,ceviplus_us.c))
names(US_out)<-c("FF","FT","TF","TT")
US_out
}
# tab_out<-cbind(bench_us,far,US2$Index,tmp_cEVI_us$Index[1:1140],pmax(tmp_cEVI_us$Index[1:1140],US2$Index),pmin(tmp_cEVI_us$Index[1:1140],US2$Index))
#
#
# tab_out$cumsumbench

require(surveillance)
data("salmonella.agona")
US<-US[1:1140]
new_cases = c(US[1], diff(US))
new_cases[new_cases<0]<-0
salmonella.agona$observed<-new_cases

salmonella.agona$state<-rep(0,length(new_cases))
salmonella.agona$freq<-1
salmonella.agona$start<-c(2020,1)
#Do surveillance for the last 42 weeks
n <- length(salmonella.agona$observed)
control <- list(b=2,w=1,range=(n-1100):n,reweight=TRUE, verbose=FALSE,alpha=0.2)
res <- algo.farrington(salmonella.agona,control=control)
plot(res)
far<-c(rep(0,39),res$alarm)


us_outA<-matrix(NA,nrow = (1140-7),ncol = 4)
us_list<-NULL
for(l in 1:5){
for(i in 1:(1140-7)){
  us_outA[i,]<-as.numeric(us.fun(rho=0.2,N = i+6,far = far)[l,])
  print(i)
}
us_list[[l]]<-us_outA
}
us_list[[1]]<-as.data.frame(us_list[[1]])
us_list[[2]]<-as.data.frame(us_list[[2]])
us_list[[3]]<-as.data.frame(us_list[[3]])
us_list[[4]]<-as.data.frame(us_list[[4]])
us_list[[5]]<-as.data.frame(us_list[[5]])
names(us_list[[1]])<-names(us_list[[2]])<-names(us_list[[3]])<-
  names(us_list[[4]])<-names(us_list[[5]])<-c("FF","FT","TF","TT")

PPVFar=(us_list[[1]]$TT)/(us_list[[1]]$TT+us_list[[1]]$FT)
PPVEVI=(us_list[[2]]$TT)/(us_list[[2]]$TT+us_list[[2]]$FT)
PPVcEVI=(us_list[[3]]$TT)/(us_list[[3]]$TT+us_list[[3]]$FT)
PPVcEVId=(us_list[[4]]$TT)/(us_list[[4]]$TT+us_list[[4]]$FT)
PPVcEVIc=(us_list[[5]]$TT)/(us_list[[5]]$TT+us_list[[5]]$FT)

plot(1:1133,PPVFar,type="l",ylim=c(0,1))
lines(1:1133,PPVEVI,type="l",col="red")
lines(1:1133,PPVcEVI,type="l",col="green")
lines(1:1133,PPVcEVId,type="l",col="blue")
lines(1:1133,PPVcEVIc,type="l",col="purple")
