# Load functions and rest of EVI package
remotes::install_github("kpatera/EVI-cEVI",force=T)
#require(EVI)
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
 Afghanistan2 <- data.frame(read_csv("Afghanistan_2022-03-23_file.csv"))
 Colombia2 <- data.frame(read_csv("Colombia_2022-03-23_file.csv"))
 India2 <- data.frame(read_csv("India_2022-03-23_file.csv"))
 France2 <- data.frame(read_csv("France_2022-03-23_file.csv"))
 dim(Afghanistan2)
 dim(Colombia2)
 dim(India2)
 dim(France2)

library(readr)
gs_27522 <- as.data.frame(read_csv("globalcases.csv"))
Afghanistan<-Colombia<-India<-France<-NULL
Afghanistan<-as.vector(t(gs_27522[gs_27522$`Country/Region`=="Afghanistan",][5:dim(gs_27522)[2]]))
Colombia<-as.vector(t(gs_27522[gs_27522$`Country/Region`=="Colombia",][5:dim(gs_27522)[2]]))
India<-as.vector(t(gs_27522[gs_27522$`Country/Region`=="India",][5:dim(gs_27522)[2]]))
France<-as.vector(t(gs_27522[gs_27522$`Country/Region`=="France",][5:dim(gs_27522)[2]][12,]))
### Run EVI cEVI for afghanistan


# AFGHANISTAN
tmp_EVI_af=deviant(new_cases = Afghanistan[1:170],cum = TRUE,method = "EVI")
save(tmp_EVI_af,file = "tmp_EVI_af_new.rdata")
load("tmp_EVI_af_new.rdata")
tmp_cEVI_af=deviant(new_cases = Afghanistan[1:170],lag_max = 40, cum=TRUE,method = "cEVI")
save(tmp_cEVI_af,file = "tmp_cEVI_af_new.rdata")
load("tmp_cEVI_af.rdata")

names(Afghanistan2)[7:8]<-c("ppv","npv")
graph_af<-evi.graphs.comb(tmp_cEVI_af,Afghanistan2,ln = T,EVI1.lab = "cEVI", EVI2.lab = " EVI", EVI3.lab = "cEVI+",EVI.country = "Afghanistan")

tmp_EVI_af$case_def<-c(round(tmp_EVI_af$EVI[-1]/tmp_EVI_af$EVI[-length((tmp_EVI_af$EVI))]-1,3)*100>20,NA)
tmp_cEVI_af$case_def<-c(round(tmp_cEVI_af$EVI[-1]/tmp_cEVI_af$EVI[-length((tmp_cEVI_af$EVI))]-1,3)*100>20,NA)

par(mfrow=c(1,2))
plot(1:789,tmp_EVI_af$ppv,type = 'l',lwd=3)
lines(1:789,tmp_cEVI_af$ppv,type = 'l',lty=3,lwd=3)
plot(1:789,tmp_EVI_af$npv,type = 'l',lwd=3)
lines(1:789,tmp_cEVI_af$npv,type = 'l',lty=3,lwd=3)

# COLOMBIA
tmp_EVI_co=deviant(new_cases = Colombia[1:170],cum = TRUE,method = "EVI")
#save(tmp_EVI_co,file = "tmp_EVI_co.rdata")
load("tmp_EVI_co.rdata")
tmp_cEVI_co=deviant(new_cases = Colombia[1:170],lag_max = 40,method="cEVI")
#save(tmp_cEVI_co,file = "tmp_cEVI_co.rdata")
load("tmp_cEVI_co.rdata")

names(Colombia2)[7:8]<-c("ppv","npv")


graph_co<-evi.graphs.comb(tmp_cEVI_co,Colombia2,ln = T, EVI1.lab = "cEVI", EVI2.lab = " EVI", EVI3.lab = "cEVI+",EVI.country = "Colombia")

par(mfrow=c(1,2))
plot(1:789,tmp_EVI_co$ppv,type = 'l',lwd=3)
lines(1:789,tmp_cEVI_co$ppv,type = 'l',lty=3,lwd=3)
plot(1:789,tmp_EVI_co$npv,type = 'l',lwd=3)
lines(1:789,tmp_cEVI_co$npv,type = 'l',lty=3,lwd=3)



# INDIA
tmp_EVI_in=deviant(new_cases = India[1:150],cum = TRUE)
#save(tmp_EVI_in,file = "tmp_EVI_in.rdata")
load("tmp_EVI_in.rdata")
#tmp_cEVI_in=deviant_cEVI(new_cases = India$Cases,lag_max = 40)
#save(tmp_cEVI_in,file = "tmp_cEVI_in.rdata")
load("tmp_cEVI_in.rdata")

graph_in<-evi.graphs.comb(tmp_cEVI_in,tmp_EVI_in,EVI1.lab = "cEVI", EVI2.lab = "EVI",EVI3.lab = "cEVI+",EVI.country = "India")

par(mfrow=c(1,2))
plot(1:790,tmp_EVI_in$ppv,type = 'l',lwd=3)
lines(1:790,tmp_cEVI_in$ppv,type = 'l',lty=3,lwd=3)
plot(1:790,tmp_EVI_in$npv,type = 'l',lwd=3)
lines(1:790,tmp_cEVI_in$npv,type = 'l',lty=3,lwd=3)



# FRANCE
#tmp_EVI_fr=deviant(new_cases = France$Cases)
#save(tmp_EVI_fr,file = "tmp_EVI_fr.rdata")
load("tmp_EVI_fr.rdata")
tmp_cEVI_fr=deviant(new_cases = France$Cases[1:70],lag_max = 40,method = "cEVI")
#save(tmp_cEVI_fr,file = "tmp_cEVI_fr.rdata")
load("tmp_cEVI_fr.rdata")
graph_fr<-evi.graphs.comb(tmp_cEVI_fr,tmp_EVI_fr,EVI1.lab = "cEVI", EVI2.lab = "EVI",EVI3.lab = "cEVI+",EVI.country = "France")

par(mfrow=c(1,2))
plot(1:789,tmp_EVI_fr$ppv,type = 'l',lwd=3)
lines(1:789,tmp_cEVI_fr$ppv,type = 'l',lty=3,lwd=3)
plot(1:789,tmp_EVI_fr$npv,type = 'l',lwd=3)
lines(1:789,tmp_cEVI_fr$npv,type = 'l',lty=3,lwd=3)

pdf("Figure1.pdf",width = 12,height = 12)
grid.arrange(graph_af,graph_co,graph_in,graph_fr)
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

# (In) Total plot of NPV PPV

pdf("TotalNPVPPV.pdf",height = 15,width = 8)
par(mfrow=c(4,2))
plot(1:789,tmp_EVI_co$ppv,type = 'l',lwd=3)
lines(1:789,tmp_cEVI_co$ppv,type = 'l',lty=3,lwd=3)
plot(1:789,tmp_EVI_co$npv,type = 'l',lwd=3)
lines(1:789,tmp_cEVI_co$npv,type = 'l',lty=3,lwd=3)
plot(1:790,tmp_EVI_in$ppv,type = 'l',lwd=3)
lines(1:790,tmp_cEVI_in$ppv,type = 'l',lty=3,lwd=3)
plot(1:790,tmp_EVI_in$npv,type = 'l',lwd=3)
lines(1:790,tmp_cEVI_in$npv,type = 'l',lty=3,lwd=3)
plot(1:789,tmp_EVI_fr$ppv,type = 'l',lwd=3)
lines(1:789,tmp_cEVI_fr$ppv,type = 'l',lty=3,lwd=3)
plot(1:789,tmp_EVI_fr$npv,type = 'l',lwd=3)
lines(1:789,tmp_cEVI_fr$npv,type = 'l',lty=3,lwd=3)
plot(1:789,tmp_EVI_co$ppv,type = 'l',lwd=3)
lines(1:789,tmp_cEVI_co$ppv,type = 'l',lty=3,lwd=3)
plot(1:789,tmp_EVI_co$npv,type = 'l',lwd=3)
lines(1:789,tmp_cEVI_co$npv,type = 'l',lty=3,lwd=3)
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


# (In) Dungue with updated code and figure. #####

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

