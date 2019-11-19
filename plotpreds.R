rm(list=ls())

library(ggplot2)
library(stringr)

load("fullcasemat.RData")

myformula <- "cenanycases ~ 0 + district + MIRsummarystat + te(lag, doymat, by=anom_var1, bs='tp') + te(lag, doymat, by=anom_var2, bs='tp') + s(doy, bs='tp')"
myformula <- as.formula(myformula)

datelist <- unique(fullcasemat$weekstartdate)
datelist <- datelist[datelist >= as.Date("2016-01-01", "%Y-%m-%d")]

humanminobsweek <- 23
humanmaxobsweek <- 40

minmosqyear <- 2004
maxmosqyear <- 2018

outputdf <- data.frame()
for (curdate in datelist) {
  
  curdate <- as.Date(curdate, origin="1970-01-01")

  fullcasemat$cenanycases <- fullcasemat$anycases
  fullcasemat$cenanycases[fullcasemat$weekstartdate > curdate] <- NA
  
  cl <- makeCluster(detectCores(logical=FALSE)-1)
  firstreg  <- bam(formula=myformula,
                   family=binomial(), data=fullcasemat,
                   subset=modeled==1,
                   cluster=cl)
  stopCluster(cl)
  
  fullcasemat$pred <- predict(firstreg, newdata=fullcasemat, type="response")
  names(fullcasemat)
  
  tempdf <- fullcasemat[c("weekstartdate", "district", "anycases", "pred")]
  tempdf$cendate <- curdate
  outputdf <- bind_rows(outputdf, tempdf)
  
}
write.csv(x=outputdf, file="outputdf.csv")
save(outputdf, "outputdf.RData")