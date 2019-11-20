rm(list=ls())

library(dplyr)
library(ggplot2)
library(stringr)

# load("fullcasemat.RData")
# 
# myformula <- "cenanycases ~ 0 + district + MIRsummarystat + te(lag, doymat, by=anom_var1, bs='tp') + te(lag, doymat, by=anom_var2, bs='tp') + s(doy, bs='tp')"
# myformula <- as.formula(myformula)
# 
# datelist <- unique(fullcasemat$weekstartdate)
# datelist <- datelist[datelist >= as.Date("2016-01-01", "%Y-%m-%d")]
# 
# outputdf <- data.frame()
# for (curdate in datelist) {
#   
#   curdate <- as.Date(curdate, origin="1970-01-01")
# 
#   fullcasemat$cenanycases <- fullcasemat$anycases
#   fullcasemat$cenanycases[fullcasemat$weekstartdate > curdate] <- NA
#   
#   cl <- makeCluster(detectCores(logical=FALSE)-1)
#   firstreg  <- bam(formula=myformula,
#                    family=binomial(), data=fullcasemat,
#                    subset=modeled==1,
#                    cluster=cl)
#   stopCluster(cl)
#   
#   fullcasemat$pred <- predict(firstreg, newdata=fullcasemat, type="response")
#   names(fullcasemat)
#   
#   tempdf <- fullcasemat[c("weekstartdate", "district", "anycases", "pred")]
#   tempdf$cendate <- curdate
#   outputdf <- bind_rows(outputdf, tempdf)
#   
# }
# write.csv(x=outputdf, file="outputdf.csv")
# save(outputdf, file="outputdf.RData")

load("outputdf.RData")
outputdf

minweek <- 23
maxweek <- 40

# summarize by state
outputdf <- group_by(outputdf, weekstartdate, cendate)
outputdf <- dplyr::summarise(outputdf,
                             anycases=sum(anycases, na.rm=TRUE),
                             pred    =sum(pred, na.rm=TRUE))
# get ready for graphing by setting up parameters
outputdf$weekdiff <- as.numeric((outputdf$weekstartdate - outputdf$cendate)/7)
mincendate <- min(outputdf$cendate, na.rm=TRUE)
obsdates <- unique(outputdf$weekstartdate, na.rm=TRUE)

startweek <- min(outputdf$weekstartdate)
changeweek <- min(outputdf$cendate)
endweek <- changeweek - 7 * 52

numweeks <- as.numeric((endweek - startweek)/(7*7))
botweeks <- seq(from = startweek,
                to = endweek,
                length.out=numweeks)

# don't start in the beginning
obsdates <- obsdates[obsdates >= endweek]

counter <- 1
for (thisobsdate in obsdates) {
  
  if (thisobsdate <= mincendate) {
    
    tempdf1 <- outputdf[outputdf$weekstartdate <= thisobsdate,]
    tempdf1 <- group_by(tempdf1, weekstartdate)
    tempdf2 <- dplyr::summarize(tempdf1,
                                meanobs=mean(anycases, na.rm=TRUE),
                                meanest=mean(pred, na.rm=TRUE))
    
    
    tempdf2$weeknum <- as.numeric(format(tempdf2$weekstartdate, "%U"))
    tempdf2$meanest[(tempdf2$weeknum <= minweek)|(tempdf2$weeknum >= maxweek)] <- 0
    tempdf2 <- tempdf2[tempdf2$weekstartdate >= botweeks[counter],]
    
    
    thisplot <- ggplot(tempdf2) + geom_line(aes(x=weekstartdate, y=meanobs), color="black") +
      geom_line(aes(x=weekstartdate, y=meanest), color="red") +
      ggtitle(paste("Retrospective estimates on ", as.Date(thisobsdate, origin="1970-01-01"), sep="")) +
      xlab("") + ylab("WNV-positive counties") + scale_x_date(date_breaks="1 year", date_labels="%Y")
    
    ggsave(thisplot, filename=paste(".\\predictions\\",
                                    str_pad(counter, 4, pad="0"),
                                    ".png", sep=""),
           height=4,
           width=12)
  
    minslideweek <- min(tempdf2$weekstartdate)
    
  }
  
  if (thisobsdate > mincendate) {
    
    tempdf1 <- outputdf[outputdf$weekstartdate <= thisobsdate,]
    tempdf1 <- group_by(tempdf1, weekstartdate)
    tempdf1 <- dplyr:: summarize(tempdf1,
                                 meanobs=mean(anycases,na.rm=TRUE),
                                 meanest=mean(pred, na.rm=TRUE))
    tempdf1$weeknum <- as.numeric(format(tempdf1$weekstartdate, "%U"))
    
    tempdf2 <- outputdf[outputdf$weekstartdate >= thisobsdate,]
    tempdf2 <- tempdf2[tempdf2$weekstartdate <= (thisobsdate+7*8),]
    tempdf2 <- group_by(tempdf2, weekstartdate)
    tempdf2 <- dplyr::summarize(tempdf2,
                                 meanest=mean(pred, na.rm=TRUE))
    tempdf2$weeknum <- as.numeric(format(tempdf2$weekstartdate, "%U"))

    tempdf1 <- tempdf1[tempdf1$weekstartdate >= minslideweek,]
    tempdf2 <- tempdf1[tempdf2$weekstartdate >= minslideweek,]
    
    tempdf3 <- tempdf2
    tempdf3 <- tempdf3[tempdf3$weekstartdate <= changeweek,]
    tempdf3$weeknumb <- as.numeric(format(tempdf3$weekstartdate, "%U"))
        
    tempdf1$meanest[(tempdf1$weeknum <= minweek)|(tempdf1$weeknum >= maxweek)] <- 0
    tempdf2$meanest[(tempdf2$weeknum <= minweek)|(tempdf2$weeknum >= maxweek)] <- 0
    tempdf3$meanest[(tempdf3$weeknum <= minweek)|(tempdf3$weeknum >= maxweek)] <- 0
    
    thisplot <- ggplot() + geom_line(data=tempdf1, aes(x=weekstartdate, y=meanobs), color="black") +
      geom_line(data=tempdf2, aes(x=weekstartdate, y=meanest), color="red", linetype=2) +
      geom_line(data=tempdf3, aes(x=weekstartdate, y=meanest), color="red", linetype=1) +
      ggtitle(paste("One-week-ahead predictions on ", as.Date(thisobsdate, origin="1970-01-01"), sep="")) +
      xlab("") + ylab("WNV-positive counties") + scale_x_date(date_breaks="1 year", date_labels="%Y")
    
    ggsave(thisplot, filename=paste(".\\predictions\\",
                                    str_pad(counter, 4, pad="0"),
                                    ".png", sep=""),
           height=4,
           width=12)
    
    minslideweek <- minslideweek + 7
    
  }
  
  
  counter <- counter + 1
  
}