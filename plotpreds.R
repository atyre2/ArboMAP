rm(list=ls())

library(dplyr)
library(ggplot2)
library(stringr)

packages <- c("reshape2", "ggplot2", "gridExtra",
              "lme4","pracma","dplyr","maptools",
              "raster","spdep","mgcv","sp","rgdal",
              "GISTools","data.table","splines","maps",
              "broom","mapproj", "Hmisc", "parallel",
              "rccmisc", "pROC", "ResourceSelection",
              "knitr")
for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package, repos = "http://cran.us.r-project.org")
    library(package, character.only=T)
  }
}



simplifynames <- function(priornames=NULL) {
  
  # convert to lower case
  priornames <- tolower(priornames)
  
  # remove spaces
  priornames <- gsub(pattern=" ", replacement="", x=priornames, fixed=TRUE)
  
  # remove district and parish
  priornames <- gsub(pattern="county", replacement="", x=priornames, fixed=TRUE)
  priornames <- gsub(pattern="parish", replacement="", x=priornames, fixed=TRUE)
  
  # return names
  return(priornames)
  
}


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

minweek <- 23
maxweek <- 40

# summarize for the entire state
outputdf <- group_by(outputdf, weekstartdate, cendate)
outputdf <- dplyr::summarise(outputdf,
                             obs=sum(anycases, na.rm=TRUE),
                             est=sum(pred, na.rm=TRUE))

head(outputdf)

# get ready for graphing by setting up parameters
outputdf$weekdiff <- as.numeric((outputdf$weekstartdate - outputdf$cendate)/7)
preobsdates <- unique(outputdf$weekstartdate)
preobsdates <- preobsdates[preobsdates <= min(outputdf$cendate, na.rm=TRUE)]
#preobsdates <- preobsdates[preobsdates >= as.Date("2012-01-01", "%Y-%m-%d")]
obsdates <- unique(outputdf$cendate, na.rm=TRUE)
weeksahead <- 4
slowdown <- 4

# cull those too far out
outputdf <- outputdf[outputdf$weekdiff <= weeksahead,]

# get rid of the bad predictions
outputdf$weeknum <- as.numeric(format(outputdf$weekstartdate, "%U"))
outputdf$est[outputdf$weeknum < minweek] <- 0
outputdf$est[outputdf$weeknum > maxweek] <- 0

outputdf$year <- as.numeric(format(outputdf$weekstartdate, "%Y"))

counter <- 1
for (thispredate in preobsdates) {
  
  thispredate <- as.Date(thispredate, origin="1970-01-01")
  
  tempdf1 <- group_by(outputdf, weekstartdate)
  tempdf1 <- dplyr::summarise(tempdf1,
                              obs=mean(obs, na.rm=TRUE),
                              est=mean(est, na.rm=TRUE))
  tempdf1$cenest <- tempdf1$est
  tempdf1$cenest[tempdf1$weekstartdate > thispredate] <- NA
  
  thisplot1 <- ggplot() + geom_line(data=tempdf1, aes(x=weekstartdate, y=obs), color="black") +
    geom_line(data=tempdf1, aes(x=weekstartdate, y=cenest), color="red") +
    ggtitle(paste("Model fitting for ", as.Date(thispredate, origin="1970-01-01"), sep="")) +
    geom_vline(xintercept = as.Date("2016-01-01"), linetype=2) +
    scale_alpha_continuous(guide=FALSE) + 
    xlab("") + ylab("WNV-positive counties") + scale_x_date(date_breaks="1 year", date_labels="%Y")

  tempdf2 <- outputdf[outputdf$year == as.numeric(format(thispredate, "%Y")),]
  tempdf2 <- group_by(tempdf2, weekstartdate)
  tempdf2 <- dplyr::summarise(tempdf2,
                              obs=mean(obs, na.rm=TRUE),
                              est=mean(est, na.rm=TRUE))
  
  tempdf2$cenest <- tempdf2$est
  tempdf2$cenest[tempdf2$weekstartdate > thispredate] <- NA
  
  tempdf2$weekstartdate <- as.Date(tempdf2$weekstartdate, origin="1970-01-01")

  thisplot2 <- ggplot() + geom_line(data=tempdf2, aes(x=weekstartdate, y=obs), color="black") +
    geom_line(data=tempdf2, aes(x=weekstartdate, y=cenest), color="red") +
    xlab("") + ylab("WNV-positive counties") + scale_x_date(date_breaks="1 month", date_labels="%b")
  
  thisplot3 <- arrangeGrob(thisplot1, thisplot2, ncol=1)

  ggsave(thisplot3, filename=paste(".\\predictions\\",
                                     str_pad(counter, 4, pad="0"),
                                     ".png", sep=""), height=8, width=12)
  
  counter <- counter + 1
  
}
for (thisobsdate in obsdates) {
  
  thisobsdate <- as.Date(thisobsdate, origin="1970-01-01")
  
  tempdf1 <- group_by(outputdf, weekstartdate)
  tempdf1 <- dplyr::summarise(tempdf1,
                              obs=mean(obs, na.rm=TRUE),
                              est=mean(est, na.rm=TRUE))
  tempdf1$cenest <- tempdf1$est
  tempdf1$weeksaheadofnow <- (tempdf1$weekstartdate - thisobsdate) / 7
  tempdf1$cenest[tempdf1$weeksaheadofnow > weeksahead] <- NA
  
  thisplot1 <- ggplot() + geom_line(data=tempdf1, aes(x=weekstartdate, y=obs), color="black") +
    geom_line(data=tempdf1, aes(x=weekstartdate, y=cenest), color="red") +
    ggtitle(paste("Four-week-ahead predictions for ", as.Date(thisobsdate, origin="1970-01-01"), sep="")) +
    geom_vline(xintercept = as.Date("2016-01-01"), linetype=2) +
    scale_alpha_continuous(guide=FALSE) + 
    xlab("") + ylab("WNV-positive counties") + scale_x_date(date_breaks="1 year", date_labels="%Y")

  outputdf$year <- as.numeric(format(as.Date(outputdf$weekstartdate, origin="1970-01-01"), "%Y"))
  tempdf1 <- outputdf[outputdf$year == as.numeric(format(thisobsdate, "%Y")),]
  tempdf1 <- group_by(tempdf1, weekstartdate)
  tempdf1 <- dplyr::summarise(tempdf1,
                              obs=mean(obs, na.rm=TRUE))
  
  tempdf2 <- outputdf[outputdf$year == as.numeric(format(thisobsdate, "%Y")),]
  tempdf2 <- tempdf2[tempdf2$weekstartdate <= (thisobsdate + 7*weeksahead),]
  #tempdf2 <- tempdf2[tempdf2$cendate >= (thisobsdate - 7*weeksahead),]
  
  # alphalist <- data.frame(myalpha=seq(from=1, to=0.05, length.out=length(unique(tempdf2$cendate))),
  #                         cendate=unique(tempdf2$cendate))
  # alphalist$myalpha <- alphalist$myalpha^4
  # tempdf2 <- left_join(tempdf2, alphalist, by="cendate")
  # tempdf2$cendate <- rank(tempdf2$cendate)
  
  tempdf3 <- tempdf2[tempdf2$cendate == max(tempdf2$cendate, na.rm=TRUE),]
  
  thisplot2 <- ggplot() + geom_line(data=tempdf2, aes(x=weekstartdate, y=est, group=cendate), color="grey") +
    geom_line(data=tempdf1, aes(x=weekstartdate, y=obs), color="black") +
    geom_line(data=tempdf3, aes(x=weekstartdate, y=obs), color="red") + 
    scale_color_manual(values=c("grey", "red"), guide=FALSE) +
    xlab("") + ylab("WNV-positive counties") + scale_x_date(date_breaks="1 month", date_labels="%b")
  
  thisplot3 <- arrangeGrob(thisplot1, thisplot2, ncol=1)
  
  ggsave(thisplot3, filename=paste(".\\predictions\\",
                                    str_pad(counter, 4, pad="0"),
                                    ".png", sep=""),
           height=8,
           width=12)
  
  counter <- counter + 1
    
}
