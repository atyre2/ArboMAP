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

load("outputdf.RData")

head(outputdf)
outputdf <- outputdf[outputdf$cendate == max(outputdf$cendate, na.rm=TRUE),]
outputdf$weeknum <- as.numeric(format(outputdf$weekstartdate, "%U"))
outputdf$year    <- as.numeric(format(outputdf$weekstartdate, "%Y"))
outputdf$pred[(outputdf$weeknum <= minweek)|(outputdf$weeknum >= maxweek)] <- 0

outputdf <- group_by(outputdf, district, year)
mysums <- dplyr::summarise(outputdf,
                           obs=sum(anycases, na.rm=TRUE),
                           est=sum(pred, na.rm=TRUE))

mysums <- group_by(mysums, district)
myr2   <- dplyr::summarise(mysums,
                           r2 = cor(obs, est, method="spearman"))
myr2




districtshapefile <- ".\\shapefile\\cb_2014_us_county_5m - in EPSG 5070 - only SD.shp"

# load the shapefile immediately so that we can get rid of any districts which are not found here
district_shapes <- readShapePoly(districtshapefile)
# simplify name
district_shapes$district <- simplifynames(district_shapes$NAME)
diagnostic_shapefiledistricts <- unique(district_shapes$district)


outputdf <- group_by(outputdf, district)
totsums <- dplyr::summarise(outputdf,
                            obs = sum(anycases, na.rm=TRUE),
                            est = sum(pred, na.rm=TRUE))






crs(district_shapes) <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80     +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
projected_districts <- spTransform(district_shapes, crs("+proj=longlat +datum=WGS84 +no_defs"))
projected_districts@data$id = rownames(projected_districts@data)
projected_districts.df <- tidy(projected_districts)
projected_districts.df <- left_join(projected_districts.df, projected_districts@data, by="id")
head(projected_districts.df)

projected_districts.df <- left_join(projected_districts.df, myr2, by="district")
projected_districts.df <- left_join(projected_districts.df, totsums, by="district")

thisplot <- ggplot(projected_districts.df) +
  aes(long,lat,fill=r2,group=group,id=id,guides=FALSE) +
  geom_polygon() + xlab("") + ylab("") +
  geom_path(color="black") +
  theme(legend.position="bottom") +
  coord_map() + ggtitle("R2 for predictions vs. observations by year") +
  theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
        axis.ticks = element_blank(), axis.text.x = element_blank(),
        axis.text.y = element_blank(), axis.title.x=element_blank(),
        axis.title.y = element_blank(),
        legend.position="right",
        legend.key.width=unit(1, "cm"),
        legend.key.height=unit(0.5,"cm")) +
  scale_fill_gradient(low="white", high="blue", name="R2, cases\nper year") +
  theme(legend.position = "bottom")

thisplot2 <- ggplot(projected_districts.df) +
  aes(long,lat,fill=log(obs+1),group=group,id=id,guides=FALSE) +
  geom_polygon() + xlab("") + ylab("") +
  geom_path(color="black") +
  theme(legend.position="bottom") +
  coord_map() + ggtitle("Cases by county") +
  theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
        axis.ticks = element_blank(), axis.text.x = element_blank(),
        axis.text.y = element_blank(), axis.title.x=element_blank(),
        axis.title.y = element_blank(),
        legend.position="right",
        legend.key.width=unit(1, "cm"),
        legend.key.height=unit(0.5,"cm")) +
  scale_fill_gradient(low="white", high="blue", name="log(total cases)") +
  theme(legend.position = "bottom")

ggsave(thisplot, filename="thisplot.png",
       height=8, width=8)
ggsave(thisplot2, filename="thisplot2.png",
       height=8, width=8)

projected_districts.df$r2[projected_districts.df$district == "mellette"] <- NA

thisplot3 <- ggplot(projected_districts.df) + geom_point(aes(x=log(obs+1), y=r2)) +
  ggtitle("Relationship between predictability\nand total case load per county") +
  theme(text=element_text(size=20)) + xlab("log(cases)") + ylab("R2")
ggsave(thisplot3, filename="thisplot3.png",
       height=8, width=8)

